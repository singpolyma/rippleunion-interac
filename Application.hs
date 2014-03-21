{-# LANGUAGE CPP #-}
module Application (home, processDeposit, plivoDeposit, verifyDeposit, stripeVerifyDeposit, processQuote) where

import Prelude ()
import BasicPrelude
import Data.Fixed (Centi)
import Data.Char (isDigit, isAlphaNum)
import System.Random (randomRIO)
import Control.Error (eitherT, EitherT(..), fmapLT, throwT)
import Data.Digest.Pure.MD5 (md5, MD5Digest)
import Database.SQLite3 (SQLError(..), Error(ErrorConstraint))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LZ

import Network.Wai (Application)
import Network.HTTP.Types (ok200, badRequest400)
import Network.Wai.Util (stringHeaders, textBuilder, string, queryLookup, noStoreFileUploads)
import Network.Wai.Parse (parseRequestBody)

import Network.Wai.Digestive (bodyFormEnv_)
import SimpleForm.Combined (input_html, label, Label(..), wdef, vdef, ShowRead(..), unShowRead)
import SimpleForm.Render.XHTML5 (render)
import SimpleForm.Digestive.Combined (SimpleForm', input, input_, getSimpleForm, postSimpleForm, fieldset)
import SimpleForm (tel, password, textarea, hidden)
import qualified SimpleForm.Validation as SFV
import Text.Digestive.Form (monadic)
import Text.Blaze (preEscapedToMarkup)

import Plivo (callAPI, createOutboundCall)

import Network.URI (URI(..))
import Network.URI.Partial (relativeTo)

import Database.SQLite.Simple (query, execute, Connection, Query)
import Database.SQLite.Simple.ToRow (ToRow)

import Stripe
import Records
import MustacheTemplates
#include "PathHelpers.hs"

type Action a = URI -> Connection -> PlivoConfig -> StripeConfig -> a

s :: (IsString s) => String -> s
s = fromString

htmlEscape :: String -> String
htmlEscape = concatMap escChar
	where
	escChar '&' = "&amp;"
	escChar '"' = "&quot;"
	escChar '<' = "&lt;"
	escChar '>' = "&gt;"
	escChar c   = [c]

lbl :: String -> Maybe Label
lbl = Just . Label . s

digits10 :: SFV.Validation Text
digits10 = SFV.pmap (go . T.filter isDigit) vdef
	where
	go t
		| T.length t == 10 = Just (T.cons '1' t)
		| T.length t == 11 && T.head t == '1' = Just t
		| otherwise = Nothing

getServiceLimit :: (MonadIO m) => Text -> m Centi
getServiceLimit _ = return 100

amountLimit :: Centi -> SFV.Validation Centi
amountLimit serviceLimit = SFV.pmap go vdef
	where
	go amnt
		| amnt <= serviceLimit = Just amnt
		| otherwise = Nothing

depositForm :: (Functor m, MonadIO m) => Centi -> SimpleForm' m Deposit
depositForm lim = do
	fn'     <- input (s"fn") (Just . depositorFN) (wdef,vdef)
		(mempty { label = lbl"Full name"})
	email'  <- input_ (s"email") (Just . depositorEmail)
	tel'    <- input  (s"tel") (Just . depositorTel) (tel,digits10)
		(mempty {label = lbl"Telephone number"})
	ripple' <- input  (s"ripple") (Just . ShowRead . depositorRipple) (wdef,vdef)
		(mempty {label = lbl"Ripple address"})
	amount' <- input (s"amount") (Just . depositAmount) (wdef,amountLimit lim)
		(mempty {label = lbl"Amount in CAD"})

	let rid = monadic $ fmap pure $ liftIO $ randomRIO (100000,999999)
	return $ Deposit <$> rid <*> fn' <*> email' <*> tel' <*> fmap unShowRead ripple' <*> amount' <*> pure False

quoteForm :: (Functor m, MonadIO m) => SimpleForm' m Quote
quoteForm = do
	destination' <- input (s"destination") (Just . quoteDestination) (wdef,vdef)
		(mempty {label = lbl"Destination Email"})
	email'       <- input (s"email") (Just . quotorEmail) (wdef,vdef)
		(mempty {label = lbl"Your Email"})
	amount'      <- input (s"amount") (Just . quoteAmount) (wdef,amountLimit quoteLimit)
		(mempty {label = lbl"Amount in CAD"})
	question'    <- input_ (s"question") (Just . quoteQuestion)
	answer'      <- input (s"answer") (Just . quoteAnswer) (password,vdef)
		(mempty {label = lbl"Secret Answer"})
	message'     <- input (s"message") (Just . quoteMessage) (textarea,vdef)
		mempty

	let rid = monadic $ fmap pure $ liftIO $ randomRIO (200000000,maxBound)
	return $ Quote <$> rid <*> pure InteracETransferQuote <*> amount' <*> destination' <*> email' <*> question' <*> answer' <*> message' <*> pure False

plivoDepositForm :: (Monad m) => SimpleForm' m PlivoDeposit
plivoDepositForm = do
	code' <- input (s"code") (Just . plivoCode) (wdef,vdef) (mempty { label = lbl"Verification code"})

	return $ PlivoDeposit <$> code'

-- TODO: verify postal code better
-- TODO: verify ranges on month and year

stripeVerifyForm :: (Monad m) => SimpleForm' m (Int64, Maybe RequestCard)
stripeVerifyForm = do
	depositId <- input (s"deposit_id") (Just . fst)
		(hidden . fmap show, SFV.read) mempty
	ccnum <- input (s"card_number") (fmap rCardNumber . snd) (wdef,vdef)
		(mempty{label = Nothing, input_html = [(s"placeholder", s"Card Number")]})

	exp <- fieldset (s"card_expiry") id $ do
		month <- input (s"month") (fmap rCardExpMonth . snd) (wdef,vdef)
			(mempty{label = Nothing, input_html = [(s"placeholder", s"MM")]})
		year <- input (s"year") (fmap rCardExpYear . snd) (wdef,vdef)
			(mempty{label = Nothing, input_html = [(s"placeholder", s"YYYY")]})

		return $ (,) <$> month <*> year

	cvc <- input (s"card_cvc") ((rCardCVC=<<) . snd) (wdef,vdef)
		(mempty{label = Nothing, input_html = [(s"placeholder", s"CVC")]})
	addr1 <- input (s"card_addr1") ((rCardAddrLineOne=<<) . snd) (wdef,vdef)
		(mempty{label = Nothing, input_html = [(s"placeholder", s"Street Address")]})
	postal <- input (s"card_postal_code") ((rCardAddrZip=<<) . snd) (wdef,vdef)
		(mempty{label = Nothing, input_html = [(s"placeholder", s"Postal Code")]})

	return $ (,) <$> depositId <*> (Just <$> (
			uncurry <$> (RequestCard <$> ccnum) <*> exp
			<*> fmap Just cvc <*> pure Nothing <*>
			fmap Just addr1 <*> pure Nothing <*> pure Nothing <*>
			fmap Just postal <*> pure Nothing <*> pure Nothing
		))

home :: Action Application
home root _ _ _ _ = do
	-- TODO: get current limit from tel in cookie
	dForm <- getSimpleForm render Nothing (depositForm 100)
	qForm <- getSimpleForm render Nothing quoteForm
	textBuilder ok200 headers $
		viewHome htmlEscape (Home [Form dForm dPath] [Form qForm qPath] 0 False)
	where
	dPath = processDepositPath `relativeTo` root
	qPath = processQuotePath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf-8")]

processDeposit :: Action Application
processDeposit root db plivo _ req = do
	tel <- fmap (parseTel <=< queryLookup "tel" . fst)
		(parseRequestBody noStoreFileUploads req)
	pastAmount <- liftIO $ query db
		(s"SELECT TOTAL(amount) FROM deposits WHERE complete=1 AND tel=?") [tel]
	let limit = case pastAmount of
		[[pastAmountSum]] -> baseDepositLimit +
			(fromIntegral $ floor (pastAmountSum / 10::Double))
		_ -> baseDepositLimit

	(rForm, dep) <- postSimpleForm render (bodyFormEnv_ req) (depositForm limit)
	liftIO $ case dep of
		Just x -> do
			finalId <- depositId <$>
				insertSucc db (s"INSERT INTO deposits VALUES (?,?,?,?,?,?,?)")
				(\d -> d {depositId = succ $ depositId d}) x
			apiResult <- callAPI (plivoAuthId plivo) (plivoAuthToken plivo) $
				createOutboundCall (plivoTel plivo) (textToString $ depositorTel x)
					(plivoDepositPath finalId `relativeTo` root)

			case apiResult of
				Right _ -> do
					vForm <- getSimpleForm render Nothing plivoDepositForm
					textBuilder ok200 headers $ viewDepositVerify htmlEscape
						(Home [Form vForm vPath] [] limit True)
				Left _ ->
					textBuilder badRequest400 headers $ viewHome htmlEscape
						(Home [Form (preEscapedToMarkup "<p class='error'>Something went wrong trying to verify your telephone number: did you enter it correctly?</p>" ++ rForm) fPath] [] limit True)
		Nothing -> textBuilder badRequest400 headers $
			viewHome htmlEscape (Home [Form rForm fPath] [] limit True)
	where
	parseTel s = let SFV.Check d10 = digits10 in d10 [s]
	vPath = verifyDepositPath `relativeTo` root
	fPath = processDepositPath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf-8")]

verifyDeposit :: Action Application
verifyDeposit root db _ _ req = do
	(vForm, dep) <- postSimpleForm render (bodyFormEnv_ req) plivoDepositForm
	liftIO $ do
		deps <- maybe (return [])
			(query db (s"SELECT id,fn,email,tel,ripple,amount,complete FROM deposits WHERE id=? AND complete=0"))
				(fmap ((:[]) . plivoCode) dep)
		case deps of
			(x:_) -> do
				execute db (s"INSERT INTO verifications (item_id,item_table,verification_type,notes,addr_token) VALUES (?,?,?,?,?)")
					(Verification x AutomatedPhoneVerification Nothing Nothing)

				-- Check if there's already a higher verification associated
				-- with this phone number
				v <- fmap (head.head) $ query db (s $ concat [
						"SELECT ",
							"count(1) ",
						"FROM ",
							"deposits LEFT JOIN verifications ",
								"ON verifications.item_table='deposits' ",
								"AND verifications.item_id=deposits.id ",
						"WHERE ",
						"deposits.tel=? AND verifications.verification_type IN (?,?)"
					])
					(depositorTel x, ManualPhoneVerification, StripeVerification)

				stripeF <- getSimpleForm render (Just (depositId x, Nothing)) stripeVerifyForm
				textBuilder ok200 headers $ viewDepositSuccess htmlEscape
					(DepositSuccess [x] (v < (1::Int)) [Form stripeF svPath] hPath)
			[] ->
				textBuilder badRequest400 headers $ viewDepositVerify htmlEscape
					(Home [Form (preEscapedToMarkup "<p class='error'>Invalid code</p>" ++ vForm) vPath] [] 0 False)
	where
	svPath = stripeVerifyDepositPath `relativeTo` root
	vPath = verifyDepositPath `relativeTo` root
	hPath = homePath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf-8")]

stripeVerifyDeposit :: Action Application
stripeVerifyDeposit root db _ stripe req = do
	(stripeF, r) <- postSimpleForm render (bodyFormEnv_ req) stripeVerifyForm
	deps <- liftIO $ query db (s"SELECT id,fn,email,tel,ripple,amount,complete FROM deposits WHERE id=? LIMIT 1") [maybe 0 fst r]
	case (deps, r) of
		((dep:_), Just (_, Just card)) -> eitherT (\e ->
			textBuilder badRequest400 headers $ viewDepositSuccess htmlEscape $
				DepositSuccess [dep] True
				[Form (preEscapedToMarkup ("<p class='error'>" ++ e ++ "</p>") ++ stripeF) svPath]
				hPath
			) return $ do
				ex <- liftIO $ fmap (head.head) $ query db
					(s"SELECT count(1) FROM verifications WHERE addr_token=?")
					[show $ tokenizeAddr (rCardAddrLineOne card) (rCardAddrZip card)]
				when (ex > (0::Int)) $
					throwT "That address has already been used in a verification."

				cust <- fmapLT (const "Verification failed.  Is it a Canadian card?") $
					EitherT $ verifyCard stripe
					(Just $ T.unpack $ depositorFN dep ++ (s" -- ") ++ depositorTel dep)
					(Just $ T.unpack $ show $ depositorEmail dep)
					card
				liftIO $ execute db (s"INSERT INTO verifications (item_id,item_table,verification_type,notes,addr_token) VALUES (?,?,?,?,?)") $
					Verification dep StripeVerification (Just cust) (Just $ show $
						tokenizeAddr (rCardAddrLineOne card) (rCardAddrZip card))

				textBuilder ok200 headers $ viewDepositSuccess htmlEscape
					(DepositSuccess [dep] False [] hPath)
		((dep:_), _) ->
			textBuilder badRequest400 headers $ viewDepositSuccess htmlEscape
				(DepositSuccess [dep] True [Form stripeF svPath] hPath)
		_ -> string badRequest400 [] "Deposit code not found in database, please email help@rippleunion.com"
	where
	svPath = stripeVerifyDepositPath `relativeTo` root
	hPath = homePath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf-8")]

plivoDeposit :: Action (Int64 -> Application)
plivoDeposit _ _ _ _ rid _ =
	textBuilder ok200 headers $ viewPlivoDeposit htmlEscape (PlivoDeposit code)
	where
	code = intersperse ' ' $ shows rid ""
	Just headers = stringHeaders [("Content-Type", "application/xml; charset=utf-8")]

processQuote :: Action Application
processQuote root db _ _ req = do
	(qForm, quote) <- postSimpleForm render (bodyFormEnv_ req) quoteForm
	liftIO $ case quote of
		Just q -> do
			finalQuote <- insertSucc db (s"INSERT INTO quotes VALUES (?,?,?,?,?,?,?,?,?)")
				(\q -> q {quoteId = succ $ quoteId q}) q

			textBuilder ok200 headers $ viewQuoteSuccess htmlEscape
				(QuoteSuccess [finalQuote {quoteAmount = quoteAmount finalQuote + fromIntegral serviceFee}] hPath)
		Nothing -> textBuilder ok200 headers $
			viewHome htmlEscape (Home [] [Form qForm qPath] 0 False)
	where
	qPath = processQuotePath `relativeTo` root
	hPath = homePath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf-8")]

-- | Increments id until success
insertSucc :: (ToRow a) => Connection -> Query -> (a -> a) -> a -> IO a
insertSucc db q succ x = do
	r <- try $ execute db q x
	case r of
		Left (SQLError ErrorConstraint _ _) ->
			insertSucc db q succ (succ x)
		Left e -> throwIO e
		Right () -> return x

tokenizeAddr :: Maybe Text -> Maybe Text -> MD5Digest
tokenizeAddr street postal =
	md5 $ LZ.fromChunks $ map encodeUtf8 [street', postal']
	where
	street' = maybe T.empty (T.takeWhile isDigit . T.dropWhile (not . isDigit))
		street
	postal' = maybe T.empty (T.filter isAlphaNum) postal
