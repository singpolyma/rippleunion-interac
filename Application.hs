{-# LANGUAGE CPP #-}
module Application (home, processDeposit, plivoDeposit, verifyDeposit) where

import Prelude ()
import BasicPrelude
import System.Random (randomRIO)
import Database.SQLite3 (SQLError(..), Error(ErrorConstraint))
import qualified Data.Text as T

import Network.Wai (Application)
import Network.HTTP.Types (ok200)
import Network.Wai.Util (stringHeaders, textBuilder)

import Network.Wai.Digestive (bodyFormEnv_)
import SimpleForm.Combined (label, Label(..), wdef, vdef, ShowRead(..), unShowRead)
import SimpleForm.Render.XHTML5 (render)
import SimpleForm.Digestive.Combined (SimpleForm', input, input_, getSimpleForm, postSimpleForm)
import SimpleForm (tel)
import qualified SimpleForm.Validation as SFV
import Text.Digestive.Form (monadic)
import Text.Blaze (preEscapedToMarkup)

import Plivo (callAPI, createOutboundCall)

import Network.URI (URI(..))
import Network.URI.Partial (relativeTo)

import Database.SQLite.Simple (query, execute, Connection)

import Records
import MustacheTemplates
#include "PathHelpers.hs"

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
digits10 = SFV.pmap go vdef
	where
	go t
		| T.length t == 10 = Just (T.cons '1' t)
		| otherwise = Nothing

amountLimit :: SFV.Validation Double
amountLimit = SFV.pmap go vdef
	where
	go amnt
		| amnt <= fromIntegral serviceLimit = Just amnt
		| otherwise = Nothing

depositForm :: (Functor m, MonadIO m) => SimpleForm' m Deposit
depositForm = do
	fn'     <- input (s"fn") (Just . depositorFN) (wdef,vdef) (mempty { label = lbl"Full name"})
	email'  <- input_ (s"email") (Just . depositorEmail)
	tel'    <- input  (s"tel") (Just . depositorTel) (tel,digits10) (mempty {label = lbl"Telephone number"})
	ripple' <- input  (s"ripple") (Just . ShowRead . depositorRipple) (wdef,vdef) (mempty {label = lbl"Ripple address"})
	amount' <- input (s"amount") (Just . depositAmount) (wdef,amountLimit) (mempty {label = lbl"Amount in CAD"})

	let rid = monadic $ fmap pure $ liftIO $ randomRIO (1,999999)
	return $ Deposit <$> rid <*> fn' <*> email' <*> tel' <*> fmap unShowRead ripple' <*> amount' <*> pure False

plivoDepositForm :: (Monad m) => SimpleForm' m PlivoDeposit
plivoDepositForm = do
	code' <- input (s"code") (Just . plivoCode) (wdef,vdef) (mempty { label = lbl"Verification code"})

	return $ PlivoDeposit <$> code'

home :: URI -> Connection -> PlivoConfig -> Application
home root _ _ _ = do
	rForm <- getSimpleForm render Nothing depositForm
	textBuilder ok200 headers $ viewHome htmlEscape (Home rForm fPath)
	where
	fPath = processDepositPath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]

processDeposit :: URI -> Connection -> PlivoConfig -> Application
processDeposit root db plivo req = do
	(rForm, dep) <- postSimpleForm render (bodyFormEnv_ req) depositForm
	liftIO $ case dep of
		Just x -> do
			finalId <- insertDeposit db x
			apiResult <- callAPI (plivoAuthId plivo) (plivoAuthToken plivo) $
				createOutboundCall (plivoTel plivo) (textToString $ depositorTel x)
					(plivoDepositPath finalId `relativeTo` root)

			case apiResult of
				Right _ -> do
					vForm <- getSimpleForm render Nothing plivoDepositForm
					textBuilder ok200 headers $ viewDepositVerify htmlEscape
						(Home vForm vPath)
				Left _ ->
					textBuilder ok200 headers $ viewHome htmlEscape
						(Home (preEscapedToMarkup "<p class='error'>Something went wrong trying to verify your telephone number: did you enter it correctly?</p>" ++ rForm) fPath)
		Nothing ->
			textBuilder ok200 headers $ viewHome htmlEscape (Home rForm fPath)
	where
	vPath = verifyDepositPath `relativeTo` root
	fPath = processDepositPath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]

verifyDeposit :: URI -> Connection -> PlivoConfig -> Application
verifyDeposit root db _ req = do
	(vForm, dep) <- postSimpleForm render (bodyFormEnv_ req) plivoDepositForm
	liftIO $ do
		deps <- maybe (return [])
			(query db (s"SELECT id,fn,email,tel,ripple,amount,complete FROM deposits WHERE id=? AND complete=0"))
				(fmap ((:[]) . plivoCode) dep)
		case deps of
			(x:_) -> do
				execute db (s"INSERT INTO verifications (item_id,item_table,verification_type) VALUES (?,?,?)")
					(Verification x AutomatedPhoneVerification)
				textBuilder ok200 headers $ viewDepositSuccess htmlEscape
					(DepositSuccess [x] hPath)
			[] ->
				textBuilder ok200 headers $ viewDepositVerify htmlEscape
					(Home (preEscapedToMarkup "<p class='error'>Invalid code</p>" ++ vForm) vPath)
	where
	vPath = verifyDepositPath `relativeTo` root
	hPath = homePath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]

plivoDeposit :: URI -> Connection -> PlivoConfig -> Int64 -> Application
plivoDeposit _ _ _ rid _ =
	textBuilder ok200 headers $ viewPlivoDeposit htmlEscape (PlivoDeposit code)
	where
	code = intersperse ' ' $ shows rid ""
	Just headers = stringHeaders [("Content-Type", "application/xml; charset=utf8")]

-- | Increments id until success
insertDeposit :: Connection -> Deposit -> IO Int64
insertDeposit db x = do
	r <- try $ execute db (s"INSERT INTO deposits VALUES (?,?,?,?,?,?,?)") x
	case r of
		Left (SQLError ErrorConstraint _ _) ->
			insertDeposit db (x {depositId = succ $ depositId x})
		Left e -> throwIO e
		Right () -> return (depositId x)
