module Records where

import Prelude ()
import BasicPrelude
import Data.Fixed (Centi)
import Control.Error (readMay)
import Data.Text.Encoding (encodeUtf8)

import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Network.URI (URI)
import Text.Email.Validate (EmailAddress, validate)
import Data.Base58Address (RippleAddress)

import Data.Text.Buildable
import Database.SQLite.Simple (SQLData(SQLText,SQLInteger,SQLFloat))
import Database.SQLite.Simple.FromRow (FromRow(..), field, fieldWith)
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.ToField (ToField(..), toField)
import Database.SQLite.Simple.FromField (fieldData, ResultError(ConversionFailed))
import Database.SQLite.Simple.Ok (Ok(Ok, Errors))
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)

s :: (IsString s) => String -> s
s = fromString

serviceFee :: Int
serviceFee = 2

toDbl :: (Real a) => a -> Double
toDbl = realToFrac

baseDepositLimit :: Centi
baseDepositLimit = 150

depositMaxLimit :: Centi
depositMaxLimit = 900

quoteLimit :: Centi
quoteLimit = 100

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

instance Buildable URI where
	build = build . show

instance ToRow Deposit where
	toRow (Deposit rid fn email tel ripple amnt complete) =
		[toField rid, toField fn, toField (show email), toField tel, toField (show ripple), toField (toDbl amnt), toField complete]

instance FromRow Deposit where
	fromRow = Deposit <$> field <*> field <*> fieldWith emailF <*> field <*> fieldWith rippleF <*> fieldWith dbl <*> field
		where
		emailF f = case fieldData f of
			(SQLText t) -> case validate (encodeUtf8 t) of
				Left e -> Errors [toException $ ConversionFailed "TEXT" "EmailAddress" e]
				Right email -> Ok email
			_ -> Errors [toException $ ConversionFailed "TEXT" "EmailAddress" "need a text"]

		rippleF f = case fieldData f of
			(SQLText t) -> case readMay (textToString t) of
				Nothing -> Errors [toException $ ConversionFailed "TEXT" "RippleAddress" "invalid"]
				Just ripple -> Ok ripple
			_ -> Errors [toException $ ConversionFailed "TEXT" "RippleAddress" "need a text"]

		dbl f = case fieldData f of
			SQLInteger i -> Ok $ fromIntegral i
			SQLFloat d -> Ok $ realToFrac d
			_ -> Errors []

instance ToRow Quote where
	toRow (Quote qid typ amnt dest email q a msg complete) =
		[toField qid, toField typ, toField (toDbl amnt), toField (show dest), toField (show email), toField q, toField a, toField msg, toField complete]

instance (CanVerify a) => ToRow (Verification a) where
	toRow (Verification item typ notes token) = [
			toField itemId,
			toField itemTable,
			toField typ,
			toField notes,
			toField token
		]
		where
		(itemId, itemTable) = verifyItemData item

instance ToField VerificationType where
	toField = toField . show

instance ToField QuoteType where
	toField = toField . show

class CanVerify a where
	verifyItemData :: a -> (Int64, String)

instance CanVerify Deposit where
	verifyItemData d = (depositId d, "deposits")

instance Eq Form where
	(Form _ a1) == (Form _ a2) = a1 == a2

data Home = Home {
		renderedDepositForm :: [Form],
		renderedQuoteForm   :: [Form],
		depositLimit        :: Centi,
		foundDepositLimit   :: Bool
	}

data Form = Form {
		formHtml   :: Html,
		formAction :: URI
	}

data DepositSuccess = DepositSuccess {
		successfulDeposit :: [Deposit],
		higherVerificationNeeded :: Bool,
		renderedStripeVerifyForm :: [Form],
		homeLink :: URI
	}

data Deposit = Deposit {
		depositId       :: Int64,
		depositorFN     :: Text,
		depositorEmail  :: EmailAddress,
		depositorTel    :: Text,
		depositorRipple :: RippleAddress,
		depositAmount   :: Centi,
		depositComplete :: Bool
	}

data VerificationType = AutomatedPhoneVerification | ManualPhoneVerification | StripeVerification
	deriving (Show, Read, Enum)

data Verification a = Verification {
		verificationItem :: a,
		verificationType :: VerificationType,
		verificationNotes :: Maybe Text,
		verificationAddrToken :: Maybe Text
	}

data PlivoDeposit = PlivoDeposit {
		plivoCode :: String
	}

data QuoteType = InteracETransferQuote
	deriving (Show, Read, Enum)

data Quote = Quote {
		quoteId          :: Word32, -- Because destination tag
		quoteType        :: QuoteType,
		quoteAmount      :: Centi,
		quoteDestination :: EmailAddress,
		quotorEmail      :: EmailAddress,
		quoteQuestion    :: Text,
		quoteAnswer      :: Text,
		quoteMessage     :: Text,
		quoteComplete    :: Bool
	}

data QuoteSuccess = QuoteSuccess {
		successfulQuote :: [Quote],
		quoteHomeLink :: URI
	}

data PlivoConfig = PlivoConfig {
		plivoAuthId    :: String,
		plivoAuthToken :: String,
		plivoTel       :: String
	}

data Header = Header {
	}

instance Monoid Header where
	mempty = Header
	mappend _ _ = Header

instance Eq Header where
	_ == _ = False

header :: Header
header = Header
