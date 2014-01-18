module Records where

import Prelude ()
import BasicPrelude

import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Network.URI (URI)
import Text.Email.Validate (EmailAddress, validate)
import Data.Base58Address (RippleAddress)

import Data.Text.Buildable
import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromRow (FromRow(..), field, fieldWith)
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.ToField (ToField(..), toField)
import Database.SQLite.Simple.FromField (fieldData, ResultError(ConversionFailed))
import Database.SQLite.Simple.Ok (Ok(Ok, Errors))
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)

serviceLimit :: Int
serviceLimit = 100

serviceFee :: Int
serviceFee = 2

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

instance Buildable URI where
	build = build . show

instance ToRow Deposit where
	toRow (Deposit rid fn email tel ripple amnt complete) =
		[toField rid, toField fn, toField (show email), toField tel, toField (show ripple), toField amnt, toField complete]

instance FromRow Deposit where
	fromRow = Deposit <$> field <*> field <*> fieldWith emailF <*> field <*> fieldWith rippleF <*> field <*> field
		where
		emailF f = case fieldData f of
			(SQLText t) -> case validate (encodeUtf8 t) of
				Left e -> Errors [toException $ ConversionFailed "TEXT" "EmailAddress" e]
				Right email -> Ok email
			_ -> Errors [toException $ ConversionFailed "TEXT" "EmailAddress" "need a text"]

		rippleF f = case fieldData f of
			(SQLText t) -> case readMay t of
				Nothing -> Errors [toException $ ConversionFailed "TEXT" "RippleAddress" "invalid"]
				Just ripple -> Ok ripple
			_ -> Errors [toException $ ConversionFailed "TEXT" "RippleAddress" "need a text"]

instance ToRow Quote where
	toRow (Quote qid typ amnt dest email q a msg complete) =
		[toField qid, toField typ, toField amnt, toField (show dest), toField (show email), toField q, toField a, toField msg, toField complete]

instance (CanVerify a) => ToRow (Verification a) where
	toRow (Verification item typ) = [
			toField itemId,
			toField itemTable,
			toField typ
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

data Home = Home {
		renderedDepositForm :: [Form],
		renderedQuoteForm   :: [Form]
	}

data Form = Form {
		formHtml   :: Html,
		formAction :: URI
	}

data DepositSuccess = DepositSuccess {
		successfulDeposit :: [Deposit],
		homeLink :: URI
	}

data Deposit = Deposit {
		depositId       :: Int64,
		depositorFN     :: Text,
		depositorEmail  :: EmailAddress,
		depositorTel    :: Text,
		depositorRipple :: RippleAddress,
		depositAmount   :: Double,
		depositComplete :: Bool
	}

data VerificationType = AutomatedPhoneVerification | ManualPhoneVerification
	deriving (Show, Read, Enum)

data Verification a = Verification {
		verificationItem :: a,
		verificationType :: VerificationType
	}

data PlivoDeposit = PlivoDeposit {
		plivoCode :: String
	}

data QuoteType = InteracETransferQuote
	deriving (Show, Read, Enum)

data Quote = Quote {
		quoteId          :: Word32, -- Because destination tag
		quoteType        :: QuoteType,
		quoteAmount      :: Double,
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
