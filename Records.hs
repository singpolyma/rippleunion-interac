module Records where

import Prelude ()
import BasicPrelude

import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Network.URI (URI)
import Text.Email.Validate (EmailAddress, validate)

import Data.Text.Buildable
import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromRow (FromRow(..), field, fieldWith)
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.ToField (ToField(..), toField)
import Database.SQLite.Simple.FromField (fieldData, ResultError(ConversionFailed))
import Database.SQLite.Simple.Ok (Ok(Ok, Errors))
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

instance Buildable URI where
	build = build . show

instance ToRow Deposit where
	toRow (Deposit rid fn email tel amnt complete) =
		[toField rid, toField fn, toField (show email), toField tel, toField amnt, toField complete]

instance FromRow Deposit where
	fromRow = Deposit <$> field <*> field <*> fieldWith emailF <*> field <*> field <*> field
		where
		emailF f = case fieldData f of
			(SQLText t) -> case validate (encodeUtf8 t) of
				Left e -> Errors [toException $ ConversionFailed "TEXT" "EmailAddress" e]
				Right email -> Ok email
			_ -> Errors [toException $ ConversionFailed "TEXT" "EmailAddress" "need a text"]

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

class CanVerify a where
	verifyItemData :: a -> (Int64, String)

instance CanVerify Deposit where
	verifyItemData d = (depositId d, "deposits")

data Home = Home {
		renderedDepositForm :: Html,
		depositFormAction :: URI
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

data PlivoConfig = PlivoConfig {
		plivoAuthId    :: String,
		plivoAuthToken :: String,
		plivoTel       :: String
	}
