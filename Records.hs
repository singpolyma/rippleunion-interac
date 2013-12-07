module Records where

import Data.Int (Int64)
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Data.Text (Text)
import Network.URI (URI)
import Text.Email.Validate (EmailAddress)

import Data.Text.Buildable
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.ToField (toField)
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

instance Buildable URI where
	build = build . show

instance ToRow Deposit where
	toRow (Deposit rid fn email tel amnt) =
		[toField rid, toField fn, toField (show email), toField tel, toField amnt]

data Home = Home {
		renderedDepositForm :: Html,
		depositFormAction :: URI
	}

data DepositSuccess = DepositSuccess {
		homeLink :: URI
	}

data Deposit = Deposit {
		depositId      :: Int64,
		depositorFN    :: Text,
		depositorEmail :: EmailAddress,
		depositorTel   :: Text,
		depositAmount  :: Float
	}
