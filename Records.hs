module Records where

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
	toRow (Deposit fn email tel amount) =
		[toField fn, toField (show email), toField tel, toField amount]

data Home = Home {
		renderedDepositForm :: Html,
		depositFormAction :: URI
	}

data DepositSuccess = DepositSuccess {
		homeLink :: URI
	}

data Deposit = Deposit {
		depositorFN    :: Text,
		depositorEmail :: EmailAddress,
		depositorTel   :: Text,
		depositAmount  :: Float
	}
