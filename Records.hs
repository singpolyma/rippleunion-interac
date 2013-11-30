module Records where

import Data.Text.Buildable
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)
import Data.Text (Text)
import Network.URI (URI)
import Text.Email.Validate (EmailAddress)

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

instance Buildable URI where
	build = build . show

data Home = Home {
		renderedDepositForm :: Html,
		depositFormAction :: URI
	}

data Deposit = Deposit {
		depositorFN    :: Text,
		depositorEmail :: EmailAddress,
		depositorTel   :: Text,
		depositAmount  :: Float
	}
