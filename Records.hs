module Records where

import Data.Text.Buildable
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)
import Data.Text (Text)
import Text.Email.Validate (EmailAddress)

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

data Home = Home {
		renderedDepositForm :: Html
	}

data Deposit = Deposit {
		depositorFN    :: Text,
		depositorEmail :: EmailAddress,
		depositorTel   :: Text,
		depositAmount  :: Float
	}
