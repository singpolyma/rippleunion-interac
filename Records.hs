module Records where

import Data.Text.Buildable
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (MarkupM)
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

data Home = Home {
		depositForm :: Html
	}
