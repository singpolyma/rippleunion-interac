{-# LANGUAGE CPP #-}
module Application (home) where

import Prelude ()
import BasicPrelude
import Network.Wai (Application)
import Network.HTTP.Types (ok200)
import Network.Wai.Util (stringHeaders, textBuilder)
import Network.URI (URI(..))

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

home :: URI -> Application
home _ _ =
	textBuilder ok200 headers (viewHome htmlEscape (Home (s"")))
	where
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]
