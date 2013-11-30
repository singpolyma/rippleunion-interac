{-# LANGUAGE CPP #-}
module Application (home) where

import Prelude ()
import BasicPrelude

import Network.Wai (Application)
import Network.HTTP.Types (ok200)
import Network.Wai.Util (stringHeaders, textBuilder)

import SimpleForm.Combined (tel, label, Label(..), wdef, vdef)
import SimpleForm.Render.XHTML5 (render)
import SimpleForm.Digestive.Combined (SimpleForm', input, input_, getSimpleForm)

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

lbl :: String -> Maybe Label
lbl = Just . Label . s

depositForm :: (Monad m) => SimpleForm' m Deposit
depositForm = do
	fn'     <- input (s"fn") (Just . depositorFN) (wdef,vdef) (mempty { label = lbl"Full name"})
	email'  <- input_ (s"email") (Just . depositorEmail)
	tel'    <- input  (s"tel") (Just . depositorTel) tel (mempty {label = lbl"Telephone number"})
	amount' <- input_ (s"amount") (Just . depositAmount)

	return $ Deposit <$> fn' <*> email' <*> tel' <*> amount'

home :: URI -> Application
home _ _ = do
	renderedForm <- getSimpleForm render Nothing depositForm
	textBuilder ok200 headers $ viewHome htmlEscape (Home renderedForm)
	where
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]
