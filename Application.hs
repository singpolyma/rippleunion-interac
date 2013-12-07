{-# LANGUAGE CPP #-}
module Application (home, processDeposit) where

import Prelude ()
import BasicPrelude

import Network.Wai (Application)
import Network.HTTP.Types (ok200)
import Network.Wai.Util (stringHeaders, textBuilder)

import Network.Wai.Digestive (bodyFormEnv_)
import SimpleForm.Combined (tel, label, Label(..), wdef, vdef)
import SimpleForm.Render.XHTML5 (render)
import SimpleForm.Digestive.Combined (SimpleForm', input, input_, getSimpleForm, postSimpleForm)

import Network.URI (URI(..))
import Network.URI.Partial (relativeTo)

import Database.SQLite.Simple (execute, Connection)

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

home :: URI -> Connection -> Application
home root _ _ = do
	rForm <- getSimpleForm render Nothing depositForm
	textBuilder ok200 headers $ viewHome htmlEscape (Home rForm fPath)
	where
	fPath = processDepositPath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]

processDeposit :: URI -> Connection -> Application
processDeposit root db req = do
	(rForm, dep) <- postSimpleForm render (bodyFormEnv_ req) depositForm
	liftIO $ case dep of
		Just x -> do
			execute db (s"INSERT INTO deposits VALUES (?,?,?,?)") x
			textBuilder ok200 headers $ viewDepositSuccess htmlEscape
				(DepositSuccess hPath)
		Nothing ->
			textBuilder ok200 headers $ viewHome htmlEscape (Home rForm fPath)
	where
	fPath = processDepositPath `relativeTo` root
	hPath = homePath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]
