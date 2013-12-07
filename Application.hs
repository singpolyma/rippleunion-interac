{-# LANGUAGE CPP #-}
module Application (home, processDeposit) where

import Prelude ()
import BasicPrelude
import System.Random (randomRIO)
import Database.SQLite3 (SQLError(..), Error(ErrorConstraint))

import Network.Wai (Application)
import Network.HTTP.Types (ok200)
import Network.Wai.Util (stringHeaders, textBuilder)

import Network.Wai.Digestive (bodyFormEnv_)
import SimpleForm.Combined (tel, label, Label(..), wdef, vdef)
import SimpleForm.Render.XHTML5 (render)
import SimpleForm.Digestive.Combined (SimpleForm', input, input_, getSimpleForm, postSimpleForm)
import Text.Digestive.Form (monadic)

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

depositForm :: (Functor m, MonadIO m) => SimpleForm' m Deposit
depositForm = do
	fn'     <- input (s"fn") (Just . depositorFN) (wdef,vdef) (mempty { label = lbl"Full name"})
	email'  <- input_ (s"email") (Just . depositorEmail)
	tel'    <- input  (s"tel") (Just . depositorTel) tel (mempty {label = lbl"Telephone number"})
	amount' <- input_ (s"amount") (Just . depositAmount)

	let rid = monadic $ fmap pure $ liftIO $ randomRIO (1,999999)
	return $ Deposit <$> rid <*> fn' <*> email' <*> tel' <*> amount'

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
			insertDeposit db x
			textBuilder ok200 headers $ viewDepositSuccess htmlEscape
				(DepositSuccess hPath)
		Nothing ->
			textBuilder ok200 headers $ viewHome htmlEscape (Home rForm fPath)
	where
	fPath = processDepositPath `relativeTo` root
	hPath = homePath `relativeTo` root
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]

-- | Increments id until success
insertDeposit :: Connection -> Deposit -> IO ()
insertDeposit db x = do
	r <- try $ execute db (s"INSERT INTO deposits VALUES (?,?,?,?,?)") x
	case r of
		Left (SQLError ErrorConstraint _ _) ->
			insertDeposit db (x {depositId = succ $ depositId x})
		Left e -> throwIO e
		Right () -> return ()
