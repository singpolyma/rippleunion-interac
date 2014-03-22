module Main (main) where

import Prelude hiding (FilePath)
import System.Environment (getArgs)
import Network.URI (parseAbsoluteURI, URI(..), URIAuth(..))
import System.IO (hPutStrLn, stderr)
import Filesystem.Path.CurrentOS (FilePath)
import Filesystem (getWorkingDirectory)
import Database.SQLite.Simple (open)
import OpenSSL (withOpenSSL)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Web.Stripe.Client as Stripe

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)

import Network.Wai.Session (withSession)
import Network.Wai.Session.ClientSession (clientsessionStore)
import Web.ClientSession (getKey)
import Web.Cookie (SetCookie, def, setCookieDomain, setCookieSecure, setCookiePath, setCookieExpires)
import Data.Vault (newKey)

import Network.Wai.Dispatch
import Records
import Routes

scookie :: URI -> SetCookie
scookie (URI {uriAuthority = Just (URIAuth {uriRegName = domain})}) = def {
		setCookieDomain = Just $ T.encodeUtf8 $ T.pack ('.':domain),
		setCookiePath = Just $ T.encodeUtf8 $ T.singleton '/',
		setCookieExpires = Just $ UTCTime (ModifiedJulianDay 100000) 0,
		setCookieSecure = True
	}
scookie _ = error "No domain in the root URI for cookie"

addTrailingSlash :: URI -> URI
addTrailingSlash u@(URI {uriPath = []}) = u {uriPath = "/"}
addTrailingSlash u@(URI {uriPath = p})
	| last p == '/' = u
	| otherwise = u {uriPath = p ++ "/"}

staticRoot :: FilePath -> Application
staticRoot = staticApp . defaultWebAppSettings

app :: URI -> Int -> String -> String -> String -> String -> String -> String -> Maybe String -> IO ()
app root port dbpth skpth pid ptk ptl stripeStr portOverride = do
	cwd <- getWorkingDirectory
	db <- open dbpth
	sess <- newKey
	store <- clientsessionStore `fmap` getKey skpth
	run realPort $
		withSession store (s"RippleUnionSession") (scookie root) sess $
		logStdoutDev $ autohead $ acceptOverride $ jsonp $           -- Middleware
		dispatch (staticRoot cwd) $ routes root db sess plivo stripe -- Do routing
	where
	stripe = Stripe.defaultConfig $ Stripe.SecretKey $ T.pack stripeStr
	realPort = maybe port read portOverride
	plivo = PlivoConfig pid ptk ptl

main :: IO ()
main = withOpenSSL $ do
	args <- getArgs
	case args of
		[dbp, sk, pid, ptk, ptl, s, root, port] ->
			main' (fmap addTrailingSlash $ parseAbsoluteURI root) dbp sk pid ptk ptl s (Just port)
		[dbp, sk, pid, ptk, ptl, s, root] ->
			main' (fmap addTrailingSlash $ parseAbsoluteURI root) dbp sk pid ptk ptl s Nothing
		[dbp, sk, pid, ptk, ptl, s] ->
			main' (parseAbsoluteURI "http://localhost:3000/") dbp sk pid ptk ptl s Nothing
		_ -> hPutStrLn stderr "Usage: ./Main <db path> <path to session key file> <Plivo AUTHID> <Plivo AUTH TOKEN> <Plivo Telephone Number> <Stripe Secret> <Root URI>"
	where
	main' (Just r@(URI {uriAuthority = Just (URIAuth {uriPort = ':':port})})) =
		app r (read port)
	main' (Just r@(URI {uriAuthority = Just (URIAuth {uriPort = ""})})) =
		app r 80
	main' _ = const $ const $ const $ const $ const $ const $ const $
		hPutStrLn stderr "Invalid Root URI given"
