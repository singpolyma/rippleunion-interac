module Main (main) where

import Prelude hiding (FilePath)
import System.Environment (getArgs)
import Network.URI (parseAbsoluteURI, URI(..), URIAuth(..))
import System.IO (hPutStrLn, stderr)
import Filesystem.Path.CurrentOS (FilePath)
import Filesystem (getWorkingDirectory)
import Database.SQLite.Simple (open)
import OpenSSL (withOpenSSL)

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)

import Network.Wai.Dispatch
import Records
import Routes

addTrailingSlash :: URI -> URI
addTrailingSlash u@(URI {uriPath = []}) = u {uriPath = "/"}
addTrailingSlash u@(URI {uriPath = p})
	| last p == '/' = u
	| otherwise = u {uriPath = p ++ "/"}

staticRoot :: FilePath -> Application
staticRoot = staticApp . defaultWebAppSettings

app :: URI -> Int -> String -> String -> String -> String -> Maybe String -> IO ()
app root port dbpth pid ptk ptl portOverride = do
	cwd <- getWorkingDirectory
	db <- open dbpth
	run realPort $
		logStdoutDev $ autohead $ acceptOverride $ jsonp $ -- Middleware
		dispatch (staticRoot cwd) $ routes root db plivo   -- Do routing
	where
	realPort = maybe port read portOverride
	plivo = PlivoConfig pid ptk ptl

main :: IO ()
main = withOpenSSL $ do
	args <- getArgs
	case args of
		[dbp, pid, ptk, ptl, root, port] ->
			main' (fmap addTrailingSlash $ parseAbsoluteURI root) dbp pid ptk ptl (Just port)
		[dbp, pid, ptk, ptl, root] ->
			main' (fmap addTrailingSlash $ parseAbsoluteURI root) dbp pid ptk ptl Nothing
		[dbp, pid, ptk, ptl] ->
			main' (parseAbsoluteURI "http://localhost:3000/") dbp pid ptk ptl Nothing
		_ -> hPutStrLn stderr "Usage: ./Main <db path> <Plivo AUTHID> <Plivo AUTH TOKEN> <Plivo Telephone Number> <Root URI>"
	where
	main' (Just r@(URI {uriAuthority = Just (URIAuth {uriPort = ':':port})})) =
		app r (read port)
	main' (Just r@(URI {uriAuthority = Just (URIAuth {uriPort = ""})})) =
		app r 80
	main' _ = const $ const $ const $ const $ const $
		hPutStrLn stderr "Invalid Root URI given"
