{-# LANGUAGE DeriveDataTypeable, CPP #-}
import Controller (withTKYProf)
import System.IO (hPutStrLn, stderr)

#if PRODUCTION
import Data.Version (showVersion)
import Network.Wai.Handler.Warp (Port, run)
import Paths_tkyprof (getDataDir, version)
import System.Console.CmdArgs
import System.Directory (setCurrentDirectory)

main :: IO ()
main = do
  getDataDir >>= setCurrentDirectory
  TKYProfArg p <- cmdArgs tkyProfArg
  hPutStrLn stderr $ "TKYProf " ++ showVersion version ++ " launched, listening on http://localhost:" ++ show p ++ "/"
  withTKYProf $ run p

data TKYProfArg = TKYProfArg
  { port    :: Port
  } deriving (Show, Data, Typeable)

tkyProfArg :: TKYProfArg
tkyProfArg = TKYProfArg { port = 3000 &= help "Port number" }
             &= summary ("TKYProf " ++ showVersion version)
#else
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  let port = 3000
  hPutStrLn stderr $ "Application launched, listening on port " ++ show port
  withTKYProf $ run port . debug
#endif

{-
import Network.Wai.Handler.Webkit (run)

main :: IO ()
main = withTKYProf $ run "Devel.TKYProf"
-}
