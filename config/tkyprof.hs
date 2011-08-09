{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withTKYProf)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withTKYProf $ run 3000
#else
import Controller (withTKYProf)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  let port = 3000
  hPutStrLn stderr $ "Application launched, listening on port " ++ show port
  withTKYProf $ run port . debug
#endif
