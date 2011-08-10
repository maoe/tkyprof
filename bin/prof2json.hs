module Main where
import Blaze.ByteString.Builder (toByteStringIO)
import Control.Applicative
import Control.Exception (bracket)
import Control.Monad.Trans (liftIO)
import Data.Aeson (toJSON)
import Data.Aeson.Encode (fromValue)
import Data.Enumerator (Iteratee, ($$), run_)
import ProfilingReport (ProfilingReport(..), profilingReportI)
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import System.IO (IOMode(WriteMode), openFile, hClose)
import qualified Data.ByteString as S
import qualified Data.Enumerator.Binary as E

main :: IO ()
main = getArgs >>= mapM_ (run_ . job)

job :: FilePath -> Iteratee S.ByteString IO ()
job f = E.enumFile f $$ iterJob json
  where
    json = dropExtension f ++ ".json"

iterJob :: FilePath -> Iteratee S.ByteString IO ()
iterJob fpath = do
  builder <- fromValue . toJSON . reportCostCentres <$> profilingReportI
  liftIO $ bracket (openFile fpath WriteMode)
                   hClose
                   (\h -> toByteStringIO (S.hPut h) builder)
