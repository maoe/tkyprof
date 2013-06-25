{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
  ( withTKYProf
  , withDevelApp
  ) where

import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, toDyn)
import Settings
import TKYProf
import Yesod.Static
-- Import all relevant handler modules here.
import Handler.Home
import Handler.Reports

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in TKYProf.hs. Please see
-- the comments there for more details.
mkYesodDispatch "TKYProf" resourcesTKYProf

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/png" "config/favicon.png"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withTKYProf :: (Application -> IO a) -> IO a
withTKYProf f = do
  rs <- atomically $ emptyReports
  s <- static Settings.staticdir
  let h = TKYProf { getStatic  = s
                  , getReports = rs }
  toWaiApp h >>= f

withDevelApp :: Dynamic
withDevelApp = toDyn (withTKYProf :: (Application -> IO ()) -> IO ())
