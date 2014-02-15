{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Home where
import TKYProf hiding (reports)
import Yesod.Form (Enctype(Multipart))
import Data.Maybe (listToMaybe)
import Handler.Reports.Helpers (getAllReports)

import GHC.RTS.TimeAllocProfile

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- TKYProf.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  reports <- getAllReports
  defaultLayout $ do
    setTitle "TKYProf"
    addScript $ StaticR js_jquery_ui_widget_js
    addScript $ StaticR js_jquery_iframe_transport_js
    addScript $ StaticR js_jquery_fileupload_js
    $(widgetFile "home")
