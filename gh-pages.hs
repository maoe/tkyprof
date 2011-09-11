{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Category ((>>>))
import Hakyll

main :: IO ()
main = hakyllWith config $ do
  match "templates/*" $ compile templateCompiler
  match (list ["index.markdown"]) $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/default.hamlet"
      >>> relativizeUrlsCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration { deployCommand = deploy }
  where deploy = "cp -r _site/* . && runghc gh-pages.hs clean"
