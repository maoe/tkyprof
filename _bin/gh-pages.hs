{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Category ((>>>))
import Hakyll

main :: IO ()
main = hakyllWith config $ do
  match "_templates/*" $ compile templateCompiler
  match (list ["index.markdown"]) $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "_templates/default.hamlet"
      >>> relativizeUrlsCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration { deployCommand = deploy }
  where deploy = "cp -r _site/* . && runghc _bin/gh-pages.hs clean"
