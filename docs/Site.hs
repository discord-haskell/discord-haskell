{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Data.List
import Data.List.Split

config ::  Configuration
config = defaultConfiguration { providerDirectory = "docs/build" }

hackage :: String -> String
hackage xs = fixUrl $ splitWhen (=='/') xs
  where
    fixUrl ("..":package:ys) = "https://hackage.haskell.org/package/" ++ package ++ "/docs/" ++ intercalate "/" ys
    fixUrl zs = intercalate "/" zs

main :: IO ()
main = do
  hakyllWith config $ do
    match "Network-*.html" $ do
      route idRoute
      compile $ fmap (withUrls hackage) <$> (relativizeUrls =<< getResourceString)

    match "index.html" $ do
      route idRoute
      compile $ relativizeUrls =<< getResourceString

    match "*.css" $ do
      route idRoute
      compile $ relativizeUrls =<< getResourceString

    match "*.js" $ do
      route idRoute
      compile $ relativizeUrls =<< getResourceString
