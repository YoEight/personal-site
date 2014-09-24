{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Main
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-------------------------------------------------------------------------------
import Control.Monad
import Data.Maybe
import Data.Traversable

--------------------------------------------------------------------------------
import Data.Aeson.Types
import Data.Text (unpack)
import Data.Vector
import Control.Lens
import Network.Wreq

-------------------------------------------------------------------------------
main :: IO ()
main = do paths <- getPostsPath
          print paths

-------------------------------------------------------------------------------
contentDir :: String
contentDir = "https://api.github.com/repos/YoEight/website-posts/contents/"

--------------------------------------------------------------------------------
getPostsPath :: IO [String]
getPostsPath
    = do r  <- asJSON =<< get contentDir
         let code = r ^. responseStatus . statusCode
             json = r ^. responseBody

         if code /= 200
             then return []
             else return $ extractPostsPath json

--------------------------------------------------------------------------------
extractPostsPath :: Value -> [String]
extractPostsPath value
    = fromMaybe [] $ parseMaybe extract value
  where
    extract (Array arr)
        = do arr' <- traverse extractPath arr
             return $ toList arr'
    extract _
        = mzero

--------------------------------------------------------------------------------
extractPath :: Value -> Parser String
extractPath (Object m)
    = m .: "path"
extractPath _
    = mzero
