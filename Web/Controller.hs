{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.Controller
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.Controller where

--------------------------------------------------------------------------------
import System.IO.Error (isDoesNotExistError, tryIOError)

--------------------------------------------------------------------------------
import Data.Text (Text, unpack)
import Network.HTTP.Types
import Network.Wai
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
import Web.Type

--------------------------------------------------------------------------------
controller :: Request -> IO (Maybe Input)
controller req
    | requestMethod req == methodGet
      = dispatch req
    | otherwise
      = return Nothing

--------------------------------------------------------------------------------
dispatch :: Request -> IO (Maybe Input)
dispatch req
    = case pathInfo req of
          []                -> dispatchIndex
          "article":name:[] -> dispatchGetArticle name
          _                 -> return Nothing

--------------------------------------------------------------------------------
dispatchIndex :: IO (Maybe Input)
dispatchIndex
    = do xs <- loadArticleNames
         return $ Just $ Index xs

--------------------------------------------------------------------------------
dispatchGetArticle :: Text -> IO (Maybe Input)
dispatchGetArticle name
    = do eContent <- tryIOError $ readFile path
         case eContent of
             Left e
                 | isDoesNotExistError e
                   -> return Nothing
                 | otherwise
                   -> ioError e
             Right content
                 -> return $ Just $ GetArticle nameStr content
  where
    nameStr = unpack name
    path    = "articles" </> nameStr

--------------------------------------------------------------------------------
loadArticleNames :: IO [String]
loadArticleNames
    = do xs <- getDirectoryContents "articles"
         let filt path = path /= "." && path /= ".."
             cleaned   = filter filt xs
         return cleaned
