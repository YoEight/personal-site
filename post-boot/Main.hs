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
import Data.ByteString (pack)
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Traversable
import System.Environment
import System.IO.Error

--------------------------------------------------------------------------------
import Codec.Binary.Base64.String
import Data.Aeson.Types hiding (decode)
import Data.Hashable
import Data.Text (unpack)
import Data.Time
import Data.Vector      hiding ((++), drop, splitAt)
import Database.SQLite.Simple
import Control.Lens
import Network.Wreq
import System.Locale (defaultTimeLocale)

-------------------------------------------------------------------------------
import Web.Pandoc

-------------------------------------------------------------------------------
data Post
    = Post
      { postName :: !String
      , postPath :: !String
      }

-------------------------------------------------------------------------------
main :: IO ()
main = do args <- getArgs
          case args of
              db:_
                  -> withConnection db $ \con ->
                         do execute_ con qCreatePosts
                            traverse_ (processPost con) =<< getPosts

              _   -> throwError "Wrong usage"

-------------------------------------------------------------------------------
contentDir :: String
contentDir = "https://api.github.com/repos/YoEight/website-posts/contents/"

--------------------------------------------------------------------------------
postLocation :: String -> String
postLocation path = contentDir ++ path

--------------------------------------------------------------------------------
getPosts :: IO [Post]
getPosts
    = do r <- asJSON =<< get contentDir
         let code = r ^. responseStatus . statusCode
             json = r ^. responseBody

         if code /= 200
             then throwError "Error when fetching posts from github"
             else either throwError return $ extractPosts json

--------------------------------------------------------------------------------
fetchPostContent :: Post -> IO String
fetchPostContent post
    = do r <- asJSON =<< get (postLocation path)
         let code = r ^. responseStatus . statusCode
             json = r ^. responseBody

         if code /= 200
             then throwError $ "Error when fetching post " ++ path
             else either throwError return $ extractPostContent json
  where
    path = postPath post

--------------------------------------------------------------------------------
processPost :: Connection -> Post -> IO ()
processPost con post
    = do content <- fetchPostContent post
         let pandoc        = readGithubMarkdown content
             html          = writePandocHtml pandoc
             style         = syntaxHighlightingCss
             (date, title) = parseArticle name
             etag          = hash html
             row           = (name, title, style, html, etag, date)

         execute con qInsertPost row

  where
    name = postName post

--------------------------------------------------------------------------------
-- Extractors
--------------------------------------------------------------------------------
extractPosts :: Value -> Either String [Post]
extractPosts value
    = parseEither extract value
  where
    extract (Array arr)
        = do arr' <- traverse extractPath arr
             return $ toList arr'
    extract _
        = mzero

--------------------------------------------------------------------------------
extractPath :: Value -> Parser Post
extractPath (Object m)
    = liftM2 Post (m .: "name") (m .: "path")
extractPath _
    = mzero

--------------------------------------------------------------------------------
extractPostContent :: Value -> Either String String
extractPostContent value
    = parseEither extract value
  where
    extract (Object m)
        = do enc <- m .: "encoding"
             con <- m .: "content"
             case enc of
                 "base64" -> return $ decode con
                 x        -> let tech = unpack x
                                 msg  = "Unsupported encoding " ++ tech in
                             fail msg
    extract _
        = mzero

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------
qInsertPost :: Query
qInsertPost
    = "INSERT INTO posts \
      \(name,title,style, content,etag,date) \
      \VALUES (?,?,?,?,?,?)"

--------------------------------------------------------------------------------
qCreatePosts :: Query
qCreatePosts
    = "CREATE TABLE posts(\
      \    name    TEXT NOT NULL,\
      \    title   TEXT NOT NULL,\
      \    style   TEXT NOT NULL,\
      \    content BLOB NOT NULL,\
      \    etag    INT  NOT NULL,\
      \    date    DATE NOT NULL \
      \);"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
throwError :: String -> IO a
throwError = ioError . userError

--------------------------------------------------------------------------------
parseArticle :: String -> (UTCTime, String)
parseArticle name
    = (date, title)
  where
    (dateStr, titleStr)
        = splitAt 10 name

    date  = readTime defaultTimeLocale "%Y-%m-%d" dateStr
    title = replace '_' ' ' $ drop 1 titleStr

--------------------------------------------------------------------------------
replace :: Eq a => a -> a -> [a] -> [a]
replace trg val = fmap (\c -> if c == trg then val else c)
