{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.DB
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.DB
    ( Etag
    , PostName
    , Post(..)
    , PostInfo(..)
    , Status(..)
    , insertPost
    , retrievePost
    , retrievePosts
    , updatePost
    ) where

--------------------------------------------------------------------------------
import qualified Data.Text      as S
import qualified Data.Text.Lazy as L

--------------------------------------------------------------------------------
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.Types

--------------------------------------------------------------------------------
type Content  = L.Text
type Etag     = Int
type PostName = S.Text
type Title    = S.Text

--------------------------------------------------------------------------------
data Status
    = NotFound
    | NoChange

--------------------------------------------------------------------------------
data PostInfo
    = PostInfo
      { postName  :: !S.Text
      , postTitle :: !S.Text
      , postEtag  :: !Int
      , postDate  :: !UTCTime
      }

--------------------------------------------------------------------------------
data Post
    = Post
      { postInfo    :: !PostInfo
      , postContent :: !L.Text
      }

--------------------------------------------------------------------------------
insertPost :: S.Text
           -> S.Text
           -> L.Text
           -> Int
           -> UTCTime
           -> Connection
           -> IO ()
insertPost name title content etag date con
    = execute_ con qInsertPost
  where
    row = (name, title, content, etag, date)

--------------------------------------------------------------------------------
retrievePost :: PostName -> Etag -> Connection -> IO (Either Status Post)
retrievePost name etag con
    = do resE <- query con qEtagMatchesPost eRow :: IO [Only Null]
         let matches = not $ null resE

         if matches
             then return $ Left NoChange
             else do resP <- query con qGetPost nRow
                     case resP of
                         []  -> return $ Left NotFound
                         (n,t,c,e,d):_
                             -> let info = makePostInfo n t e d
                                    post = Post
                                           { postInfo    = info
                                           , postContent = c
                                           } in

                                return $ Right post
  where
    eRow = (name, etag)
    nRow = Only name

--------------------------------------------------------------------------------
retrievePosts :: Connection -> IO [PostInfo]
retrievePosts con
    = do ps <- query_ con qGetPosts
         return $ fmap (\(n,t,e,d) -> makePostInfo n t e d)  ps

--------------------------------------------------------------------------------
updatePost :: PostName -> Content -> Etag -> Connection -> IO ()
updatePost name content etag con
    = execute con qUpdatePost row
  where
    row = (content, etag, name)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
makePostInfo :: PostName -> Title -> Etag -> UTCTime -> PostInfo
makePostInfo name title etag date
    = PostInfo
      { postName  = name
      , postTitle = title
      , postEtag  = etag
      , postDate  = date
      }

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------
qInsertPost :: Query
qInsertPost
    = "INSERT INTO posts (name,title,content,etag,date) VALUES (?,?,?,?,?)"

--------------------------------------------------------------------------------
qUpdatePost :: Query
qUpdatePost
    = "UPDATE posts SET content = ?, etag = ? WHERE name = ?"

--------------------------------------------------------------------------------
qGetPost :: Query
qGetPost
    = "SELECT name, title, content, etag, date FROM posts where name = ?"

--------------------------------------------------------------------------------
qEtagMatchesPost :: Query
qEtagMatchesPost
    = "SELECT NULL FROM posts WHERE name = ? AND etag = ?"

--------------------------------------------------------------------------------
qGetPosts :: Query
qGetPosts
    = "SELECT name, title, etag, date FROM posts"
