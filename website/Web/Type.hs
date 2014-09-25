{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.Type
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.Type where

--------------------------------------------------------------------------------
import Data.Foldable

--------------------------------------------------------------------------------
import Data.Text
import Data.Time

--------------------------------------------------------------------------------
data Input
    = forall f. Foldable f => Index (f ArticleInfo)
    | GetArticle (Maybe Article)

--------------------------------------------------------------------------------
data Output
    = forall f. Foldable f => ArticleList (f ArticleInfo)
    | ArticleView (Maybe Article)
    | NotFound

--------------------------------------------------------------------------------
data AppEnv
    = AppEnv
      { envPostRepo :: !String }

--------------------------------------------------------------------------------
data AppState = AppState

--------------------------------------------------------------------------------
data Article
    = Article
      { articleInfo    :: !ArticleInfo
      , articleStyle   :: !Text
      , articleContent :: !Text
      }

--------------------------------------------------------------------------------
data ArticleInfo
    = ArticleInfo
      { articleName  :: !Text
      , articleTitle :: !Text
      , articleDate  :: !UTCTime
      }
