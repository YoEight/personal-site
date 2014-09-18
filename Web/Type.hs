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
import Data.Time
import Text.Pandoc

--------------------------------------------------------------------------------
data Input
    = Index [String]
    | GetArticle String String

--------------------------------------------------------------------------------
data Output
    = ArticleList [Article]
    | ArticleView String String Pandoc
    | NotFound

--------------------------------------------------------------------------------
data AppState = AppState

--------------------------------------------------------------------------------
data Article
    = Article
      { articleName  :: !String
      , articleDate  :: !UTCTime
      , articleTitle :: !String
      }
    deriving Show
