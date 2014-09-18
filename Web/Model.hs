{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.Model
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.Model where

--------------------------------------------------------------------------------
import Control.Monad.State
import Data.List
import System.Locale (defaultTimeLocale)

--------------------------------------------------------------------------------
import Data.Time

--------------------------------------------------------------------------------
import Web.Pandoc
import Web.Type

--------------------------------------------------------------------------------
model :: MonadState AppState m => Input -> m Output
model (Index names)       = modelIndex names
model (GetArticle name c) = modelGetArticle name c

--------------------------------------------------------------------------------
modelIndex :: MonadState AppState m => [String] -> m Output
modelIndex = return . ArticleList . sortArticles . fmap parseArticle

--------------------------------------------------------------------------------
modelGetArticle :: MonadState AppState m => String -> String -> m Output
modelGetArticle name content
    = return $ ArticleView title date doc
  where
    (date, titleStr)
        = splitAt 10 name

    title   = replace '_' ' ' $ drop 1 titleStr
    doc     = readGithubMarkdown content

--------------------------------------------------------------------------------
parseArticle :: String -> Article
parseArticle name
    = Article name date title
  where
    (dateStr, titleStr)
        = splitAt 10 name

    date  = readTime defaultTimeLocale "%Y-%m-%d" dateStr
    title = replace '_' ' ' $ drop 1 titleStr

--------------------------------------------------------------------------------
replace :: Eq a => a -> a -> [a] -> [a]
replace trg val = map (\c -> if c == trg then val else c)

--------------------------------------------------------------------------------
sortArticles :: [Article] -> [Article]
sortArticles = reverse . sortBy go where
  go a b = compare (articleDate a) (articleDate b)
