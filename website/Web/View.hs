{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.View
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.View where

--------------------------------------------------------------------------------
import Data.Text (Text)
import Data.Time
import Network.HTTP.Types
import Network.Wai
import System.Locale (defaultTimeLocale)
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Pandoc

--------------------------------------------------------------------------------
import Web.Pandoc
import Web.Type

--------------------------------------------------------------------------------
type Respond = Response -> IO ResponseReceived

--------------------------------------------------------------------------------
view :: Respond -> Output -> IO ResponseReceived
view respond o
    = case o of
          ArticleList as
              -> viewArticleList respond as
          ArticleView title date doc
              -> viewArticleView respond title date doc
          NotFound
              -> respond $ responseLBS status404 headers "Not Found !"

  where
    headers = [ (hContentType, "text/plain")
              , (hConnection,  "keep-alive")
              ]

--------------------------------------------------------------------------------
viewArticleList :: Respond -> [Article] -> IO ResponseReceived
viewArticleList respond articles
    = respond $ responseBuilder status202 headers html
  where
    title   = "Home" :: Text
    header  = ""     :: Text
    content = $(shamletFile "html/articles.hamlet")
    html    = renderHtmlBuilder $(shamletFile "html/template.hamlet")
    headers = [ (hContentType, "text/html")
              , (hConnection,  "keep-alive")
              ]

--------------------------------------------------------------------------------
viewArticleView :: Respond -> String -> String -> Pandoc -> IO ResponseReceived
viewArticleView respond title date doc
    = respond $ responseBuilder status202 headers html
  where
    header  = $(shamletFile "html/article-header.hamlet")
    docHtml = writePandocHtml doc
    content = $(shamletFile "html/article.hamlet")
    html    = renderHtmlBuilder $(shamletFile "html/template.hamlet")
    headers = [ (hContentType, "text/html")
              , (hConnection,  "keep-alive")
              ]
