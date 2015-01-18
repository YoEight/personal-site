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
import Data.Foldable

--------------------------------------------------------------------------------
import Data.Text (Text)
import Data.Time
import Network.HTTP.Types
import Network.Wai
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html (preEscapedToHtml, unsafeByteString)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Hamlet (shamletFile)

--------------------------------------------------------------------------------
import Web.Type

--------------------------------------------------------------------------------
type Respond = Response -> IO ResponseReceived

--------------------------------------------------------------------------------
view :: Respond -> Output -> IO ResponseReceived
view respond o
    = case o of
          ArticleList as
              -> viewArticleList respond as
          ArticleView m
              -> case m of
                      Nothing  -> respond $ responseLBS status304 keepAlive ""
                      Just art -> viewArticleView respond art
          NotFound
              -> respond $ responseLBS status404 headers "Not Found !"

  where
    keepAlive = [(hConnection,  "keep-alive")]
    headers   = (hContentType, "text/plain") : keepAlive

--------------------------------------------------------------------------------
viewArticleList :: Foldable f => Respond -> f ArticleInfo -> IO ResponseReceived
viewArticleList respond articles
    = respond $ responseBuilder status202 headers html
  where
    title   = "Home" :: Text
    header  = ""     :: Text
    content = $(shamletFile "html/articles.hamlet")
    html    = renderHtmlBuilder $(shamletFile "html/template.hamlet")
    headers = [ (hContentType, "text/html;charset=utf-8")
              , (hConnection,  "keep-alive")
              ]

--------------------------------------------------------------------------------
viewArticleView :: Respond -> Article -> IO ResponseReceived
viewArticleView respond article
    = respond $ responseBuilder status202 headers html
  where
    doc     = articleContent article
    style   = preEscapedToHtml $ articleStyle article --unsafeByteString $ encodeUtf8 $ articleStyle article
    info    = articleInfo article
    title   = articleTitle info
    date    = formatTime defaultTimeLocale "%Y-%m-%d" $ articleDate info
    header  = $(shamletFile "html/article-header.hamlet")
    docHtml = unsafeByteString doc
    content = $(shamletFile "html/article.hamlet")
    html    = renderHtmlBuilder $(shamletFile "html/template.hamlet")
    headers = [ (hContentType, "text/html;charset=utf-8")
              , (hConnection,  "keep-alive")
              ]
