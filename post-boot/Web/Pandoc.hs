--------------------------------------------------------------------------------
-- |
-- Module : Web.Pandoc
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.Pandoc where

--------------------------------------------------------------------------------
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Highlighting.Kate (haddock, styleToCss)
import Text.Pandoc

--------------------------------------------------------------------------------
readGithubMarkdown :: String -> Pandoc
readGithubMarkdown = readMarkdown readerOpts

--------------------------------------------------------------------------------
readerOpts :: ReaderOptions
readerOpts = def { readerExtensions = githubMarkdownExtensions }

--------------------------------------------------------------------------------
writerOpts :: WriterOptions
writerOpts = def { writerHighlight      = True
                 , writerHighlightStyle = haddock
                 }

--------------------------------------------------------------------------------
writePandocHtml :: Pandoc -> ByteString
writePandocHtml = renderHtml . writeHtml writerOpts

--------------------------------------------------------------------------------
syntaxHighlightingCss :: ByteString
syntaxHighlightingCss = pack $ styleToCss haddock
