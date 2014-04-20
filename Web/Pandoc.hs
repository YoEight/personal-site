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
import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Text.Blaze.Html (Html, unsafeByteString)
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
writePandocHtml :: Pandoc -> Html
writePandocHtml = writeHtml writerOpts

--------------------------------------------------------------------------------
syntaxHighlightingCss :: Html
syntaxHighlightingCss = unsafeByteString $ pack $ styleToCss haddock
