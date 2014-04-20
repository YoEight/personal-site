{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
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
--------------------------------------------------------------------------------
import           Control.Monad (join)
import           Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import           Data.List (sortBy)
import           Data.Maybe
import           Data.Foldable (for_)
import           Data.Traversable (traverse)
import           System.IO.Error

--------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as B
import           Data.Default
import           Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy.IO as T
import           Network.HTTP.Types
import           System.Directory
import           System.FilePath
import Text.Cassius
import Text.Hamlet
import           Text.Highlighting.Kate
import Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html hiding (Tag)
import           Text.HTML.TagSoup
import Text.XML.Light
import Text.XML.Light.Cursor
import           Text.Pandoc
import           Web.Scotty

--------------------------------------------------------------------------------
data Article =
    Article { articlePath  :: FilePath
            , articleDate  :: String
            , articleTitle :: String
            } deriving Show

--------------------------------------------------------------------------------
main :: IO ()
main = scotty 3000 $ do
    get "/" index
    get "/article/:year/:month/:day/:name" article

--------------------------------------------------------------------------------
index :: ActionM ()
index = do
    articles <- liftIO loadArticles
    let title   = "Home" :: Text
        content = $(shamletFile "html/articles.hamlet")
        header  =  ""    :: Text
    html $ renderHtml $(shamletFile "html/template.hamlet")

--------------------------------------------------------------------------------
article :: ActionM ()
article = do
    year  <- param "year"
    month <- param "month"
    day   <- param "day"
    name  <- param "name"
    withArticleContent (year </> month </> day </> name) $ \doc -> do
        let reader_opts = def { readerExtensions = githubMarkdownExtensions }
            writer_opts = def { writerHighlight      = True
                              , writerHighlightStyle = haddock
                              }
            pandoc  = readMarkdown reader_opts doc
            title   = replace '_' ' ' name
            css     = unsafeByteString $ B.pack $ styleToCss haddock
            header  = $(shamletFile "html/article-header.hamlet")
            content = writeHtml writer_opts pandoc
        html $ renderHtml $(shamletFile "html/template.hamlet")

--------------------------------------------------------------------------------
withArticleContent :: FilePath -> (String -> ActionM ()) -> ActionM ()
withArticleContent path k =
    either handler k =<< liftIO (tryIOError $ readFile filepath)
  where
    filepath = "articles/" ++ path
    handler e
        | isDoesNotExistError e = status status404 >> text "Not Found"
        | otherwise             = liftIO $ ioError e

--------------------------------------------------------------------------------
loadSortedContents :: FilePath -> IO [FilePath]
loadSortedContents path =  fmap (filter useful) $ getDirectoryContents path
  where
    useful path = path /= "." && path /= ".."

--------------------------------------------------------------------------------
toInt :: String -> Int
toInt = read

--------------------------------------------------------------------------------
sortFilePath :: [FilePath] -> [FilePath]
sortFilePath = sortBy go where
  go a b = compare (toInt a) (toInt b)

--------------------------------------------------------------------------------
data Scope = Day
           | Month
           | Year deriving Enum

--------------------------------------------------------------------------------
loadArticles :: IO [Article]
loadArticles = fmap join . traverse (go "" Year) =<< loadSortedContents root
  where
    root = "articles"
    go p Day dir =
        let finalDir = p </> dir
            leaf     = root </> finalDir
            date     = replace '/' '.' finalDir
            make n   = Article (finalDir </> n) date (replace '_' ' ' n) in
        fmap (fmap make) (loadSortedContents leaf)
    go p s dir =
        let newDir = p    </> dir
            newP   = root </> newDir
            newS   = pred s in
        fmap join . traverse (go newDir newS) =<< loadSortedContents newP

--------------------------------------------------------------------------------
replace :: Eq a => a -> a -> [a] -> [a]
replace trg val = map (\c -> if c == trg then val else c)
