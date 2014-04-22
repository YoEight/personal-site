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
--------------------------------------------------------------------------------
import           Control.Monad (join)
import           Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import           Data.List (sortBy)
import           Data.Foldable (for_)
import           Data.Traversable (traverse)
import           System.IO.Error (isDoesNotExistError, tryIOError)

--------------------------------------------------------------------------------
import Data.Text.Lazy (Text)
import Network.HTTP.Types (status404)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty

--------------------------------------------------------------------------------
import Web.Pandoc

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
    keepAlive
    html $ renderHtml $(shamletFile "html/template.hamlet")

--------------------------------------------------------------------------------
article :: ActionM ()
article = do
    year  <- param "year"
    month <- param "month"
    day   <- param "day"
    name  <- param "name"
    withArticleContent (year </> month </> day </> name) $ \doc -> do
        let pandoc  = readGithubMarkdown doc
            title   = replace '_' ' ' name
            header  = $(shamletFile "html/article-header.hamlet")
            content = writePandocHtml pandoc
        keepAlive
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
loadSortedContents path =
    fmap (sortFilePath . filter useful) $ getDirectoryContents path
  where
    useful path = path /= "." && path /= ".."

--------------------------------------------------------------------------------
toInt :: String -> Int
toInt = read

--------------------------------------------------------------------------------
sortFilePath :: [FilePath] -> [FilePath]
sortFilePath = reverse . sortBy go where
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

--------------------------------------------------------------------------------
keepAlive :: ActionM ()
keepAlive = do
    con <- header "Connection"
    for_ con $ \c ->
        if c == "keep-alive"
        then setHeader "Connection" c
        else return ()
