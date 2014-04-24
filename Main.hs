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
import Data.Configurator
import Data.Default (def)
import Data.Text.Lazy (Text)
import Data.Time
import Network.HTTP.Types (status404)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Locale (defaultTimeLocale)
import System.Log.FastLogger (newFileLoggerSet)
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty

--------------------------------------------------------------------------------
import Web.Pandoc

--------------------------------------------------------------------------------
data Article =
    Article { articlePath  :: FilePath
            , articleDate  :: UTCTime
            , articleTitle :: String
            } deriving Show

--------------------------------------------------------------------------------
main :: IO ()
main = do
    cfg        <- load [Required "config/site.cfg"]
    port       <- require cfg "site_port"
    logFile    <- require cfg "log_file"
    logBufSize <- require cfg "log_buffer"
    logSet     <- newFileLoggerSet logBufSize logFile
    app        <- scottyApp handlers
    let settings = def
                   { outputFormat = Detailed True
                   , destination  = Logger logSet
                   }
    mware <- mkRequestLogger settings
    run port $ mware app

--------------------------------------------------------------------------------
handlers :: ScottyM ()
handlers = do
    get "/" index
    get "/article/:name" article

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
    name  <- param "name"
    withArticleContent name $ \doc -> do
        let pandoc   = readGithubMarkdown doc
            title    = replace '_' ' ' $ drop 11 name
            date     = take 10 name
            header   = $(shamletFile "html/article-header.hamlet")
            acontent = writePandocHtml pandoc
            content  = $(shamletFile "html/article.hamlet")
        keepAlive
        html $ renderHtml $(shamletFile "html/template.hamlet")

--------------------------------------------------------------------------------
withArticleContent :: FilePath -> (String -> ActionM ()) -> ActionM ()
withArticleContent path k =
    either handler k =<< liftIO (tryIOError $ readFile filepath)
  where
    filepath = "articles" </> path
    handler e
        | isDoesNotExistError e = status status404 >> text "Not Found"
        | otherwise             = liftIO $ ioError e

--------------------------------------------------------------------------------
sortArticles :: [Article] -> [Article]
sortArticles = reverse . sortBy go where
  go a b = compare (articleDate a) (articleDate b)

--------------------------------------------------------------------------------
usefulContents :: FilePath -> IO [FilePath]
usefulContents = fmap (filter go) . getDirectoryContents
  where
    go p = p /= "." && p /= ".."

--------------------------------------------------------------------------------
loadArticles :: IO [Article]
loadArticles = fmap (sortArticles . fmap go) $ usefulContents "articles"
  where
    go p =
        let date  = readTime defaultTimeLocale "%Y-%m-%d" $ take 10 p
            title = replace '_' ' ' $ drop 11 p in
        Article p date title

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
