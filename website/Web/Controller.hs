{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.Controller
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.Controller where

--------------------------------------------------------------------------------
import Control.Monad
import Data.ByteString.Char8 (pack)
import Data.Traversable

--------------------------------------------------------------------------------
import           Control.Lens
import qualified Codec.Binary.Base64.String as Base64
import           Data.Aeson.Types
import           Data.Text (Text, unpack)
import           Data.Vector (Vector)
import           Network.HTTP.Types hiding (statusCode)
import           Network.Wai hiding (responseStatus)
import           Network.Wreq

--------------------------------------------------------------------------------
import Web.Type

--------------------------------------------------------------------------------
controller :: AppEnv -> Request -> IO (Maybe Input)
controller env req
    | requestMethod req == methodGet
      = dispatch env req
    | otherwise
      = return Nothing

--------------------------------------------------------------------------------
dispatch :: AppEnv -> Request -> IO (Maybe Input)
dispatch env req
    = case pathInfo req of
          []                -> dispatchIndex env
          "article":name:[] -> dispatchGetArticle env name
          _                 -> return Nothing

--------------------------------------------------------------------------------
dispatchIndex :: AppEnv -> IO (Maybe Input)
dispatchIndex env = fmap (Just . Index) $ loadArticlesInfo env

--------------------------------------------------------------------------------
dispatchGetArticle :: AppEnv -> Text -> IO (Maybe Input)
dispatchGetArticle env name
    = do r <- asJSON =<< get (domain ++ "post/" ++ nameStr)
         let code = r ^. responseStatus . statusCode
             json = r ^. responseBody

         if code == 200
             then either
                  throwError
                  (return . Just . GetArticle . Just) $ extractArticle json
             else if code == 304
                  then return $ Just $ GetArticle Nothing
                  else throwError "post: Bad response from post repository"
  where
    domain  = envPostRepo env
    nameStr = unpack name

--------------------------------------------------------------------------------
loadArticlesInfo :: AppEnv -> IO (Vector ArticleInfo)
loadArticlesInfo env
    = do r <- asJSON =<< get (domain ++ "posts")
         let code = r ^. responseStatus . statusCode
             json = r ^. responseBody

         if code /= 200
             then throwError "summaries: Bad response from post repository"
             else either throwError return $ extractInfos json

  where
    domain = envPostRepo env

--------------------------------------------------------------------------------
-- Extractors
--------------------------------------------------------------------------------
extractInfos :: Value -> Either String (Vector ArticleInfo)
extractInfos value = parseEither infosParser value

--------------------------------------------------------------------------------
extractArticle :: Value -> Either String Article
extractArticle value = parseEither articleParser value

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------
infosParser :: Value -> Parser (Vector ArticleInfo)
infosParser (Array arr) = traverse infoParser arr
infosParser _           = mzero

--------------------------------------------------------------------------------
infoParser :: Value -> Parser ArticleInfo
infoParser (Object m)
    = liftM3 ArticleInfo (m .: "name") (m .: "title") (m .: "date")
infoParser _
    = mzero

--------------------------------------------------------------------------------
articleParser :: Value -> Parser Article
articleParser (Object m)
    = do v  <- m .: "info"
         i  <- infoParser v
         ec <- m .:"content"
         s  <- m .: "style"
         let content = pack $ Base64.decode $ unpack ec
         return $ Article i s content
articleParser _
    = mzero

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
throwError :: String -> IO a
throwError = ioError . userError
