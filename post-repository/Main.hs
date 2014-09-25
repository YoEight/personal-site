{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
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
import Data.Aeson
import Data.Configurator
import Data.Default (def)
import Data.Predicate
import Data.Text (Text)
import Database.SQLite.Simple hiding (query)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Predicate hiding (def)
import Network.Wai.Routing
import System.Log.FastLogger (newFileLoggerSet)

--------------------------------------------------------------------------------
import Web.DB

--------------------------------------------------------------------------------
type DBName = String

--------------------------------------------------------------------------------
main :: IO ()
main = do cfg        <- load [Required "config/site.cfg"]
          dbname     <- require cfg "db_name"
          port       <- require cfg "site_port"
          logFile    <- require cfg "log_file"
          logBufSize <- require cfg "log_buffer"
          logSet     <- newFileLoggerSet logBufSize logFile
          mware      <- mkRequestLogger def
                                        { outputFormat = Detailed True
                                        , destination  = Logger logSet
                                        }

          run port $ mware (route $ prepare $ routes dbname)

--------------------------------------------------------------------------------
routes :: DBName -> Routes a IO ()
routes db
    = do get "posts"      (getPosts db) $ constant ()
         get "post/:name" (getPost db)  $ capture ":name" .&. query "etag"

--------------------------------------------------------------------------------
getPosts :: DBName -> ignore -> Continue IO -> IO ResponseReceived
getPosts db _ respond
    = withConnection db $ \con ->
          do posts <- retrievePosts con
             let bytes   = encode $ fmap postInfoJSON posts
                 headers = [(hContentType, "application/json")]

             respond $ responseLBS status200 headers bytes

--------------------------------------------------------------------------------
getPost :: DBName -> (PostName ::: Etag) -> Continue IO -> IO ResponseReceived
getPost db (name ::: etag) respond
    = withConnection db $ \con ->
          do rE <- retrievePost name etag con
             case rE of
                 Left s
                     | NotFound <- s -> respond $ responseLBS status404 [] ""
                     | NoChange <- s -> respond $ responseLBS status304 [] ""
                 Right post
                     -> let bytes   = encode $ postJSON post
                            headers = [(hContentType, "application/json")] in
                        respond $ responseLBS status200 headers bytes

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------
postInfoJSON :: PostInfo -> Value
postInfoJSON info
    = object [ "name"  .= postName info
             , "title" .= postTitle info
             , "etag"  .= postEtag info
             , "date"  .= postDate info
             ]

--------------------------------------------------------------------------------
postJSON :: Post -> Value
postJSON post
    = object [ "info"    .= postInfoJSON (postInfo post)
             , "style"   .= postStyle post
             , "content" .= postContent post
             ]
