{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.App
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.App where

--------------------------------------------------------------------------------
import Control.Concurrent.STM

--------------------------------------------------------------------------------
import Control.Monad.State.Strict
import Data.Configurator
import Data.Default (def)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger (newFileLoggerSet)

--------------------------------------------------------------------------------
import Web.Controller
import Web.Model
import Web.View
import Web.Type

--------------------------------------------------------------------------------
newtype App = App (IO ())

--------------------------------------------------------------------------------
runApp :: App -> IO ()
runApp (App action) = action

--------------------------------------------------------------------------------
myApp :: App
myApp
    = App $
        do cfg         <- load [Required "config/site.cfg"]
           port        <- require cfg "site_port"
           logFile     <- require cfg "log_file"
           logBufSize  <- require cfg "log_buffer"
           logSet      <- newFileLoggerSet logBufSize logFile
           mware       <- mkRequestLogger def
                                          { outputFormat = Detailed True
                                          , destination  = Logger logSet
                                          }

           putStrLn "Yo Eight's personal website !"
           putStrLn ("Listening port " ++ show port)

           varState <- newTMVarIO AppState
           run port $ mware $ application varState

--------------------------------------------------------------------------------
application :: TMVar AppState -> Application
application varState request respond
    = controller request >>=
          maybe (send404 respond) (handleInput varState respond)

--------------------------------------------------------------------------------
handleInput :: TMVar AppState -> Respond -> Input -> IO ResponseReceived
handleInput varState respond input
    =  getOutput varState input >>= view respond

--------------------------------------------------------------------------------
getOutput :: TMVar AppState -> Input -> IO Output
getOutput varState input
    = atomically $
          do s <- takeTMVar varState
             let (output, s') = runState (model input) s
             putTMVar varState s'
             return output

--------------------------------------------------------------------------------
send404 :: Respond -> IO ResponseReceived
send404 respond = view respond NotFound
