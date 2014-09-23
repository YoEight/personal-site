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
import Data.Configurator
import Data.Pool
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Routing

--------------------------------------------------------------------------------
main :: IO ()
main = run 3000 (route $ prepare routes)

--------------------------------------------------------------------------------
routes :: Routes a IO ()
routes
    = do get "push-event/:repo" (updateDB db) $ capture ":repo"

--------------------------------------------------------------------------------
updateDB :: String -> String -> Continue IO -> IO ResponseReceived
updateDB database = undefined
