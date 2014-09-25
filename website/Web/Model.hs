{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module : Web.Model
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Web.Model where

--------------------------------------------------------------------------------
import Control.Monad.State

--------------------------------------------------------------------------------
import Web.Type

--------------------------------------------------------------------------------
model :: MonadState AppState m => Input -> m Output
model (Index as)     = return $ ArticleList as
model (GetArticle m) = return $ ArticleView m
