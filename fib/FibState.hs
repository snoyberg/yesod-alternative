{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
-- | Store fib numbers in acid-state.
module FibState
    ( AcidState
    , FibState
    , getFib
    , initFib
    ) where

import Data.Acid
import Control.Monad.State
import Data.Typeable (Typeable)
import Data.SafeCopy
import qualified Data.Map as Map

data FibState = FibState !(Map.Map Int Int)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''FibState)

fibUpdate :: Int -> Update FibState Int
fibUpdate 0 = return 1
fibUpdate 1 = return 1
fibUpdate i = do
    FibState m <- get
    case Map.lookup i m of
        Just x -> return x
        Nothing -> do
            y <- fibUpdate $ i - 1
            z <- fibUpdate $ i - 2
            let x = y + z
            put $ FibState $ Map.insert i x m
            return x

$(makeAcidic ''FibState ['fibUpdate])

getFib :: MonadIO m => AcidState FibState -> Int -> m Int
getFib fibState i = liftIO $ update fibState $ FibUpdate i

initFib :: IO (AcidState FibState)
initFib = openLocalState $ FibState Map.empty
