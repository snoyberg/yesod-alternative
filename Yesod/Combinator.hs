{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
-- | An alternate to type-safe URLs: combinator based routing.
--
-- Since our code still lives in the standard @Handler@ monad, we have full
-- access to most features of Yesod, including widgets. The combinators
-- provided here are not intended to be comprehensive, only to give an idea of
-- what's possible. Dispatch is also abysmally inefficient.
module Yesod.Combinator
    ( Handler
    , Widget
    , warp
    , handler
    , static
    , dynamic
    , CombinatorApp
    , module Yesod.Core
    ) where

import Yesod.Core
import Data.Text (Text)
import Control.Monad.Trans.Writer (tell, execWriter, Writer)
import Data.Maybe (listToMaybe, catMaybes)
import Network.Wai.Handler.Warp (run)

type Handler = GHandler CombinatorApp CombinatorApp
type Widget = GWidget CombinatorApp CombinatorApp ()

newtype CombinatorApp = CombinatorApp
    { runCombinatorApp :: [Text] -> Maybe (Handler ChooseRep)
    }

instance Yesod CombinatorApp

instance RenderRoute CombinatorApp where
    newtype Route CombinatorApp = CombinatorAppRoute [Text]
        deriving (Show, Eq, Read)
    renderRoute (CombinatorAppRoute ts) = (ts, [])

instance YesodDispatch CombinatorApp CombinatorApp where
    yesodDispatch master sub toMaster h404 _ _ pieces session req =
        case runCombinatorApp sub pieces of
            Nothing -> h404 session req
            Just h -> yesodRunner
                h
                master
                sub
                (Just $ CombinatorAppRoute pieces)
                toMaster
                session
                req

handler :: HasReps r => Handler r -> DispatchBuilder ()
handler h =
    tell [f]
  where
    f [] = Just $ fmap chooseRep h
    f _ = Nothing

static :: Text -> DispatchBuilder () -> DispatchBuilder ()
static t d =
    tell [f]
  where
    f (t':ts) | t == t' = runDispatch d ts
    f _ = Nothing

dynamic :: PathPiece p => (p -> DispatchBuilder ()) -> DispatchBuilder ()
dynamic d =
    tell [f]
  where
    f (t:ts) | Just p <- fromPathPiece t = runDispatch (d p) ts
    f _ = Nothing

runDispatch :: DispatchBuilder () -> [Text] -> Maybe (Handler ChooseRep)
runDispatch db ts = listToMaybe $ catMaybes $ map ($ ts) (execWriter db)

type Dispatch = [[Text] -> Maybe (Handler ChooseRep)]

type DispatchBuilder = Writer Dispatch

combinatorApp :: DispatchBuilder () -> CombinatorApp
combinatorApp = CombinatorApp . runDispatch

warp :: Int -> DispatchBuilder () -> IO ()
warp port db = toWaiApp (combinatorApp db) >>= run port
