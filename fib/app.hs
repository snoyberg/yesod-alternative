-- | Simple web app showing the fib numbers. Store results in acid state,
-- dispatches uses combinators, and renders HTML via Heist.
{-# LANGUAGE OverloadedStrings #-}
import Yesod.Combinator
import Yesod.Heist
import qualified Data.Text as T
import FibState

main :: IO ()
main = do
    fibState <- initFib
    ts <- loadTemplates "fib/templates"

    warp 3000 $ do
        handler $ homepage ts
        static "fib" $ do
            dynamic $ \i -> do
                handler $ fibHandler fibState ts i

homepage :: HeistState -> Handler RepHtml
homepage ts = defaultLayout $ heistToWidget ts "index" []

fibHandler :: AcidState FibState -> HeistState -> Int -> Handler RepHtml
fibHandler fibState ts i = do
    x <- getFib fibState i
    let toText = T.pack . show
    defaultLayout $ heistToWidget ts "fib"
        [ ("next", toText $ i + 1)
        , ("input", toText i)
        , ("output", toText x)
        ]
