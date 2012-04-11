-- | Helper functions for using Heist from Yesod.
--
-- I'm particularly inexperienced at Heist, and this might make no sense.
-- Review is definitely welcome!
module Yesod.Heist
    ( loadTemplates
    , heistToWidget
    , HeistState
    ) where

import qualified Text.Templating.Heist as H
import Blaze.ByteString.Builder (toByteString)
import Text.Blaze (unsafeByteString)
import Yesod.Widget (GWidget, toWidget)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Yesod.Combinator (Widget, CombinatorApp)
import Data.Text (Text)
import Data.ByteString (ByteString)

loadTemplates :: MonadIO m => FilePath -> m HeistState
loadTemplates folder = do
    ets <- liftIO $ H.loadTemplates folder H.defaultHeistState
    either error return ets

heistToWidget :: HeistState -> ByteString -> [(Text, Text)] -> Widget
heistToWidget ts name args = do
    Just (builder, _) <- H.renderWithArgs args ts name
    toWidget $ unsafeByteString $ toByteString builder

type HeistState = H.HeistState (GWidget CombinatorApp CombinatorApp)
