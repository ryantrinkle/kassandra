{-# LANGUAGE PatternSynonyms #-}
module Frontend.MainWidget
  ( mainWidget
  )
where

import Prelude ( const
               , pure
               , (<$)
               , ($)
               , void
               , (.)
               , fmap
               , last
               , (=<<)
               , (<$>)
               )

import qualified Reflex.Dom                    as D
import qualified Reflex as R
import qualified Reflex.Network as R
import Data.List.NonEmpty (NonEmpty (..))
import Data.These (These (..))
import Data.Either (Either (..), partitionEithers)
import Control.Monad.Fix (MonadFix)

mainWidget :: (D.DomBuilder t m, MonadFix m, R.PostBuild t m, R.MonadHold t m) => m ()
mainWidget = do
  rec let (appChangeEvents, _) =
            R.fanThese $ partitionEithersNE <$> stateChanges
      dragDyn <- R.holdDyn () $ last <$> appChangeEvents
      (_, stateChanges' :: R.Event t (NonEmpty (Either () ()))) <-
        R.runEventWriterT
          (do
            R.tellEvent . fmap (:| [])
               . fmap (\_ -> Right ()) . R.traceEventWith (const "Creating Task2")
               =<< D.button "Create2"
            void $ R.networkView $ (void $ R.networkView $ pure () <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pure () <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pure () <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pure () <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pure () <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pure () <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pure () <$ dragDyn) <$ pure ()
          )
      stateChanges <- pure $ R.traceEventWith (const "StateChange") stateChanges'
  pure ()

partitionEithersNE :: NonEmpty (Either a b) -> These (NonEmpty a) (NonEmpty b)
partitionEithersNE (x :| xs) = case (x, ls, rs) of
  (Left  y, ys    , []    ) -> This (y :| ys)
  (Left  y, ys    , z : zs) -> These (y :| ys) (z :| zs)
  (Right z, []    , zs    ) -> That (z :| zs)
  (Right z, y : ys, zs    ) -> These (y :| ys) (z :| zs)
  where (ls, rs) = partitionEithers xs
