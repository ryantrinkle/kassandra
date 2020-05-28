{-# LANGUAGE PatternSynonyms #-}
module Frontend.MainWidget
  ( mainWidget
  )
where

import Prelude (MonadFix, NonEmpty, Either (..), const, pure, (<$), ($), pass, void, (.), fmap, last, (=<<), partitionEithersNE, (<$>), one)

import qualified Reflex.Dom                    as D
import qualified Reflex as R
import qualified Reflex.Network as R

mainWidget :: (D.DomBuilder t m, MonadFix m, R.PostBuild t m, R.MonadHold t m) => m ()
mainWidget = do
  rec let (appChangeEvents, _) =
            R.fanThese $ partitionEithersNE <$> stateChanges
      dragDyn <- R.holdDyn () $ last <$> appChangeEvents
      (_, stateChanges' :: R.Event t (NonEmpty (Either () ()))) <-
        R.runEventWriterT
          (do
            R.tellEvent . fmap one
               . fmap (\_ -> Right ()) . R.traceEventWith (const "Creating Task2")
               =<< D.button "Create2"
            void $ R.networkView $ (void $ R.networkView $ pass <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pass <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pass <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pass <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pass <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pass <$ dragDyn) <$ pure ()
            void $ R.networkView $ (void $ R.networkView $ pass <$ dragDyn) <$ pure ()
          )
      stateChanges <- pure $ R.traceEventWith (const "StateChange") stateChanges'
  pure ()
