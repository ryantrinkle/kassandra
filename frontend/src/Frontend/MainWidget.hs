{-# LANGUAGE PatternSynonyms #-}
module Frontend.MainWidget
  ( mainWidget
  )
where

import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Frontend.Types                 ( DragState(NoDrag)
                                                )

mainWidget :: (D.DomBuilder t m, MonadFix m, R.PostBuild t m, R.MonadHold t m) => m ()
mainWidget = do
  rec let (appChangeEvents, _) =
            R.fanThese $ partitionEithersNE <$> stateChanges
      dragDyn <- R.holdDyn NoDrag $ last <$> appChangeEvents
      (_, stateChanges' :: R.Event t (NonEmpty (Either DragState ()))) <-
        R.runEventWriterT
          (do
            R.tellEvent . fmap one
               . fmap (\_ -> Right ()) . R.traceEventWith (const "Creating Task2")
               =<< D.button "Create2"
            D.dyn_ ((D.dyn_ $ pass <$ dragDyn) <$ pure ())
            D.dyn_ ((D.dyn_ $ pass <$ dragDyn) <$ pure ())
            D.dyn_ ((D.dyn_ $ pass <$ dragDyn) <$ pure ())
            D.dyn_ ((D.dyn_ $ pass <$ dragDyn) <$ pure ())
            D.dyn_ ((D.dyn_ $ pass <$ dragDyn) <$ pure ())
            D.dyn_ ((D.dyn_ $ pass <$ dragDyn) <$ pure ())
            D.dyn_ ((D.dyn_ $ pass <$ dragDyn) <$ pure ())
          )
      stateChanges <- pure $ R.traceEventWith (const "StateChange") stateChanges'
  pure ()
