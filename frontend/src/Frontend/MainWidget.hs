{-# LANGUAGE PatternSynonyms #-}
module Frontend.MainWidget
  ( mainWidget
  )
where

import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Frontend.Types                 ( DragState(NoDrag)
                                                , AppState(AppState)
                                                , FilterState(FilterState)
                                                , AppStateChange
                                                , WidgetIO
                                                , StandardWidget
                                                , DataChange
                                                )

mainWidget :: WidgetIO t m => m ()
mainWidget = do
  time    <- liftIO getZonedTime

  rec let (appChangeEvents, _) =
            R.fanThese $ partitionEithersNE <$> stateChanges
      dragDyn <- R.holdDyn NoDrag $ last <$> appChangeEvents
      (_, stateChanges' :: R.Event t (NonEmpty AppStateChange)) <-
        R.runEventWriterT
          (do
            R.tellEvent . fmap one
               . fmap (_Typed @AppStateChange % _Typed @DataChange % #_CreateTask #) . R.traceEventWith (const "Creating Task2")
               =<< fmap (, id)
               <$> ("Click" <$)
               <$> D.button "Create2"
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
