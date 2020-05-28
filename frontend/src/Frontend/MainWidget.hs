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
        R.runEventWriterT $ runReaderT
          (do
            R.tellEvent . fmap one
               . fmap (_Typed @AppStateChange % _Typed @DataChange % #_CreateTask #) . R.traceEventWith (const "Creating Task2")
               =<< fmap (, id)
               <$> ("Click" <$)
               <$> D.button "Create2"
            listWidget dragDyn $ pure ()
            listWidget dragDyn $ pure ()
            listWidget dragDyn $ pure ()
            listWidget dragDyn $ pure ()
            listWidget dragDyn $ pure ()
            listWidget dragDyn $ pure ()
            listWidget dragDyn $ pure ()
          )
          (AppState (pure mempty) (pure time) dragDyn (pure (FilterState 0 60)))
      stateChanges <- pure $ R.traceEventWith (const "StateChange") stateChanges'
  pure ()

listWidget
  :: forall t m r e . StandardWidget t m r e => R.Dynamic t DragState -> R.Dynamic t () -> m ()
listWidget dragStateD list = D.dyn_ ((D.dyn_ $ pass <$ dragStateD) <$ list)
