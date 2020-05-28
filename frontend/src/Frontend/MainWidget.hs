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
                                                , getDragState
                                                )
import           Frontend.Util                  ( tellNewTask )
import           Common.Debug                   ( logR
                                                , pattern D
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
            tellNewTask
               =<< logR D (const "Creating Task2")
               =<< fmap (, id)
               <$> ("Click" <$)
               <$> D.button "Create2"
            listWidget $ pure ()
            listWidget $ pure ()
            listWidget $ pure ()
            listWidget $ pure ()
            listWidget $ pure ()
            listWidget $ pure ()
            listWidget $ pure ()
          )
          (AppState (pure mempty) (pure time) dragDyn (pure (FilterState 0 60)))
      stateChanges <- pure $ R.traceEventWith (const "StateChange") stateChanges'
  pure ()

listWidget
  :: forall t m r e . StandardWidget t m r e => R.Dynamic t () -> m ()
listWidget list = D.dyn_ (innerRenderList <$ list)
 where
  innerRenderList :: m ()
  innerRenderList
    = do
      dragStateD <- getDragState
      let dropActive = fmap (\_ -> ()) dragStateD
      D.dyn_ $ dropActive <&> const pass
