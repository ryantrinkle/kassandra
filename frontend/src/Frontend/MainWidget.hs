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
                                                , WriteApp
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
            tellNewTask . R.traceEventWith (const "Creating Task2")
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

tellNewTask :: WriteApp t m e => R.Event t (Text, Task -> Task) -> m ()
tellNewTask = tellSingleton
  . fmap (_Typed @AppStateChange % _Typed @DataChange % #_CreateTask #)

tellSingleton
  :: (R.Reflex t, R.EventWriter t (NonEmpty event) m) => R.Event t event -> m ()
tellSingleton = R.tellEvent . fmap one
