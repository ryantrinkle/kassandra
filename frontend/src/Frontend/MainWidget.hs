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
import           Frontend.State                 ( StateProvider )
import           Frontend.Util                  ( tellNewTask )
import           Common.Debug                   ( logR
                                                , log
                                                , pattern I
                                                , pattern D
                                                )
import Data.Time.Clock
import System.IO.Unsafe

{-# NOINLINE blah #-}
blah :: R.Reflex t => R.Dynamic t UTCTime
blah = pure $ unsafePerformIO getCurrentTime

mainWidget :: WidgetIO t m => StateProvider t m -> m ()
mainWidget _ = do
  D.divClass "header" $ D.text "Kassandra Taskmanagement"
  log I "Loaded Mainwidget"
  time    <- liftIO getZonedTime
  timeDyn <-
    (logR D (const "timeTick"))
    =<< fmap
          (utcToZonedTime (zonedTimeZone time))
    <$> pure blah
  let filterState = R.constDyn (FilterState 0 60)
  event <- logR D (const "Click Event")
               =<<
              ("Click" <$)
               <$> D.button "Create1"
  countDyn <- R.count event
  D.dynText $ show <$> countDyn
  D.dynText $ show <$> timeDyn

  rec let (appChangeEvents, dataChangeEvents) =
            R.fanThese $ partitionEithersNE <$> stateChanges
      let taskState = R.constDyn mempty
      dragDyn <- R.holdDyn NoDrag $ last <$> appChangeEvents
      (_, stateChanges' :: R.Event t (NonEmpty AppStateChange)) <-
        R.runEventWriterT $ runReaderT
          (do
            tellNewTask
               =<< logR D (const "Creating Task2")
               =<< fmap (, id)
               <$> ("Click" <$)
               <$> D.button "Create2"
            D.divClass "container" $ do
              D.divClass "pane" (listWidget $ pure ())
              D.divClass "pane" (listWidget $ pure ())
              D.divClass "pane" (listWidget $ pure ())
              D.divClass "pane" (listWidget $ pure ())
              D.divClass "pane" (listWidget $ pure ())
              D.divClass "pane" (listWidget $ pure ())
              D.divClass "pane" (listWidget $ pure ())
          )
          (AppState taskState (R.constDyn time) dragDyn filterState)
      stateChanges <- pure $ R.traceEventWith (const "StateChange") stateChanges'
  D.divClass "footer"
     $ D.text
        "Powered by taskwarrior, Haskell and reflex-frp -- AGPL Licensed -- Malte Brandy -- 2019 - 2020"

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
