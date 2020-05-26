{-# LANGUAGE PatternSynonyms #-}
module Frontend.MainWidget
  ( mainWidget
  )
where

import Control.Concurrent (threadDelay)
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Data.HashMap.Strict           as HashMap
import           Frontend.Types                 ( DragState(NoDrag)
                                                , AppState(AppState)
                                                , FilterState(FilterState)
                                                , getTasks
                                                , TaskInfos
                                                , AppStateChange
                                                , WidgetIO
                                                , StandardWidget
                                                , TaskState
                                                )
import           Frontend.ListWidget            ( listsWidget
                                                , listWidget
                                                , TaskList(TagList)
                                                )
import           Frontend.State                 ( StateProvider )
import           Frontend.TaskWidget            ( taskTreeWidget )
import           Frontend.TextEditWidget        ( createTextWidget )
import           Frontend.BaseWidgets           ( button )
import           Frontend.Util                  ( tellNewTask )
import           Common.Debug                   ( logR
                                                , logRShow
                                                , log
                                                , pattern I
                                                , pattern D
                                                )

mainWidget :: WidgetIO t m => StateProvider t m -> m ()
mainWidget stateProvider = do
  D.divClass "header" $ D.text "Kassandra Taskmanagement"
  log I "Loaded Mainwidget"
  time    <- liftIO getZonedTime
  timeDyn <-
    (logR D (const "timeTick"))
    =<< fmap
          (utcToZonedTime (zonedTimeZone time) . (^. lensVL R.tickInfo_lastUTC))
    <$> R.clockLossy 1 (zonedTimeToUTC time)
  let filterState = R.constDyn (FilterState 0 60)
--  (_,event) <- R.runEventWriterT $ do
--    R.tellEvent =<<
  event <- logR D (const "Click Event")
               =<<
              ("Click" <$)
               <$> D.button "Create"
  countDyn <- R.count event
  D.dynText $ show <$> countDyn
  D.dynText $ show <$> timeDyn

  rec let (appChangeEvents, dataChangeEvents) =
            R.fanThese $ partitionEithersNE <$> stateChanges
      --taskState <- stateProvider dataChangeEvents
      let taskState = R.constDyn mempty
      dragDyn <- R.holdDyn NoDrag $ last <$> appChangeEvents
      (_, stateChanges' :: R.Event t (NonEmpty AppStateChange)) <-
        R.runEventWriterT $ runReaderT
          (do
            tellNewTask
               =<< logR D (const "Creating Task")
               =<< fmap (, id)
               <$> ("Click" <$)
               <$> D.button "Create"
            taskDiagnosticsWidget
            D.divClass "container" $ do
              D.divClass "pane" widgetSwitcher
              D.divClass "pane" (listWidget $ R.constDyn (TagList "root"))
          )
          (AppState taskState (R.constDyn time) dragDyn filterState)
      stateChanges <- logR I (const "StateChange") stateChanges'
  D.divClass "footer"
     $ D.text
        "Powered by taskwarrior, Haskell and reflex-frp -- AGPL Licensed -- Malte Brandy -- 2019 - 2020"

taskDiagnosticsWidget :: (StandardWidget t m r e) => m ()
taskDiagnosticsWidget = do
  tasks <- getTasks
  D.dynText $ do
    tasksMap <- tasks
    let uuids = HashMap.keys tasksMap
        hasLoop :: [UUID] -> UUID -> Maybe UUID
        hasLoop seen new | new `elem` seen = Just new
                         | otherwise = firstJust (hasLoop (new : seen)) nexts
          where nexts = maybe [] (^. #children) $ HashMap.lookup new tasksMap
    pure $ firstJust (hasLoop []) uuids & \case
      Just uuid -> "Found a loop for uuid " <> show uuid
      Nothing   -> "" -- everything fine

widgets :: StandardWidget t m r e => [(Text, m ())]
widgets =
  [ ("Next"    , nextWidget)
  , ("Lists"   , listsWidget)
  , ("Inbox"   , inboxWidget)
  , ("Unsorted", unsortedWidget)
  ]

widgetSwitcher :: forall t m r e . StandardWidget t m r e => m ()
widgetSwitcher = D.el "div" $ do
  tellNewTask
    =<< logR D (const "Creating Task")
    =<< fmap (, id)
    <$> ("Click" <$)
    <$> D.button "Create"
  tellNewTask
    =<< logR D (const "Creating Task")
    =<< fmap (, id)
    <$> createTextWidget (button "selector" $ D.text "New Task")
  buttons <- forM (widgets @t @m) $ \l ->
    (l <$) . D.domEvent D.Click . fst <$> D.elClass' "a"
                                                     "selector"
                                                     (D.text $ fst l)
  listName <- R.holdDyn ("No list", pass) (R.leftmost buttons)
  D.el "div" $ D.dyn_ (snd <$> listName)

filterInbox :: TaskState -> [TaskInfos]
filterInbox tasks =
  sortOn (^. #modified) . filter inInbox . HashMap.elems $ tasks
 where
  inInbox :: TaskInfos -> Bool
  inInbox taskInfos =
    has (#tags % _Empty) taskInfos
      && has (#status % #_Pending) taskInfos
      && has (#children % _Empty)  taskInfos
      && (  not
         .  any (`notElem` ["kategorie", "project", "root"])
         .  join
         $  lookupTasks tasks (taskInfos ^. #parents)
         ^. #tags
         )
      && not (taskInfos ^. #blocked)

lookupTasks :: TaskState -> [UUID] -> [TaskInfos]
lookupTasks tasks = mapMaybe (\uuid -> tasks ^. at uuid)

nextWidget :: (StandardWidget t m r e) => m ()
nextWidget = do
  inboxTasks <- fmap filterInbox <$> getTasks
  D.dynText
    $   (\x -> "There are " <> show (length x) <> " tasks in the inbox.")
    <$> inboxTasks
  void . flip R.simpleList taskTreeWidget $ take 1 <$> inboxTasks

inboxWidget :: (StandardWidget t m r e) => m ()
inboxWidget = do
  inboxTasks <- fmap filterInbox <$> getTasks
  D.dynText
    $   (\x -> "There are " <> show (length x) <> " tasks in the inbox.")
    <$> inboxTasks
  void . flip R.simpleList taskTreeWidget $ inboxTasks

unsortedWidget :: (StandardWidget t m r e) => m ()
unsortedWidget = do
  unsortedTasks <-
    fmap
        ( filter
            (\task ->
              "root"
                `notElem` (task ^. #tags)
                &&        has (#partof % _Nothing)  task
                &&        has (#status % #_Pending) task
            )
        . HashMap.elems
        )
      <$> getTasks
  D.dynText
    $   (\x -> "There are " <> show (length x) <> " unsorted tasks.")
    <$> unsortedTasks
  void . flip R.simpleList taskTreeWidget $ unsortedTasks
