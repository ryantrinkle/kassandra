
module Frontend.ListWidget
  ( listsWidget
  , listWidget
  , TaskList(UUIDList, TagList)
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet
import           Frontend.Types                 ( al
                                                , StandardWidget
                                                , TaskInfos
                                                , TaskState
                                                , Widget
                                                , getTasks
                                                , getDragState
                                                )
import           Frontend.Util                  ( filterCurrent )
import           Frontend.TaskWidget            ( taskList
                                                , taskTreeWidget
                                                )
import           Frontend.Sorting               ( sortTasks
                                                , SortMode(SortModeTag)
                                                )

data TaskList = TagList Text | SubList [TaskList] | UUIDList [UUID] deriving (Eq, Show, Read)

listsWidget :: (StandardWidget t m r e) => m ()
listsWidget = do
  taskState <- getTasks
  D.text "Select a list"
  list <- listSelector (getLists <$> taskState)
  listWidget list
 where
  getLists :: TaskState -> [TaskList]
  getLists =
    fmap TagList
      . HashSet.toList
      . fold
      . fmap (^. (#tags % to HashSet.fromList))
      . filter (has $ #status % #_Pending)
      . (^. al #task)
      . HashMap.elems
  listSelector
    :: (Widget t m) => R.Dynamic t [TaskList] -> m (R.Dynamic t TaskList)
  listSelector lists = D.el "div" $ do
    buttons   <- D.dyn $ mapM listButton <$> lists
    buttonSum <- R.switchHold R.never $ R.leftmost <$> buttons
    R.holdDyn (SubList []) buttonSum
  listButton :: (Widget t m) => TaskList -> m (R.Event t TaskList)
  listButton list | TagList tag <- list = button tag
                  | otherwise           = button "Anonymous List"
   where
    button =
      fmap ((list <$) . D.domEvent D.Click . fst)
        . D.elClass' "a" "selector"
        . D.text

listWidget
  :: forall t m r e . StandardWidget t m r e => R.Dynamic t TaskList -> m ()
listWidget list = D.dyn_ (innerRenderList <$ list)
 where
  innerRenderList :: m ()
  innerRenderList
    = do
      dragStateD <- getDragState
      let dropActive = fmap (\_ -> ()) dragStateD
      D.dyn_ $ dropActive <&> const pass
