
module Frontend.ListWidget
  ( TaskList(UUIDList, TagList)
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
