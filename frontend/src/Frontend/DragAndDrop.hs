{-# LANGUAGE PatternSynonyms #-}


module Frontend.DragAndDrop
  ( droppableElementConfig
  , childDropArea
  , taskDropArea
  , tellDragTask
  )
where

import qualified Reflex                        as R
import qualified Reflex.Dom                    as D
import           Data.Proxy                     ( Proxy(Proxy) )
import           Frontend.Sorting               ( SortPosition
                                                , saveSorting
                                                )
import           Frontend.Types                 ( AppStateChange
                                                , StandardWidget
                                                , TaskInfos
                                                , DragState(DraggedTask, NoDrag)
                                                , getTasks
                                                , getDragState
                                                , DataChange
                                                , WriteApp
                                                , TaskState
                                                )
import           Frontend.Util                  ( tellSingleton
                                                , lookupTask
                                                )
import Common.Debug (logRShow, pattern I)

tellDragTask :: (MonadIO m, WriteApp t m e) => R.Event t (Maybe UUID) -> m ()
tellDragTask = tellSingleton . fmap
  ((_Typed @AppStateChange % _Typed @DragState #) . maybe NoDrag DraggedTask) <=< logRShow I

taskDropArea
  :: forall t m r e
  .  StandardWidget t m r e
  => R.Dynamic t [UUID]
  -> m ()
  -> (R.Event t TaskInfos -> R.Event t [Task])
  -> m ()
taskDropArea blacklistD areaW handler = do
  tasksD :: D.Dynamic t TaskState <- getTasks
  dragStateD <- getDragState
  let dropActive = fmap (\_ -> ()) dragStateD
  D.dyn_ $ dropActive <&> const pass

childDropArea
  :: StandardWidget t m r e
  => SortPosition t
  -> R.Dynamic t [UUID]
  -> m ()
  -> m ()
childDropArea pos blacklistD areaW =
  taskDropArea blacklistD areaW
    $ const R.never


droppableElementConfig
  :: forall s d
   . (R.Reflex s, D.DomSpace d)
  => (D.ElementConfig D.EventResult s d)
droppableElementConfig =
  lensVL D.elementConfig_eventSpec
    %~ D.addEventSpecFlags (Proxy :: Proxy d)
                           D.Dragover
                           (const D.preventDefault)
    $  (D.def :: (D.ElementConfig D.EventResult s d))
