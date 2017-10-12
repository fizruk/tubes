module Tubes.Model.Action where

import Tubes.Model.Tube

data Missing a = Missing

newtype Present a = Present a

data Action f
  = StartNewLine Point (f Point)
  | ContinueLine TubeLineId TubeLineDirection Point (f Point)

type IncompleteAction = Action Missing
type CompleteAction = Action Present

startAction :: Point -> Tube -> Maybe IncompleteAction
startAction from tube
  = case pointToStation from tube of
      Just s  -> Just (StartNewLine s Missing)
      Nothing -> case pointToTubeLineEnd from tube of
        ((s, tubeLineId, dir):_) -> Just (ContinueLine tubeLineId dir s Missing)
        _ -> Nothing

completeAction :: Point -> IncompleteAction -> Tube -> Maybe CompleteAction
completeAction point action tube
  = case pointToStation point tube of
      Nothing -> Nothing
      Just to -> case action of
        StartNewLine from _
          | nearStation from to -> Nothing
          | otherwise -> Just (StartNewLine from (Present to))
        ContinueLine tubeLineId dir from _
          | nearStation from to -> Nothing
          | otherwise -> Just (ContinueLine tubeLineId dir from (Present to))

handleAction :: CompleteAction -> Tube -> Tube
handleAction (StartNewLine from (Present to))
  | nearStation from to = id
  | otherwise
    = addTubeLine from to
    . addStation from
    . addStation to
handleAction (ContinueLine tubeLineId dir from (Present to))
  | nearStation from to = id
  | otherwise
    = continueLine
    . addStation to
  where
    continueLine = case dir of
      Forward  -> modifyTubeLine tubeLineId (appendTubeLineSegment  to)
      Backward -> modifyTubeLine tubeLineId (prependTubeLineSegment to)

