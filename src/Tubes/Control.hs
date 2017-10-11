module Tubes.Control where

import Tubes.Model

-- | Append a new segment to the end of the line.
appendTubeLineSegment :: Point -> TubeLine -> TubeLine
appendTubeLineSegment point tubeLine = tubeLine
  { tubeLineSegments = appendSegment (tubeLineSegments tubeLine) }
  where
    appendSegment []  = []
    appendSegment [segment] = [segment, Segment (segmentEnd segment) point]
    appendSegment (x:xs) = x : appendSegment xs

-- | Prepend a new segment to the beginning of the line.
prependTubeLineSegment :: Point -> TubeLine -> TubeLine
prependTubeLineSegment point tubeLine = tubeLine
  { tubeLineSegments = prependSegment (tubeLineSegments tubeLine)
  , tubeLineTrains   = map (updateStationIndices (+1)) (tubeLineTrains tubeLine)
  }
  where
    prependSegment []  = []
    prependSegment segments@(segment : _) = Segment point (segmentStart segment) : segments

    updateStationIndices f train = train
      { trainFrom = f (trainFrom train)
      , trainTo   = f (trainTo train)
      }

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
        ((tubeLineId, dir):_) -> Just (ContinueLine tubeLineId dir from Missing)
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
  = addTubeLine from to
  . addStation from
  . addStation to
handleAction (ContinueLine tubeLineId dir _ (Present to))
  = continueLine
  . addStation to
  where
    continueLine = case dir of
      Forward  -> modifyTubeLine tubeLineId (appendTubeLineSegment  to)
      Backward -> modifyTubeLine tubeLineId (prependTubeLineSegment to)
