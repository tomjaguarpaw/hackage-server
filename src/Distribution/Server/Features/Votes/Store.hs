module Distribution.Server.Features.Votes.Store
  ( votesScore
  ) where

import Distribution.Server.Features.Votes.Types
import Distribution.Server.Users.Types (UserId)

import Control.Arrow ((&&&))
import Data.List (group, sort)
import Data.Map (Map)
import qualified Data.Map as Map

-- Using a Bayesian average (m=1.5, C=2) to calculate scoring
votesScore :: Map UserId Score -> Float
votesScore m =
     let grouping = map (head &&& length) . group . sort . Map.elems $ m
         score :: Float
         score = fromIntegral ((sum $ map (uncurry (*)) grouping) + 3)/
                 fromIntegral (2 + sum (map snd grouping))
         roundedScore = fromIntegral (round (score * 4) :: Int) / 4
     in roundedScore
