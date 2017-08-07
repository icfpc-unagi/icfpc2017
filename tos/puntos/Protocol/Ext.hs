module Protocol.Ext
  (
    riversFromMove
  )
  where

import Protocol as P

riversFromMove :: P.Move -> [P.River]
riversFromMove (P.MoveClaim p s t) = [River s t]
riversFromMove (P.MovePass p) = []
riversFromMove (P.MoveSplurge p r) = [River s t | (s, t) <- zip r (tail r)]
riversFromMove (P.MoveOption p s t) = [River s t]
