module BISocie.Helpers.Util 
       ( mkPagenate
       ) where

import Settings (pagenateWidth)

mkPagenate :: Int -> Int -> Int -> [[Int]]
mkPagenate current maxpage width =
  if leftConnected && rightConnected
  then [[ll..rr]]
  else if leftConnected
       then [[ll..cr], [rl..rr]]
       else if rightConnected
            then [[ll..lr],[cl..rr]]
            else [[ll..lr],[cl..cr],[rl..rr]]
  where
    leftConnected = cl-lr <= pagenateWidth
    rightConnected = rl-cr <= pagenateWidth
    ll = 0
    lr = width
    cl = current-width
    cr = current+width
    rl = maxpage-width
    rr = maxpage

