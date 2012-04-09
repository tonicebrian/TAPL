module TAPL.Support where

data InfoS = InfoS String Int Int deriving Show

type Info = Maybe InfoS

dummyInfo = Just $ InfoS "dummy" 0 0