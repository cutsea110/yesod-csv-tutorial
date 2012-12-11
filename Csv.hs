{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
module Csv where

import Yesod

data C = C

mkYesod "C" [parseRoutes|
/ RootR GET
|]

instance Yesod C

newtype RepCsv = RepCsv Content
instance HasReps RepCsv where
  chooseRep (RepCsv c) _ = return (typeOctet, c)


getRootR :: Handler RepCsv
getRootR = do
  setHeader "Content-Disposition" "attachiment; filename=foo.csv"
  return $ RepCsv "a,b,c\n1,2,3\n4,5,6\n7,8,9\n"

main :: IO ()
main = warpDebug 3000 C
