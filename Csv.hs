{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE DatatypeContexts, FlexibleInstances #-}
module Csv where

import Yesod
import Control.Arrow ((***))
import Data.Text (Text)
import qualified Data.Text as T

data C = C

mkYesod "C" [parseRoutes|
/ RootR GET
|]

instance Yesod C

newtype Show a => CSV a = CSV { unCsv :: ([Text], [[a]]) }
instance Show a => ToContent (CSV a) where
  toContent = toContent.trans
    where
      trans :: Show a => CSV a -> Text
      trans = T.unlines.uncurry (:).(xHead *** xBody).unCsv
      xBody :: Show a => [[a]] -> [Text]
      xBody = map (T.intercalate "," . map toText)
      xHead :: [Text] -> Text
      xHead = T.intercalate ","
      toText :: Show a => a -> Text
      toText = T.pack . show

newtype Show a => RepCsv a = RepCsv (CSV a)
instance Show a => HasReps (RepCsv a) where
  chooseRep (RepCsv c) _ = return (typeOctet, toContent c)


getRootR :: Handler (RepCsv Int)
getRootR = do
  download "test.csv" $ CSV (["a","b","c"], [[1,2,3],[4,5,6],[7,8,9]])

download :: Yesod m => Text -> CSV Int -> GHandler m s (RepCsv Int)
download fn rep = do
  setHeader "Content-Disposition" $ T.append "attachiment; filename=" fn
  return (RepCsv rep)

main :: IO ()
main = warpDebug 3000 C
