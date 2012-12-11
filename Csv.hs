{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE DatatypeContexts, FlexibleInstances #-}
module Csv where

import Yesod
import Data.Text (Text)
import qualified Data.Text as T

data C = C

mkYesod "C" [parseRoutes|
/ RootR GET
|]

instance Yesod C

newtype Show a => CSV a = CSV { unCsv :: [[a]] }
instance Show a => ToContent (CSV a) where
  toContent = trans.unCsv
    where
      trans = toContent . T.unlines . map (T.intercalate "," . map toText)
      toText = T.pack . show

newtype Show a => RepCsv a = RepCsv (CSV a)
instance Show a => HasReps (RepCsv a) where
  chooseRep (RepCsv c) _ = return (typeOctet, toContent c)


getRootR :: Handler (RepCsv Int)
getRootR = do
  download "test.csv" $ CSV [[1,2,3],[4,5,6],[7,8,9]]

download :: Yesod m => Text -> CSV Int -> GHandler m s (RepCsv Int)
download fn rep = do
  setHeader "Content-Disposition" $ T.append "attachiment; filename=" fn
  return (RepCsv rep)

main :: IO ()
main = warpDebug 3000 C
