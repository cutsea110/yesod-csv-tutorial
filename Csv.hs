{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, GADTs #-}
module Csv where

import Yesod
import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Data.Text (Text)
import qualified Data.Text as T

data C = C

mkYesod "C" [parseRoutes|
/ RootR GET
/download/#Text DownloadR GET
|]

instance Yesod C
instance RenderMessage C FormMessage where
  renderMessage _ _ = defaultFormMessage

newtype CSV a where
  CSV :: ([Text],[[a]]) -> CSV a
unCsv :: CSV a -> ([Text],[[a]])
unCsv (CSV x) = x
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

newtype RepCsv a where
  RepCsv :: (CSV a) -> RepCsv a
instance Show a => HasReps (RepCsv a) where
  chooseRep (RepCsv c) _ = return (typeOctet, toContent c)

data F = F {name::Text}
type Form x = Html -> MForm C C (FormResult x, Widget)
sampleForm :: Form F
sampleForm = renderDivs $ F <$> areq textField "file name" Nothing

getRootR :: Handler RepHtml
getRootR = do
  ((r, w), e) <- runFormGet sampleForm
  case r of
    FormSuccess x -> redirect (DownloadR $ name x)
    _ -> defaultLayout [whamlet|
<form action=@{RootR} enctype=#{e}>
  ^{w}
  <input type=submit>
|]

getDownloadR :: Text -> Handler (RepCsv Int)
getDownloadR fn = do
  download (fn `T.append` ".csv") $ CSV (["a","b","c"], [[1,2,3],[4,5,6],[7,8,9]])

download :: Yesod m => Text -> CSV Int -> GHandler m s (RepCsv Int)
download fn rep = do
  setHeader "Content-Disposition" $ T.append "attachiment; filename=" fn
  return (RepCsv rep)

main :: IO ()
main = warpDebug 3000 C
