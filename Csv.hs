{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Csv where

import Yesod
import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((***))
import Data.List (transpose)
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (getStdGen, randomRs)

data C = C

mkYesod "C" [parseRoutes|
/ RootR GET POST
|]

instance Yesod C
instance RenderMessage C FormMessage where
  renderMessage _ _ = defaultFormMessage

newtype CSV a = CSV { unCsv :: ([Text],[[a]]) }
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

newtype RepCsv a = RepCsv (CSV a)
instance Show a => HasReps (RepCsv a) where
  chooseRep (RepCsv c) _ = return (typeOctet, toContent c)

data F = F {name::Text, candidates :: Textarea, num :: Int}
       deriving Show
type Form x = Html -> MForm C C (FormResult x, Widget)
sampleForm :: Maybe F -> Form F
sampleForm mf = renderDivs $ F 
             <$> areq textField "file name" (name <$> mf)
             <*> areq textareaField "candidates" (candidates <$> mf)
             <*> areq intField "generate number" (num <$> mf)

getRootR :: Handler RepHtml
getRootR = do
  (w, e) <- generateFormPost $ sampleForm $ Just $ F "sample" (Textarea "type:X,Y,Z\ncost:100,200,300,400,500,600,700\ncategory:Kid,Man,Women") 1000
  defaultLayout [whamlet|
<form method=post action=@{RootR} enctype=#{e}>
  ^{w}
  <input type=submit>
|]

postRootR :: Handler (RepCsv Text)
postRootR = do
  ((r, _), _) <- runFormPost $ sampleForm Nothing
  case r of
    FormSuccess x -> download x
    _ -> redirect RootR

download :: F -> Handler (RepCsv Text)
download (F fn cs n) = do
  rows <- liftIO $ generateFrom (unTextarea cs) n
  downloadCSV (fn `T.append` ".csv") $ CSV rows

downloadCSV :: Yesod m => Text -> CSV Text -> GHandler m s (RepCsv Text)
downloadCSV fn rep = do
  setHeader "Content-Disposition" $ T.append "attachiment; filename=" fn
  return (RepCsv rep)

generateFrom :: Text -> Int -> IO ([Text], [[Text]])
generateFrom cs n = fmap toCSV $ mapM (genR n) $ extract cs

extract :: Text -> [(Text, [Text])]
extract = map (toDataList . T.break (==':')) . lines'
  where
    toDataList = id *** (T.split (==',') . T.tail)

lines' :: Text -> [Text]
lines' ps | T.null ps = []
          | otherwise = h : case T.uncons t of
  Nothing -> []
  Just (c,t')
    | c == '\n' -> lines' t'
    | c == '\r' -> case T.uncons t' of
      Just ('\n',t'') -> lines' t''
      _               -> lines' t'
  where
    (h,t) = T.span notEOL ps
    notEOL c = c /= '\n' && c /= '\r'

genR :: Int -> (Text, [Text]) -> IO (Text, [Text])
genR n (h, cs) = do
  g <- getStdGen
  return $ (h, take n $ map (cs!!) $ randomRs (0, length cs-1) g)

toCSV :: [(Text, [Text])] -> ([Text], [[Text]])
toCSV = (id *** transpose) . foldr f ([], [])
  where
    f (h, b) (hs, bs) = (h:hs, b:bs)

main :: IO ()
main = warpDebug 3000 C
