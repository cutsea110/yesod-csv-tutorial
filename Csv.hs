{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
module Csv where

import Yesod
import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((***),(&&&))
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

newtype CSV a = CSV { unCsv :: ([Text],[[a]]) } deriving Show
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

data F = F {fn :: Text, dt :: Textarea, c :: Int}
       deriving Show
type Form x = Html -> MForm C C (FormResult x, Widget)
sampleForm :: Maybe F -> Form F
sampleForm mf extra = do
  (fR, fV) <- mreq textField "file name" (fn <$> mf)
  (dR, dV) <- mreq textareaField "seeds" (dt <$> mf)
  (cR, cV) <- mreq (rangeIntField (10,1000)) "counts" (c <$> mf)
  let res = F <$> fR <*> dR <*> cR
      w = do
        toWidget [lucius|
##{fvId fV} {
  width: 8em;
  text-align: center;
}
##{fvId dV} {
  width: 600px;
  height: 10em;
}
##{fvId cV} {
  width: 6em;
  size: 5;
  text-align: right;
}
|]
        [whamlet|
#{extra}
<div>
  <div>
    ^{fvLabel fV}: ^{fvInput fV}
  <div>
    ^{fvLabel dV}: ^{fvInput dV}
  <div>
    ^{fvLabel cV}: ^{fvInput cV}
|]
  return (res, w)

rangeIntField :: (Int, Int) -> Field s C Int
rangeIntField (l, h) = intField {
  fieldView = \theId name attrs val isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="number" :isReq:required="" value="#{showVal val}" min="#{show l}" max="#{show h}">
|]
  }
  where
    showVal = either id (T.pack.showI)
    showI x = show (fromIntegral x::Integer)

sampleCandidate :: Textarea
sampleCandidate = Textarea "type:T-Shirt,Jacket,Polo-Shirt,Court\nsize:S,M,L,LL,XL\nsex:Man,Women,Kids"

getRootR :: Handler RepHtml
getRootR = do
  ((_, w), e) <- runFormPost $ sampleForm $ Just $ F "sample" sampleCandidate 1000
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
    FormFailure ms -> do
      setMessage $ toHtml $ foldl T.append T.empty ms
      redirect RootR
    _ -> redirect RootR

download :: F -> Handler (RepCsv Text)
download (F fn cs n) = do
  setHeader "Content-Disposition" $ "attachiment; filename=" `T.append` fn `T.append` ".csv"  
  csv <- liftIO $ generateCSV n cs
  return (RepCsv csv)

generateCSV :: Int -> Textarea -> IO (CSV Text)
generateCSV n = fmap (CSV . arrange) . mapM genR . extract . unTextarea
  where
    extract :: Text -> [(Text, [Text])]
    extract = map records . lines'
    records :: Text -> (Text, [Text])
    records = (id *** (T.split (==',') . T.tail)) . T.break (==':')
    arrange :: [(Text, [Text])] -> ([Text], [[Text]])
    arrange = foldr (curry mix) ([], repeat [])
    mix :: ((Text, [Text]), ([Text],[[Text]])) -> ([Text],[[Text]])
    mix = (:)<$>fst.fst<*>fst.snd &&& zipWith (:)<$>snd.fst<*>snd.snd
    genR :: (Text, [Text]) -> IO (Text, [Text])
    genR (h, cs) = do
      g <- getStdGen
      return (h, take n $ map (cs!!) $ randomRs (0, length cs-1) g)

main :: IO ()
main = warpDebug 3000 C

-- | utility which deprecated from Data.Text
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
