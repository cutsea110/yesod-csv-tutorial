{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
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
instance ToText a => ToContent (CSV a) where
  toContent = toContent.trans
    where
      trans :: ToText a => CSV a -> Text
      trans = T.unlines.uncurry (:).(xHead *** xBody).unCsv
      xBody :: ToText a => [[a]] -> [Text]
      xBody = map (T.intercalate "," . map toText)
      xHead :: [Text] -> Text
      xHead = T.intercalate ","

class ToText a where
  toText :: a -> Text

instance ToText Text where
  toText = id
instance ToText String where
  toText = T.pack
instance ToText Int where
  toText = T.pack . show
instance ToText Double where
  toText = T.pack . show
instance ToText Integer where
  toText = T.pack . show

newtype RepCsv a = RepCsv (CSV a)
instance ToText a => HasReps (RepCsv a) where
  chooseRep (RepCsv csv) _ = return (typeOctet, toContent csv)

data F = F {fn :: Text, dt :: Textarea, c :: Int}
       deriving Show
type Form x = Html -> MForm C C (FormResult x, Widget)
sampleForm :: (MsgC -> Text) -> Maybe F -> Form F
sampleForm r mf extra = do
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
    #{r FileName}: ^{fvInput fV}.csv
  <div>
    #{r DataSeed}: ^{fvInput dV}
  <div>
    #{r Counts}: ^{fvInput cV}
|]
  return (res, w)

instance RenderMessage C MsgC where
  renderMessage _ ("ja":_) = renderJa
  renderMessage _ _ = renderEn

rangeIntField :: RenderMessage m FormMessage => (Int, Int) -> Field s m Int
rangeIntField (l, h) = intField {
  fieldView = \theId name attrs val isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="number" :isReq:required="" value="#{showVal val}" min="#{show l}" max="#{show h}">
|]
  }
  where
    showVal = either id (T.pack.showI)
    showI x = show (fromIntegral x::Integer)

data MsgC = SampleCandidate
          | FileName
          | DataSeed
          | Counts
          | Sample
          deriving (Show, Eq, Read)

renderEn :: MsgC -> Text
renderEn SampleCandidate = "type:T-Shirt,Jacket,Polo-Shirt,Court\nsize:S,M,L,LL,XL\ncategory:Man,Women,Kids"
renderEn FileName = "file name"
renderEn DataSeed = "seed"
renderEn Counts = "counts"
renderEn Sample = "sample"

renderJa :: MsgC -> Text
renderJa SampleCandidate = "種別:Tシャツ,ジャケット,ポロシャツ,コート\nサイズ:S,M,L,LL,XL\nカテゴリ:紳士服,婦人服,キッズ"
renderJa FileName = "ファイル名"
renderJa DataSeed = "データの種"
renderJa Counts = "データ数"
renderJa Sample = "サンプル"

getRootR :: Handler RepHtml
getRootR = do
  r <- getMessageRender
  ((_, w), e) <- runFormPost $ sampleForm r $ Just $ F (r Sample) (Textarea $ r SampleCandidate) 1000
  defaultLayout [whamlet|
<form method=post action=@{RootR} enctype=#{e}>
  ^{w}
  <input type=submit>
|]

postRootR :: Handler (RepCsv Text)
postRootR = do
  r <- getMessageRender
  ((r, _), _) <- runFormPost $ sampleForm r Nothing
  case r of
    FormSuccess x -> download x
    FormFailure ms -> do
      setMessage $ toHtml $ foldl T.append T.empty ms
      redirect RootR
    _ -> redirect RootR

download :: F -> Handler (RepCsv Text)
download (F f cs n) = do
  setHeader "Content-Disposition" $ "attachiment; filename=" `T.append` f `T.append` ".csv"  
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
    notEOL = (&&)<$>(/='\n')<*>(/='\r')
