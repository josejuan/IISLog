{-# LANGUAGE RecordWildCards #-}
module Data.IISLog (
  Log (..)
, utctime
, fromString
, fromFile
) where

import Control.Applicative
import Data.List
import Data.Monoid
import Data.Time
import Data.Time.Format
import Control.Applicative
import Data.IP
import Text.Read
import Network.HTTP.Types.Method

k_HEADER_PREFIX :: String
k_HEADER_PREFIX = "#Fields:"

k_COMMENT_PREFIX :: String
k_COMMENT_PREFIX = "#"

isHeader :: String -> Bool
isHeader = isPrefixOf k_HEADER_PREFIX

data Log = Log { date           :: !(Maybe Day      ) 
               , time           :: !(Maybe TimeOfDay) 
               , sip            :: !(Maybe IP       ) 
               , method         :: !(Maybe StdMethod) 
               , uristem        :: !(Maybe String   ) 
               , uriquery       :: !(Maybe String   ) 
               , sport          :: !(Maybe Int      ) 
               , username       :: !(Maybe String   ) 
               , cip            :: !(Maybe IP       ) 
               , useragent      :: !(Maybe String   ) 
               , referer        :: !(Maybe String   ) 
               , status         :: !(Maybe Int      ) 
               , substatus      :: !(Maybe Int      ) 
               , win32status    :: !(Maybe Int      ) 
               , cbytes         :: !(Maybe Int      ) 
               , sbytes         :: !(Maybe Int      ) 
               , timetaken      :: !(Maybe Int      ) -- milliseconds
               } deriving (Eq, Show)

utctime :: Log -> Maybe UTCTime
utctime (Log {..}) = UTCTime <$> date <*> (timeOfDayToTime <$> time)

instance Monoid Log where
    mempty = Log Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    (Log a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1) `mappend` (Log a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2) =
        Log (a1 <|> a2) (b1 <|> b2) (c1 <|> c2) (d1 <|> d2) (e1 <|> e2) (f1 <|> f2) (g1 <|> g2) (h1 <|> h2) (i1 <|> i2) (j1 <|> j2) (k1 <|> k2) (l1 <|> l2) (m1 <|> m2) (n1 <|> n2) (o1 <|> o2) (p1 <|> p2) (q1 <|> q2)

field :: String -> String -> Log
field "date"               x = mempty { date        = parseTimeM True defaultTimeLocale "%F" x }
field "time"               x = mempty { time        = parseTimeM True defaultTimeLocale "%T" x }
field "s-ip"               x = mempty { sip         = readMaybe x }
field "cs-method"          x = mempty { method      = readMaybe x }
field "cs-uri-stem"        x = mempty { uristem     = Just x }
field "cs-uri-query"       x = mempty { uriquery    = Just x }
field "s-port"             x = mempty { sport       = readMaybe x }
field "cs-username"        x = mempty { username    = Just x }
field "c-ip"               x = mempty { cip         = readMaybe x }
field "cs(User-Agent)"     x = mempty { useragent   = Just x }
field "cs(Referer)"        x = mempty { referer     = Just x }
field "sc-status"          x = mempty { status      = readMaybe x }
field "sc-substatus"       x = mempty { substatus   = readMaybe x }
field "sc-win33-status"    x = mempty { win32status = readMaybe x }
field "sc-bytes"           x = mempty { cbytes      = readMaybe x }
field "cs-bytes"           x = mempty { sbytes      = readMaybe x }
field "time-taken"         x = mempty { timetaken   = readMaybe x }
field _ _ = mempty

block :: [String] -> [Log]
block (x:xs) =
    let (p:hs) = words x
    in  if p /= k_HEADER_PREFIX
            then error $ "Impossible!"
            else map (mconcat.zipWith field hs.words) $ filter (not.isPrefixOf k_COMMENT_PREFIX) xs

blocks :: [String] -> [[String]]
blocks (x:xs) =
    if isHeader x
        then (x: takeWhile (not.isHeader) xs): blocks (dropWhile (not.isHeader) xs)
        else blocks xs
blocks [] = []

fromString :: String -> [Log]
fromString = filter (mempty /=) . concatMap block . blocks . lines

fromFile :: FilePath -> IO [Log]
fromFile file = fromString <$> readFile file
