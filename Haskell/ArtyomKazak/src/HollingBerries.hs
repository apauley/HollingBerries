module Main where

import Text.CSV
import Text.Printf
import Data.Time
import Data.Time.Format
import System.Locale
import Data.Maybe

data Kind = Apple | Banana | Berry | Other
  deriving Eq

data Item = Item
          { supplier :: Int
          , code     :: Int
          , descript :: String
          , date     :: Day
          , price    :: Double
          , units    :: Int
          }
          
instance Show Item where
  show i = concat (replicate (units i) one)
    where one = printf "R%8.2f%10s%31s\n" 
                  (price i)
                  (formatTime defaultTimeLocale "%Y/%m/%d" (date i)) 
                  (take 31 (descript i))
             
readItem :: Record -> Item
readItem [supp, code, desc, date, pric, unit] = 
  Item (read supp) (read code) desc t (read pric / 100) (read unit)
  where t = readTime defaultTimeLocale "%Y/%m/%d" date

main = parseCSVFromFile "produce.csv" 
   >>= (\(Right f) -> writeFile "pricefile.txt" (process f))

process :: CSV -> String
process = concatMap (show . calc . readItem) . filter ((== 6) . length). tail

kind :: Item -> Kind
kind i | x < 1100  = Other
       | x < 1200  = Apple
       | x < 1300  = Banana
       | x < 1400  = Berry
       | otherwise = Other
  where x = code i

calc :: Item -> Item
calc i = i {price = price', date = sellBy'}
  where
    k = kind i
    trouble = supplier i `elem` [32, 101]
    premium = supplier i `elem` [204, 219]
    
    markup = fromJust $ lookup k 
      [(Apple, 1.4), (Banana, 1.35), (Berry, 1.55), (Other, 1.5)]
    price' = max 0 
           $ if premium then fromIntegral $ ceiling ((markup + 0.1) * price i)
                        else (markup * price i - if trouble then 2 else 0)
    
    diff = fromJust $ lookup k 
      [(Apple, 14), (Banana, 5), (Berry, 7), (Other, 7)]
    sellBy' = addDays (diff - if trouble then 3 else 0) (date i)