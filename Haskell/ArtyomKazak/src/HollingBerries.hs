module Main where

import Text.CSV
import Text.Printf
import Data.Time
import Data.Time.Format
import System.Locale
import Data.Maybe
import Data.List
import Data.Ix

data Kind = Apple | Banana | Berry | Other
  deriving Eq
  
markupTable = [(Apple, 1.4), (Banana, 1.35), (Berry, 1.55), (Other, 1.5)]
shelfTable  = [(Apple, 14), (Banana, 5), (Berry, 7), (Other, 7)]
codeTable   = [(Apple, [(1100, 1199)]), (Banana, [(1200, 1299)]),
               (Berry, [(1300, 1399)]), (Other, [(1000, 1099), (1400, 1999)])]

isTrouble = (`elem` [32, 101])
isPremium = (`elem` [204, 219])

data Item = Item
          { supplier :: Int
          , code     :: Int
          , descript :: String
          , day      :: Day
          , price    :: Double
          , units    :: Int
          }
          
instance Show Item where
  show i = concat (replicate (units i) one)
    where one = printf "R%8.2f%10s%31s\n" 
                  (price i)
                  (formatTime defaultTimeLocale "%Y/%m/%d" (day i)) 
                  (take 31 (descript i))
             
readItem :: Record -> Item
readItem [supp, code, desc, day, pric, unit] = 
  Item (read supp) (read code) desc t (read pric / 100) (read unit)
  where t = readTime defaultTimeLocale "%Y/%m/%d" day

main = parseCSVFromFile "produce.csv" 
   >>= (\(Right f) -> writeFile "pricefile.txt" (process f))

process :: CSV -> String
process = concatMap (show . calc . readItem) . filter ((== 6) . length). tail

calc :: Item -> Item
calc i = i {price = price', day = day'}
  where
    kind = fst $ fromJust $ find (any (`inRange` (code i)) . snd) codeTable
    trouble = isTrouble $ supplier i
    premium = isPremium $ supplier i
    
    markup = fromJust $ lookup kind markupTable
    price' = max 0 
           $ if premium then fromIntegral $ ceiling ((markup + 0.1) * price i)
                        else (markup * price i - if trouble then 2 else 0)
    
    shelf = fromJust $ lookup kind shelfTable
    day'  = addDays (shelf - if trouble then 3 else 0) (day i)