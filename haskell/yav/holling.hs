{-# LANGUAGE RecordWildCards #-}
import Data.Time.Calendar
import Text.Parsec hiding (label)
import Text.Parsec.String
import qualified Data.Map as Map
import Control.Applicative ( (<*) )
import Numeric(showFFloat)
import System.Environment(getArgs)
import System.IO(hPutStrLn,stderr)
import Prelude hiding (product)


data Product = Product
  { supplierId    :: Int
  , productCode   :: Int
  , description   :: String
  , deliveryDate  :: Day
  , costPrice     :: Int    -- in cents
  , unitCount     :: Int
  } deriving Show


--------------------------------------------------------------------------------
-- Bussiness Rules

computePrice :: Product -> Float
computePrice product
  | fromPremiumSupplier product = fromInteger $ ceiling $ markup (base + 10)
  | fromSlowSupplier product    = max 0 $ markup base - 2
  | otherwise                   = markup base

  where
  markup x = (fromIntegral (costPrice product) / 100) * (1 + x / 100)

  base | isApple  product = 40
       | isBanana product = 35
       | isBerry  product = 55
       | otherwise        = 50


computeSellBy :: Product -> Day
computeSellBy product
  | isApple  product = delay (2 * 7)
  | isBanana product = delay 5
  | isFruit  product = delay (1 * 7)
  | otherwise        = deliveryDate product    -- This is not specified?

  where
  delay x
    | fromSlowSupplier product = after $ max 0 (x - 3)
    | otherwise                = after x

  after x = addDays x (deliveryDate product)


inRange :: Int -> Int -> Product -> Bool
inRange x y i = x <= code && code <= y
  where code = productCode i

isFruit, isApple, isBanana, isBerry :: Product -> Bool
isFruit  = inRange 1000 1999
isApple  = inRange 1100 1199
isBanana = inRange 1200 1299
isBerry  = inRange 1300 1399

fromSlowSupplier :: Product -> Bool
fromSlowSupplier product = supplierId product `elem` [ 32, 101 ]

fromPremiumSupplier :: Product -> Bool
fromPremiumSupplier product = supplierId product `elem` [ 219, 204 ]


--------------------------------------------------------------------------------

data Label = Label
  { price       :: Float
  , sellBy      :: Day
  , label       :: String
  }

computeLabels :: Product -> [String]
computeLabels product = replicate (unitCount product)
                      $ formatLabel
                      $ Label { label  = take 31 $ description product
                              , price  = computePrice product
                              , sellBy = computeSellBy product
                              }

formatLabel :: Label -> String
formatLabel l = formatPrice ++ formatDate ++ formatDescr
  where
  pad n c txt = replicate (n - length txt) c ++ txt

  formatPrice = "R" ++ pad 8 ' ' (showFFloat (Just 2) (price l) "")

  formatDate  = let (y,m,d) = toGregorian (sellBy l)
                -- This is not year 10000 compatible :-)
                in pad 4 '0' (show y) ++ "/" ++
                   pad 2 '0' (show m) ++ "/" ++
                   pad 2 '0' (show d)

  formatDescr = take 31 $ label l


main :: IO ()
main = do args <- getArgs
          case args of
            [inFile, outFile] ->
              do ps <- parseProducts inFile
                 writeFile outFile $ unlines $ concatMap computeLabels ps

            _ -> hPutStrLn stderr $ unlines
                   [ "Parameters:"
                   , "  FILE      Input file CSV format."
                   , "  FILE      File where to save results."
                   ]


--------------------------------------------------------------------------------
-- Parsing the inputs.

number :: Num a => Parser a
number = (fromInteger . read) `fmap` many1 digit

date :: Parser Day
date = do year  <- number; char '/'
          month <- number; char '/'
          day   <- number
          return (fromGregorian year month day)

-- This allows for the columns to be re-arranged.
inputRow :: Int -> Map.Map String String -> Parser Product
inputRow row fieldValues =
  do supplierId   <- get number         "Supplier ID"
     productCode  <- get number         "Product Code"
     description  <- get (many anyChar) "Product Description"
     deliveryDate <- get date           "Delivery Date"
     costPrice    <- get number         "Unit Price"
     unitCount    <- get number         "Number of Units"
     return Product { .. }

  where
  get p field =
    case Map.lookup field fieldValues of
      Just str ->
        case parse p field str of
          Right a  -> return a
          Left err -> parserFail $ unlines
                        [ unwords ["In field", field, ", row", show row]
                        , "  " ++ show err
                        ]
      Nothing  -> parserFail ("Missing field: " ++ field)

inputFile :: Parser [Product]
inputFile =
  do headings <- csvLine
     many $ do row  <- sourceLine `fmap` getPosition
               vals <- csvLine
               inputRow row $ Map.fromList $ zip headings vals

parseProducts :: FilePath -> IO [Product]
parseProducts file =
  do txt <- readFile file
     case parse (inputFile <* eof) file txt of
       Right inputs -> return inputs
       Left err     -> fail (show err)



--------------------------------------------------------------------------------
-- Handling of data in CSV format

csvWord :: Parser String
csvWord = between (char '"') (char '"') (many strChar) <|> many (noneOf ",\n")
  where strChar = (char '\\' >> escChar) <|> noneOf "\""
        escChar = char '"' <|> char '\\' <|> return '\\'

csvLine :: Parser [String]
csvLine = (csvWord `sepBy` char ',') <* ((newline >> return ()) <|> eof)



