{-

Convert the input files into a file suitable for drawing

Input files are for each indicator like:
Name, Code, .., 1963, 1964, ...
Albany, AL, .., 34.2, 67.1, ... (may be empty)

Whereas output files should be for each country like:
[[
  {year: 1963, indicator1: 34.2, indicator2: '45%'}, // start
   ... // stop
 ], [
   ... // start
   ... // stop
]]

That means, an array of pairs of points, each of them representing the
beginning and the end of a line. I opt to skip a point if it has an
empty value even for just one of the corresponding indicators

-}

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BLChar8
import Data.Csv
import qualified Data.Vector as V
import System.Environment
import Control.Monad
import Text.JSON
import System.Directory
import qualified Data.HashMap.Lazy as H
import qualified Data.List as List

-- Intermediate data structures, using hash maps in order to aggregate
-- indicators from multiple files. the process is translating the
-- decoded file in country data sets, and then in country data points
-- suitable for drawing, which will be saved as comma separated files
-- again

type Indicator = String
type Year = String
type Country = String
type DataPoint = H.HashMap Indicator String
type DataSet   = H.HashMap Year DataPoint
type CountryDataSets = H.HashMap Country DataSet
type LinePoints = [(DataPoint, DataPoint)]
type CountryLinePoints = H.HashMap Country LinePoints

dropFirstFourLines = BLChar8.unlines . drop 4 . BLChar8.lines

type Decoded = (V.Vector (V.Vector L.ByteString))
type Row = (V.Vector L.ByteString)

rightDecoded :: Either String Decoded -> Decoded
rightDecoded (Right v) = v
rightDecoded (Left v) =  V.fromList [V.fromList []]

simplify :: (V.Vector (V.Vector L.ByteString)) -> [[String]]
simplify v = map (map BLChar8.unpack) (map V.toList (V.toList v))

decodeFile :: L.ByteString -> [[String]]
decodeFile content = simplify $ rightDecoded $ Data.Csv.decode NoHeader cleaned
  where cleaned = dropFirstFourLines content

getIndicator :: [String] -> Indicator
getIndicator (countryName:countryCode:indicatorName:indicatorCode:values) =
  indicatorCode

getCountry :: [String] -> Country
getCountry (countryName:countryCode:indicatorName:indicatorCode:values) =
  countryName

getValues :: [String] -> [String]
getValues (countryName:countryCode:indicatorName:indicatorCode:values) =
  values

makePoint :: Indicator -> (String, String) -> (Year, DataPoint)
makePoint indicator (h,r) = (h, point)
  where point    = H.insert indicator r withYear
        withYear = H.insert "Year" h H.empty

makeDataSet :: [String] -> [String] -> (Country, H.HashMap Year DataPoint)
makeDataSet h r = (country, points)
  where country = getCountry r
        indicator = getIndicator r
        pairs = zip (getValues h) (getValues r)
        points = H.fromList $ map (makePoint indicator) pairs

makeDataSets :: [[String]] -> CountryDataSets
makeDataSets decoded =
  let h = head decoded
      t = tail decoded
  in H.fromList (map (makeDataSet h) t)

step1 :: [L.ByteString] -> [CountryDataSets]
step1 = map (makeDataSets . decodeFile)

transformDataSet :: DataSet -> Year -> DataPoint -> DataPoint
transformDataSet s y p = H.union (H.lookupDefault H.empty y s) p
  
mergeTwoDataSets :: DataSet -> DataSet -> DataSet
mergeTwoDataSets s1 s2 = H.mapWithKey (transformDataSet s1) s2

mergeTwo :: CountryDataSets -> CountryDataSets -> CountryDataSets
mergeTwo s1 s2 = H.unionWith mergeTwoDataSets s1 s2

merge :: [CountryDataSets] -> CountryDataSets
merge = foldl1 mergeTwo

-- group elements in a list by two, duplicating one element every time
byTwo :: [a] -> [(a,a)]
byTwo []       = []
byTwo (a:[])   = []
byTwo (a:b:[]) = [(a,b)]
byTwo (a:b:c)  = (a,b):(byTwo (b:c))

takeSecond :: [(a,b)] -> [b]
takeSecond = map snd

compareYear :: Year -> Year -> Ordering
compareYear y1 y2 = compare (read y1 :: Int) (read y2 :: Int)

makePairs :: [(Year, DataPoint)] -> [(DataPoint, DataPoint)]
makePairs l =
  let sorted = List.sortBy (\x y -> compareYear (fst x) (fst y)) l
  in byTwo $ takeSecond sorted

allValuesSet :: DataPoint -> Bool
allValuesSet = (all (/="")) . H.elems

removeEmpty :: DataSet -> DataSet
removeEmpty = H.filter allValuesSet

makeLines :: CountryDataSets -> CountryLinePoints
makeLines = H.map (makePairsFromDict . removeEmpty)
  where makePairsFromDict = makePairs . H.toList

dataPointToJS :: DataPoint -> JSValue
dataPointToJS = JSObject . toJSObject . H.toList . mapPoint
  where mapPoint = H.map (JSString . toJSString)

linePointToJS :: (DataPoint, DataPoint) -> JSValue
linePointToJS (a, b) =
  let c = dataPointToJS a
      d = dataPointToJS b
  in JSArray [c, d]

linePointsToBytes :: LinePoints -> String
linePointsToBytes = Text.JSON.encode . (map linePointToJS)

toJS :: CountryLinePoints -> [(Country,String)]
toJS = H.toList . (H.map linePointsToBytes)

convert :: [L.ByteString] -> [(Country,String)]
convert = toJS . makeLines . merge . step1

outdir = "converted"

writeConverted (name, content) = writeFile (outdir ++ "/" ++ name) content

main = do
  args <- getArgs
  contents <- mapM L.readFile args
  createDirectoryIfMissing False outdir
  mapM writeConverted (convert contents)
  return ()
