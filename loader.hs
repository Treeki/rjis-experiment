import Data.Either
import Data.List
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import RecordTypes

discardComments :: [T.Text] -> [T.Text]
discardComments = filter (\x -> T.head x /= '/')

recordLines :: T.Text -> [T.Text]
recordLines f = discardComments $ T.lines f

extractRecordLines :: Char -> [T.Text] -> [T.Text]
extractRecordLines recordType = filter (\x -> (T.index x 1) == recordType)

recordIsCurrent :: Day -> Maybe Day -> Day -> Bool
recordIsCurrent start Nothing    today = today >= start
recordIsCurrent start (Just end) today = (today >= start) && (today <= end)

extractCurrentRecords :: (a -> Day) -> (a -> Maybe Day) -> Day -> [a] -> [a]
extractCurrentRecords getStart getEnd today lst =
    filter isCurrent lst
    where isCurrent record = recordIsCurrent (getStart record) (getEnd record) today

holderTypeParser :: A.Parser HolderType
holderTypeParser = do
    ht <- A.satisfy (\c -> c == 'A' || c == 'C')
    return (if ht == 'A' then Adult else Child)

ynParser :: A.Parser Bool
ynParser = do
    yn <- A.satisfy (\c -> c == 'Y' || c == 'N')
    return (yn == 'Y')

zeroOneBoolParser :: A.Parser Bool
zeroOneBoolParser = do
    zeroOne <- A.satisfy (\c -> c == '0' || c == '1')
    return (zeroOne == '1')

optionalNumberParser :: Int -> A.Parser (Maybe Int)
optionalNumberParser l = do
    result <- A.choice [isNothing, isSomething]
    return result
  where
    isNothing = do
        A.count l $ A.char ' '
        return Nothing
    isSomething = do
        s <- A.count l A.digit
        return $ Just (read s)

dateParser :: A.Parser Day
dateParser = do
    d <- A.count 2 A.digit
    m <- A.count 2 A.digit
    y <- A.count 4 A.digit
    return $ fromGregorian (read y) (read m) (read d)

optionalDateParser :: A.Parser (Maybe Day)
optionalDateParser = do
    dStr <- A.count 2 A.digit
    mStr <- A.count 2 A.digit
    yStr <- A.count 4 A.digit
    let d = read dStr
        m = read mStr
        y = read yStr
    return $ if y == 2999 then Nothing else Just $ fromGregorian y m d

railcardRecordParser :: A.Parser RailcardRecord
railcardRecordParser = do
    railcardCode      <- A.count 3 A.anyChar
    endDate           <- optionalDateParser
    startDate         <- dateParser
    quoteDate         <- dateParser
    holderType        <- holderTypeParser
    description       <- A.count 20 A.anyChar
    restrictedByIssue <- ynParser
    restrictedByArea  <- ynParser
    restrictedByTrain <- ynParser
    restrictedByDate  <- ynParser
    masterCode        <- A.count 3 A.anyChar
    displayFlag       <- ynParser
    maxPassengers     <- A.count 3 A.digit
    minPassengers     <- A.count 3 A.digit
    maxHolders        <- A.count 3 A.digit
    minHolders        <- A.count 3 A.digit
    maxAccAdults      <- A.count 3 A.digit
    minAccAdults      <- A.count 3 A.digit
    maxAdults         <- A.count 3 A.digit
    minAdults         <- A.count 3 A.digit
    maxChildren       <- A.count 3 A.digit
    minChildren       <- A.count 3 A.digit
    price             <- A.count 8 A.digit
    discountPrice     <- A.count 8 A.digit
    validity          <- A.choice [validityMMDD, validityArbitraryDate]
    physicalCard      <- ynParser
    capriTicketType   <- A.count 3 A.anyChar
    adultStatus       <- A.count 3 A.anyChar
    childStatus       <- A.count 3 A.anyChar
    aaaStatus         <- A.count 3 A.anyChar
    return $ RailcardRecord
        { rcRailcardCode      = T.pack railcardCode
        , rcEndDate           = endDate
        , rcStartDate         = startDate
        , rcQuoteDate         = quoteDate
        , rcHolderType        = holderType
        , rcDescription       = T.stripEnd $ T.pack description
        , rcRestrictedByIssue = restrictedByIssue
        , rcRestrictedByArea  = restrictedByArea
        , rcRestrictedByTrain = restrictedByTrain
        , rcRestrictedByDate  = restrictedByDate
        , rcMasterCode        = T.pack masterCode
        , rcDisplayFlag       = displayFlag
        , rcMaxPassengers     = read maxPassengers
        , rcMinPassengers     = read minPassengers
        , rcMaxHolders        = read maxHolders
        , rcMinHolders        = read minHolders
        , rcMaxAccAdults      = read maxAccAdults
        , rcMinAccAdults      = read minAccAdults
        , rcMaxAdults         = read maxAdults
        , rcMinAdults         = read minAdults
        , rcMaxChildren       = read maxChildren
        , rcMinChildren       = read minChildren
        , rcPrice             = read price
        , rcDiscountPrice     = read discountPrice
        , rcValidity          = validity
        , rcPhysicalCard      = physicalCard
        , rcCapriTicketType   = T.pack capriTicketType
        , rcAdultStatus       = T.pack adultStatus
        , rcChildStatus       = T.pack childStatus
        , rcAaaStatus         = T.pack aaaStatus
        }
  where
    validityMMDD = do
        mm <- A.count 2 A.digit
        dd <- A.count 2 A.digit
        A.count 8 $ A.char ' '
        return $ ValidityMMDD (read mm) (read dd)
    validityArbitraryDate = do
        A.count 4 $ A.char ' '
        date <- dateParser
        return $ ValidUntilDate date

classLegendRecordParser :: A.Parser ClassLegendRecord
classLegendRecordParser = do
    class'    <- A.anyChar
    endDate   <- optionalDateParser
    startDate <- dateParser
    atbDesc   <- A.count 8 A.anyChar
    ccDesc    <- A.count 3 A.anyChar
    return $ ClassLegendRecord
        { clClass     = class'
        , clEndDate   = endDate
        , clStartDate = startDate
        , clAtbDesc   = T.stripEnd $ T.pack atbDesc
        , clCcDesc    = T.stripEnd $ T.pack ccDesc
        }

regionParser :: A.Parser Region
regionParser = do
    code <- A.satisfy $ A.inClass "0123456"
    return $ case code of
        '0' -> NonBrOrLul
        '1' -> ER
        '2' -> LMR
        '3' -> SCR
        '4' -> SR
        '5' -> WR
        '6' -> LUL


locationRecordParser :: A.Parser LocationRecord
locationRecordParser = do
    A.char 'R'
    A.char 'L'
    uicCode <- A.take 7
    endDate <- optionalDateParser
    startDate <- dateParser
    quoteDate <- dateParser
    adminAreaCode <- A.take 3
    nlcCode <- A.take 4
    description <- A.take 16
    crsCode <- A.take 3
    resvCode <- A.take 5
    ersCountry <- A.take 2
    ersCode <- A.take 3
    fareGroup <- A.take 6
    county <- A.take 2
    pteCode <- A.take 2
    zoneNo <- A.take 4
    zoneInd <- A.take 2
    region <- regionParser
    hierarchy <- A.anyChar
    ccDescOut <- A.take 41
    ccDescRtn <- A.take 16
    atbDescOut <- A.take 60
    atbDescRtn <- A.take 30
    specialFacilities <- A.count 26 A.anyChar
    lulDirectionInd <- A.satisfy $ A.inClass "0123 "
    lulUtsMode <- A.anyChar
    lulZones <- A.count 6 ynParser
    lulUtsLondonStation <- zeroOneBoolParser
    utsCode <- A.take 3
    utsAltCode <- A.take 3
    utsPtrBias <- A.anyChar
    utsOffset <- A.anyChar
    utsNorth <- A.take 3
    utsEast <- A.take 3
    utsSouth <- A.take 3
    utsWest <- A.take 3
    return $ LocationRecord
        { locUicCode                = uicCode
        , locEndDate                = endDate
        , locStartDate              = startDate
        , locQuoteDate              = quoteDate
        , locAdminAreaCode          = adminAreaCode
        , locNlcCode                = nlcCode
        , locDescription            = T.stripEnd description
        , locCrsCode                = crsCode
        , locResvCode               = resvCode
        , locErsCountry             = ersCountry
        , locErsCode                = ersCode
        , locFareGroup              = fareGroup
        , locCounty                 = county
        , locPteCode                = pteCode
        , locZoneNo                 = zoneNo
        , locZoneInd                = zoneInd
        , locRegion                 = region
        , locHierarchy              = hierarchy
        , locCcDescOut              = T.stripEnd ccDescOut
        , locCcDescRtn              = T.stripEnd ccDescRtn
        , locAtbDescOut             = T.stripEnd atbDescOut
        , locAtbDescRtn             = T.stripEnd atbDescRtn
        , locSpecialFacilities      = specialFacilities
        , locLulDirectionInd        = lulDirectionInd
        , locLulUtsMode             = lulUtsMode
        , locLulZones               = lulZones
        , locLulUtsLondonStation    = lulUtsLondonStation
        , locUtsCode                = utsCode
        , locUtsAltCode             = utsAltCode
        , locUtsPtrBias             = utsPtrBias
        , locUtsOffset              = utsOffset
        , locUtsNorth               = utsNorth
        , locUtsEast                = utsEast
        , locUtsSouth               = utsSouth
        , locUtsWest                = utsWest
        }



ttGroupLocationRecordParser :: A.Parser TTGroupLocationRecord
ttGroupLocationRecordParser = do
    A.char 'R' -- require full refresh file
    A.char 'G'
    groupUicCode <- A.count 7 A.digit
    endDate      <- optionalDateParser
    startDate    <- dateParser
    quoteDate    <- dateParser
    description  <- A.count 16 A.anyChar
    ersCountry   <- A.count 2 A.anyChar
    ersCode      <- A.count 2 A.anyChar
    return $ TTGroupLocationRecord
        { ttGlGroupUicCode = (read groupUicCode) 
        , ttGlEndDate      = endDate
        , ttGlStartDate    = startDate
        , ttGlQuoteDate    = quoteDate
        , ttGlDescription  = T.stripEnd $ T.pack description
        , ttGlErsCountry   = T.stripEnd $ T.pack ersCountry
        , ttGlErsCode      = T.stripEnd $ T.pack ersCode
        }

displayForTesting :: Show a => A.Parser a -> T.Text -> IO ()
displayForTesting parser line = do
    let parseResult = A.parseOnly parser line
    let text = case parseResult of
                  Left error -> "Fucked it " ++ error ++ " on " ++ show line
                  Right result -> show result
    putStrLn text


main :: IO ()
main = do
    rightNow <- getCurrentTime
    let today = utctDay rightNow

    tcl <- T.readFile "atocData/RJFAF851.TCL"
    let tclLines = recordLines tcl
    mapM_ (displayForTesting classLegendRecordParser) tclLines

    rlc <- T.readFile "atocData/RJFAF851.RLC"
    let rlcLines = recordLines rlc
    mapM_ (displayForTesting railcardRecordParser) (take 3 rlcLines)

    loc <- T.readFile "atocData/RJFAF851.LOC"
    let locLines = recordLines loc

    let locationLines          = extractRecordLines 'L' locLines
    let railcardGeographyLines = extractRecordLines 'R' locLines
    let ttGroupLocationLines   = extractRecordLines 'G' locLines
    let groupMemberLines       = extractRecordLines 'M' locLines
    let synonymLines           = extractRecordLines 'S' locLines

    let locations = rights $ map (A.parseOnly locationRecordParser) locationLines
    let currentLocations = extractCurrentRecords locStartDate locEndDate today locations
    let glasgowZone = filter (\x -> (locZoneNo x) == (T.pack "I417")) currentLocations
    let glasgowZoneNames = map locDescription glasgowZone
    T.putStrLn $ T.unlines $ sort glasgowZoneNames

    --mapM_ (\x -> putStrLn $ show x) glasgow

    --let hst = filter (\x -> T.isPrefixOf (T.pack "HIGH ST") (locDescription x)) currentLocations
    --mapM_ (\x -> putStrLn $ show x) hst

    --mapM_ (\x -> putStrLn $ show x) (take 20 locations)
    --mapM_ (displayForTesting locationRecordParser) (take 20 (drop 10000 locationLines))
    --mapM_ (displayForTesting ttGroupLocationRecordParser) (take 3 ttGroupLocationLines)
