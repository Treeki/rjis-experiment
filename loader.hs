{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List
import Data.Time
import qualified Data.Map.Strict as Map
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

flowUsageCodeParser :: A.Parser FlowUsageCode
flowUsageCodeParser = do
    code <- A.satisfy $ A.inClass "CGA"
    return $ case code of
        'A' -> ActualFare
        'G' -> CombinedFlows
        'C' -> Summation

flowDirectionParser :: A.Parser FlowDirection
flowDirectionParser = do
    code <- A.satisfy (\x -> (x == 'S') || (x == 'R'))
    return $ case code of
        'S' -> SingleDirection
        'R' -> Reversible

flowCrossLondonParser :: A.Parser FlowCrossLondon
flowCrossLondonParser = do
    code <- A.satisfy $ A.inClass "0123"
    return $ case code of
        '0' -> NotViaLondon
        '1' -> ViaLondonTube
        '2' -> ViaLondon
        '3' -> ViaThameslink

flowRecordParser :: A.Parser FlowRecord
flowRecordParser = do
    A.char 'R'
    A.char 'F'
    originCode              <- A.take 4
    destinationCode         <- A.take 4
    routeCode               <- A.count 5 A.digit
    statusCode              <- A.take 3
    usageCode               <- flowUsageCodeParser
    direction               <- flowDirectionParser
    endDate                 <- optionalDateParser
    startDate               <- dateParser
    toc                     <- A.take 3
    crossLondonInd          <- flowCrossLondonParser
    nsDiscInd               <- A.satisfy $ A.inClass "0123"
    let isPrivate =            (nsDiscInd == '2') || (nsDiscInd == '3')
    let nonStandardDiscounts = (nsDiscInd == '1') || (nsDiscInd == '3')
    publishedInNFM          <- ynParser
    flowID                  <- A.count 7 A.digit
    return $ FlowRecord
        { flOriginCode           = originCode
        , flDestinationCode      = destinationCode
        , flRouteCode            = read routeCode
        , flStatusCode           = statusCode
        , flUsageCode            = usageCode
        , flDirection            = direction
        , flEndDate              = endDate
        , flStartDate            = startDate
        , flToc                  = toc
        , flCrossLondonInd       = crossLondonInd
        , flIsPrivate            = isPrivate
        , flNonStandardDiscounts = nonStandardDiscounts
        , flPublishedInNFM       = publishedInNFM
        , flFlowID               = read flowID
        }

fareRecordParser :: A.Parser FareRecord
fareRecordParser = do
    A.char 'R'
    A.char 'T'
    flowID          <- A.count 7 A.digit
    ticketCode      <- A.take 3
    fare            <- A.count 8 A.digit
    restrictionCode <- A.take 2
    return $ FareRecord
        { fareFlowID            = read flowID
        , fareTicketCode        = ticketCode
        , farePrice             = read fare
        , fareRestrictionCode   = restrictionCode
        }

stationClusterRecordParser :: A.Parser StationClusterRecord
stationClusterRecordParser = do
    A.char 'R'
    clusterID   <- A.take 4
    clusterNLC  <- A.take 4
    endDate     <- optionalDateParser
    startDate   <- dateParser
    return $ StationClusterRecord
        { scClusterID   = clusterID
        , scClusterNLC  = clusterNLC
        , scEndDate     = endDate
        , scStartDate   = startDate
        }

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
        , locNLCCode                = nlcCode
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


loadFlows :: T.Text -> Day -> ([FlowRecord], Map.Map Int [FareRecord])
loadFlows rawText currentDay = (currentFlows, faresMap)
    where
        allLines            = recordLines rawText
        flowLines           = extractRecordLines 'F' allLines
        fareLines           = extractRecordLines 'T' allLines
        flowParseResults    = map (A.parseOnly flowRecordParser) flowLines
        fareParseResults    = map (A.parseOnly fareRecordParser) fareLines
        allFlows            = rights flowParseResults
        currentFlows        = extractCurrentRecords flStartDate flEndDate currentDay allFlows
        allFares            = rights fareParseResults
        farePairs           = [(fareFlowID fare, [fare]) | fare <- allFares]
        faresMap            = Map.fromListWith (++) farePairs

{-
    let locationLines          = extractRecordLines 'L' locLines
    let railcardGeographyLines = extractRecordLines 'R' locLines
    let ttGroupLocationLines   = extractRecordLines 'G' locLines
    let groupMemberLines       = extractRecordLines 'M' locLines
    let synonymLines           = extractRecordLines 'S' locLines
-}
loadLocations :: T.Text -> Day -> Map.Map NLC LocationRecord
loadLocations rawText currentDay = outputMap
    where
        allLines        = recordLines rawText
        locationLines   = extractRecordLines 'L' allLines
        locParseResults = map (A.parseOnly locationRecordParser) locationLines
        locRecords      = rights locParseResults
        curLocRecords   = extractCurrentRecords locStartDate locEndDate currentDay locRecords
        nlcPairs        = [(locNLCCode loc, loc) | loc <- curLocRecords]
        outputMap       = Map.fromList nlcPairs

loadStationClusters :: T.Text -> Day -> Map.Map NLC [NLC]
loadStationClusters rawText currentDay = outputMap
    where
        allLines        = recordLines rawText
        parseResults    = map (A.parseOnly stationClusterRecordParser) allLines
        records         = rights parseResults
        curRecords      = extractCurrentRecords scStartDate scEndDate currentDay records
        pairs           = [(scClusterID rec, [scClusterNLC rec]) | rec <- curRecords]
        outputMap       = Map.fromListWith (++) pairs

main :: IO ()
main = do
    rightNow <- getCurrentTime
    let today = utctDay rightNow

    -- LOCATIONS
    loc <- T.readFile "atocData/RJFAF851.LOC"
    let locsByNLC = loadLocations loc today

    let displayMaybeLoc code Nothing  = T.unpack code
        displayMaybeLoc code (Just l) = (T.unpack code) ++ ": " ++ (T.unpack $ locDescription l)
    let displayLoc code = displayMaybeLoc code (Map.lookup code locsByNLC)

    -- STATION CLUSTERS
    fsc <- T.readFile "atocData/RJFAF851.FSC"
    let clusters = loadStationClusters fsc today

    -- Print out all the clusters
    let showClusterPair (k, v) =
          "[" ++ (T.unpack k) ++ "]\n" ++ (unlines memberList)
          where memberList = map (\x -> displayLoc x) v
    let groupedClusterPairs = (Map.toList clusters)
    let justQ5Clusters = filter (\(k, v) -> (T.isPrefixOf "Q5" k)) groupedClusterPairs
    mapM_ (\x -> putStrLn $ showClusterPair x) justQ5Clusters

    --let glasgowZone = filter (\x -> (locZoneNo x) == "I417") currentLocations
    --let glasgowZone = filter (\x -> (locPteCode x) == "SC") currentLocations
    --let glasgowZoneNames = map (\x -> T.intercalate " | " [locUicCode x, locNLCCode x, locCrsCode x, locCounty x, locPteCode x, locZoneNo x, locDescription x]) glasgowZone
    --T.putStrLn $ T.unlines $ sort glasgowZoneNames

    -- OTHER SHITE
    let glqNLC = "9950"
    let glcNLC = "9813"
    let glqOrGlcNLC = "0433"
    let partickNLC = "9917"
    let hstNLC = "9909"
    let cathcartNLC = "9795"
    let neilstonNLC = "9773"
    let eastKilbrideNLC = "9793"
    let glaSubwayNLC = "H973"

    let cct = Map.lookup cathcartNLC locsByNLC
    putStrLn $ show cct

    ffl <- T.readFile "atocData/RJFAF851.FFL"
    let (flows, faresMap) = loadFlows ffl today
    let subwayFlows = filter (\x -> (flOriginCode x == glaSubwayNLC) || (flDestinationCode x == glaSubwayNLC)) flows
    
    -- this is horrible
    let displayFlow flow =
          (src ++ " -> " ++ dst ++ " :: " ++ (show $ flFlowID flow) ++ "\n" ++ (unlines fareStrings))
          where
            srcCode = flOriginCode flow
            dstCode = flDestinationCode flow
            src     = displayLoc srcCode
            dst     = displayLoc dstCode
            fareRecords = faresMap Map.! (flFlowID flow)
            fareStrings = ["  " ++ (T.unpack $ fareTicketCode flow) ++ " " ++ (show $ farePrice flow) | flow <- fareRecords]

    mapM_ (\x -> putStrLn $ displayFlow x) (take 3 subwayFlows)

    --let glqOrGlcToCctFares = filter (\x -> fareFlowID x == 8390) fares
    --mapM_ (\x -> putStrLn $ show x) cathcartFlows
    --mapM_ (\x -> putStrLn $ show x) glqOrGlcToCctFares
    --mapM_ (\x -> putStrLn $ show x) someFares


    --mapM_ (\x -> putStrLn $ show x) glasgow

    --let hst = filter (\x -> T.isPrefixOf "HIGH ST" (locDescription x)) currentLocations
    --mapM_ (\x -> putStrLn $ show x) hst

    --mapM_ (\x -> putStrLn $ show x) (take 20 locations)
    --mapM_ (displayForTesting locationRecordParser) (take 20 (drop 10000 locationLines))
    --mapM_ (displayForTesting ttGroupLocationRecordParser) (take 3 ttGroupLocationLines)
