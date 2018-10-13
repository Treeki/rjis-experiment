import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A

data HolderType = Adult | Child deriving (Show)

data RailcardValidity = ValidityMMDD Int Int
                      | ValidUntilDate Day
                      deriving (Show)

data RailcardRecord = RailcardRecord { rcRailcardCode      :: T.Text
                                     , rcEndDate           :: Maybe Day
                                     , rcStartDate         :: Day
                                     , rcQuoteDate         :: Day
                                     , rcHolderType        :: HolderType
                                     , rcDescription       :: T.Text
                                     , rcRestrictedByIssue :: Bool
                                     , rcRestrictedByArea  :: Bool
                                     , rcRestrictedByTrain :: Bool
                                     , rcRestrictedByDate  :: Bool
                                     , rcMasterCode        :: T.Text
                                     , rcDisplayFlag       :: Bool
                                     , rcMaxPassengers     :: Int
                                     , rcMinPassengers     :: Int
                                     , rcMaxHolders        :: Int
                                     , rcMinHolders        :: Int
                                     , rcMaxAccAdults      :: Int
                                     , rcMinAccAdults      :: Int
                                     , rcMaxAdults         :: Int
                                     , rcMinAdults         :: Int
                                     , rcMaxChildren       :: Int
                                     , rcMinChildren       :: Int
                                     , rcPrice             :: Int
                                     , rcDiscountPrice     :: Int
                                     , rcValidity          :: RailcardValidity
                                     , rcPhysicalCard      :: Bool
                                     , rcCapriTicketType   :: T.Text
                                     , rcAdultStatus       :: T.Text
                                     , rcChildStatus       :: T.Text
                                     , rcAaaStatus         :: T.Text
                                     } deriving (Show)

data ClassLegendRecord = ClassLegendRecord { clClass     :: Char
                                           , clEndDate   :: Maybe Day
                                           , clStartDate :: Day
                                           , clAtbDesc   :: T.Text
                                           , clCcDesc    :: T.Text
                                           } deriving (Show)

discardComments :: [T.Text] -> [T.Text]
discardComments = filter (\x -> T.head x /= '/')

recordLines :: T.Text -> [T.Text]
recordLines f = discardComments $ T.lines f

holderTypeParser :: A.Parser HolderType
holderTypeParser = do
    ht <- A.satisfy (\c -> c == 'A' || c == 'C')
    return (if ht == 'A' then Adult else Child)

ynParser :: A.Parser Bool
ynParser = do
    yn <- A.satisfy (\c -> c == 'Y' || c == 'N')
    return (yn == 'Y')

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

displayForTesting :: Show a => A.Parser a -> T.Text -> IO ()
displayForTesting parser line = do
    let parseResult = A.parseOnly parser line
    let text = case parseResult of
                  Left error -> "Fucked it " ++ error
                  Right result -> show result
    putStrLn text


main :: IO ()
main = do
    tcl <- T.readFile "atocData/RJFAF851.TCL"
    let tclLines = recordLines tcl
    mapM_ (displayForTesting classLegendRecordParser) tclLines

    rlc <- T.readFile "atocData/RJFAF851.RLC"
    let rlcLines = recordLines rlc
    mapM_ (displayForTesting railcardRecordParser) (take 20 rlcLines)
