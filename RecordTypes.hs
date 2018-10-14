module RecordTypes where

import Data.Time
import qualified Data.Text as T

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Enum)

-- FFL: Flow (Page 12-14)
data FlowUsageCode = ActualFare | CombinedFlows | Summation
        deriving (Show, Enum)
data FlowDirection = SingleDirection | Reversible
        deriving (Show, Enum)
data FlowCrossLondon = NotViaLondon | ViaLondonTube | ViaLondon | ViaThameslink
        deriving (Show, Enum)

data FlowRecord = FlowRecord
    { flOriginCode           :: T.Text
    , flDestinationCode      :: T.Text
    , flRouteCode            :: Int
    , flStatusCode           :: T.Text
    , flUsageCode            :: FlowUsageCode
    , flDirection            :: FlowDirection
    , flEndDate              :: Maybe Day
    , flStartDate            :: Day
    , flToc                  :: T.Text
    , flCrossLondonInd       :: FlowCrossLondon
    , flIsPrivate            :: Bool
    , flNonStandardDiscounts :: Bool
    , flPublishedInNFM       :: Bool
    , flFlowID               :: T.Text
    } deriving Show

data FareRecord = FareRecord
    { fareFlowID          :: T.Text
    , fareTicketCode      :: T.Text
    , farePrice           :: Int
    , fareRestrictionCode :: T.Text
    } deriving Show

-- FSC: Station Cluster (Page 15)
data StationClusterRecord = StationClusterRecord
    { scClusterID  :: T.Text
    , scClusterNLC :: T.Text
    , scEndDate    :: Maybe Day
    , scStartDate  :: Day
    } deriving Show

-- NDF: Non-derivable fares (Page 16-17)
-- NFC: Non-derivable fare overrides (Page 18-19)
-- Pretty much the same, as far as I can tell
data NonDerivableFareRecord = NonDerivableFareRecord
    { ndfOriginCode      :: T.Text
    , ndfDestinationCode :: T.Text
    , ndfRouteCode       :: Int
    , ndfRailcardCode    :: Maybe T.Text
    , ndfTicketCode      :: T.Text
    , ndfEndDate         :: Maybe Day
    , ndfStartDate       :: Day
    , ndfQuoteDate       :: Day
    , ndfSuppressMarker  :: Bool
    , ndfAdultFare       :: Int
    , ndfChildFare       :: Int
    , ndfRestrictionCode :: Maybe T.Text
    , ndfComposite       :: Bool
    , ndfCrossLondon     :: Bool
    , ndfIsPrivate       :: Bool
    } deriving Show

-- FNS: Non-Standard discounts (Page 20-22)
data RebookFlag = RebookFlagN | RebookFlagY | RebookFlagS
    deriving (Show, Enum)
data NodisFlag = NodisFlagN | NodisFlagX | NodisFlagD | NodisFlagSpace
    deriving (Show, Enum)

data NonStandardDiscountRecord = NonStandardDiscountRecord
    { nsdOriginCode       :: Maybe T.Text
    , nsdDestinationCode  :: Maybe T.Text
    , nsdRouteCode        :: Maybe Int
    , nsdRailcardCode     :: Maybe T.Text
    , nsdTicketCode       :: Maybe T.Text
    , nsdEndDate          :: Maybe Day
    , nsdStartDate        :: Day
    , nsdQuoteDate        :: Day
    , nsdUseNlc           :: Maybe T.Text
    , nsdAdultNodisFlag   :: NodisFlag
    , nsdAdultAddOnAmount :: Maybe Int
    , nsdAdultRebookFlag  :: RebookFlag
    , nsdChildNodisFlag   :: NodisFlag
    , nsdChildAddOnAmount :: Maybe Int
    , nsdChildRebookFlag  :: RebookFlag
    } deriving Show

-- TTY: Ticket types (Page 23-25)
data TicketClass = TicketClass1 | TicketClass2 | TicketClass9
    deriving (Show, Enum)
data TicketType = Single | Return | Season
    deriving (Show, Enum)
data TicketGroup = First | Standard | Promotion | Euro
    deriving (Show, Enum)

data TicketReservationRequired
    = ReservationRequired
    | ReservationNotRequired
    | OutwardReservationRequired
    | ReturnReservationRequired
    | BothReservationRequired
    | EitherReservationRequired
    deriving (Show, Enum)

data TicketPackage
    = NotAPackage
    | SupplementsPackage
    | FaresPackage
    | FaresAndSupplementsPackage
    deriving (Show, Enum)

data TicketTypeRecord = TicketTypeRecord
    { ttTicketCode          :: T.Text
    , ttEndDate             :: Maybe Day
    , ttStartDate           :: Day
    , ttQuoteDate           :: Day
    , ttDescription         :: T.Text
    , ttClass               :: TicketClass
    , ttType                :: TicketType
    , ttGroup               :: TicketGroup
    , ttLastValidDay        :: Maybe Day
    , ttMaxPassengers       :: Int
    , ttMinPassengers       :: Int
    , ttMaxAdults           :: Int
    , ttMinAdults           :: Int
    , ttMaxChildren         :: Int
    , ttMinChildren         :: Int
    , ttRestrictedByDate    :: Bool
    , ttRestrictedByTrain   :: Bool
    , ttRestrictedByArea    :: Bool
    , ttValidityCode        :: T.Text
    , ttAtbDescription      :: T.Text
    , ttLulXLondonIssue     :: Int
    , ttReservationRequired :: TicketReservationRequired
    , ttCapriCode           :: T.Text
    , ttLul93               :: Bool
    , ttUtsCode             :: T.Text
    , ttTimeRestriction     :: Int
    , ttFreePassLul         :: Maybe Bool
    , ttPackage             :: TicketPackage
    , ttFareMultiplier      :: Int
    , ttDiscountCategory    :: T.Text
    } deriving Show

-- TVL: Ticket validity (Page 26-27)
data TicketValidityRecord = TicketValidityRecord
    { tvValidityCode      :: T.Text
    , tvEndDate           :: Maybe Day
    , tvStartDate         :: Day
    , tvDescription       :: T.Text
    , tvOutDays           :: Int
    , tvOutMonths         :: Int
    , tvRetDays           :: Int
    , tvRetMonths         :: Int
    , tvRetAfterDays      :: Int
    , tvRetAfterMonths    :: Int
    , tvRetAfterDay       :: Maybe Weekday
    , tvBreakOut          :: Bool
    , tvBreakRtn          :: Bool
    , tvOutDescription    :: T.Text
    , tvRtnDescription    :: T.Text
    } deriving Show

-- TJS: Journey segments (Page 28)
data JourneySegmentRecord = JourneySegmentRecord
    { jsCode      :: T.Text
    , jsEndDate   :: Maybe Day
    , jsStartDate :: Day
    , jsLine      :: Int
    , jsStart     :: Int
    , jsEnd       :: Int
    } deriving Show

-- TPB: Ticket publication data (Page 29)
data TicketPublicationRecord = TicketPublicationRecord
    { tpTicketCode          :: T.Text
    , tpPublicationSequence :: Int
    } deriving Show

-- TPN: Print formats (Page 30-31)
data SupplementCodeType
    = SupplementCode
    | TicketCode
    | RailcardCode
    | RailRoverCode
    deriving (Show, Enum)

data PrintFormatRecord = PrintFormatRecord
    { pfSupplementCode :: SupplementCodeType
    , pfRailcardCode   :: Maybe T.Text
    , pfRevCode        :: T.Text
    , pfNonRevCode     :: T.Text
    , pfTextCode1      :: T.Text
    , pfTextCode2      :: T.Text
    , pfTextCode3      :: T.Text
    , pfTextCode4      :: T.Text
    , pfTextCode5      :: T.Text
    } deriving Show

data TextRecord = TextRecord
    { txtCode     :: T.Text
    , txtFreeText :: T.Text
    } deriving Show

-- TCL: Class Legends (Page 32)
data ClassLegendRecord = ClassLegendRecord
    { clClass     :: Char
    , clEndDate   :: Maybe Day
    , clStartDate :: Day
    , clAtbDesc   :: T.Text
    , clCcDesc    :: T.Text
    } deriving Show

-- TRR: Rail Rovers (Page 33-34)
data RailRoverRecord = RailRoverRecord
    { rrRoverCode       :: T.Text
    , rrEndDate         :: Maybe Day
    , rrStartDate       :: Day
    , rrQuoteDate       :: Day
    , rrDescription     :: T.Text
    , rrTicketDesc      :: T.Text
    , rrCapriTicketCode :: Maybe T.Text
    , rrAccountingCode  :: T.Text
    , rrDaysTravel      :: Int
    , rrMonthsValid     :: Int
    , rrDaysValid       :: Int
    } deriving Show

data RoverPriceRecord = RoverPriceRecord
    { rpRoverCode       :: T.Text
    , rpEndDate         :: Maybe Day
    , rpRailcardCode    :: Maybe T.Text
    , rpRoverClass      :: TicketClass
    , rpAdultFare       :: Int
    , rpChildFare       :: Maybe Int
    , rpRestrictionCode :: Maybe T.Text
    } deriving Show

-- TPK: Packages (Page 35-36)
data SupplementDirection
    = OutwardDirection
    | ReturnDirection
    | BothDirections
    | EitherDirection
    deriving (Show, Enum)

data PackageRecord = PackageRecord
    { pkPackageCode           :: T.Text
    , pkEndDate               :: Maybe Day
    , pkStartDate             :: Day
    , pkQuoteDate             :: Day
    , pkRestrictionCode       :: Maybe T.Text
    , pkOriginFacilities      :: [Char]
    , pkDestinationFacilities :: [Char]
    } deriving Show

data PackageSupplementRecord = PackageSupplementRecord
    { pksPackageCode         :: T.Text
    , pksEndDate             :: Maybe Day
    , pksSupplementCode      :: T.Text
    , pksDirection           :: SupplementDirection
    , pksPackNumber          :: Int
    , pksOriginFacility      :: Maybe Char
    , pksDestinationFacility :: Maybe Char
    } deriving Show

-- SUP: Supplements (Page 37-41)
data AccomClass = FirstAccom | StandardAccom | BothAccom deriving (Show, Enum)
data Travellers = Adults | Children | AdultsAndChildren deriving (Show, Enum)

data SupplementRuleRecord = SupplementRuleRecord
    { srRuleNumber        :: T.Text
    , srEndDate           :: Maybe Day
    , srStartDate         :: Day
    , srQuoteDate         :: Day
    , srTrainUid          :: T.Text
    , srTrainUidDesc      :: T.Text
    , srFareClass         :: Maybe TicketClass
    , srQuota             :: Maybe Bool
    , srWeekendFirst      :: Maybe Bool
    , srSilverStandard    :: Maybe Bool
    , srRailcard          :: Maybe Bool
    , srCateringCode      :: Maybe Char
    , srSleeper           :: Maybe AccomClass
    , srAccomClass        :: Maybe AccomClass
    , srStatus            :: Travellers
    , srReservationStatus :: [Char]
    , srSectors           :: [Char]
    } deriving Show

-- todo: like four more record types x.x

-- RLC: Railcards (Page 42-43)
data HolderType = Adult | Child deriving (Show, Enum)

data RailcardValidity = ValidityMMDD Int Int
                      | ValidUntilDate Day
                      deriving Show

data RailcardRecord = RailcardRecord
    { rcRailcardCode      :: T.Text
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

-- RCM: Railcard Minimum Fares
-- DIS: Status Discounts
-- FRR: Rounding Rules
-- RST: Restrictions

-- LOC: Locations (Page 63-68)
data Region = NonBrOrLul | ER | LMR | SCR | SR | WR | LUL
    deriving (Show, Enum)

data LocationRecord = LocationRecord
    { locUicCode                :: T.Text
    , locEndDate                :: Maybe Day
    , locStartDate              :: Day
    , locQuoteDate              :: Day
    , locAdminAreaCode          :: T.Text
    , locNlcCode                :: T.Text
    , locDescription            :: T.Text
    , locCrsCode                :: T.Text
    , locResvCode               :: T.Text
    , locErsCountry             :: T.Text
    , locErsCode                :: T.Text
    , locFareGroup              :: T.Text
    , locCounty                 :: T.Text
    , locPteCode                :: T.Text
    , locZoneNo                 :: T.Text
    , locZoneInd                :: T.Text
    , locRegion                 :: Region
    , locHierarchy              :: Char
    , locCcDescOut              :: T.Text
    , locCcDescRtn              :: T.Text
    , locAtbDescOut             :: T.Text
    , locAtbDescRtn             :: T.Text
    , locSpecialFacilities      :: [Char]
    , locLulDirectionInd        :: Char
    , locLulUtsMode             :: Char
    , locLulZones               :: [Bool]
    , locLulUtsLondonStation    :: Bool
    , locUtsCode                :: T.Text
    , locUtsAltCode             :: T.Text
    , locUtsPtrBias             :: Char
    , locUtsOffset              :: Char
    , locUtsNorth               :: T.Text
    , locUtsEast                :: T.Text
    , locUtsSouth               :: T.Text
    , locUtsWest                :: T.Text
    } deriving Show

data TTGroupLocationRecord = TTGroupLocationRecord
    { ttGlGroupUicCode :: Int
    , ttGlEndDate      :: Maybe Day
    , ttGlStartDate    :: Day
    , ttGlQuoteDate    :: Day
    , ttGlDescription  :: T.Text
    , ttGlErsCountry   :: T.Text
    , ttGlErsCode      :: T.Text
    } deriving Show



-- RTE: Routes
-- TOC: TOC
-- TSP: TOC Specific Tickets
-- TAP: Advance Purchase Tickets
