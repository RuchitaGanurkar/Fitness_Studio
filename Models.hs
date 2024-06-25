{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveGeneric          #-}

module Models where

import           Data.Aeson
import           Data.Text
import           GHC.Generics (Generic)
import           Data.Aeson.Types (parseJSON, toJSON)
import           Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import           Database.PostgreSQL.Simple.ToField (ToField, toField)
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Data.Time (UTCTime, formatTime, defaultTimeLocale, parseTimeM)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.Types as Aeson
import qualified Data.Csv as CE


{-

User | Admin : Both can use the functionality of SIGN UP | LOGIN

User : Have the priviledge of choosing facility, choosing groups (if made available) and booking facilities based on requirements.

Admin : Have the priviledge of defining available facilities, facility groups (if available) and criteria on bookings.

User | Admin : Would be have to pass through authentication for accesing endpoints


-}
data User = User {
  user_code :: Int, -- to uniquely identify user
  user_name :: String,
  user_email :: String,
  user_password :: String,
  user_register_date :: UTCTime,
  user_facility :: Int, -- facilities that user wants to book
  user_group :: Int


} deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field


instance ToRow User where
  toRow (User _ user_name user_email user_password user_register_date user_facility user_group ) =
    toRow (user_name, user_email, user_password, user_register_date ,  user_facility, user_group )


data Admin = Admin {
  admin_code :: Int,  -- to uniquely identify admin
  admin_name :: String,
  admin_email :: String,
  admin_password :: String,
  admin_facility :: Int, -- facilities that admin has provided to the users
  admin_group :: Int

} deriving (Show, Generic)

instance ToJSON Admin
instance FromJSON Admin

instance FromRow Admin where
  fromRow = Admin <$> field <*> field <*> field <*> field <*> field <*> field


instance ToRow Admin where
  toRow (Admin _ admin_name admin_email admin_password  admin_facility admin_group ) =
    toRow (admin_name, admin_email, admin_password, admin_facility, admin_group )



data Facility = Facility {
  facility_id :: Maybe Int, -- uniquely identify available facility
  studio_id :: Int,
  name :: String,
  facility_type :: String,
  min_booking_duration :: Int -- minimum booking time available for facility to be booked by user
} deriving (Eq, Show, Generic)

instance FromJSON Facility


{-
Following instances are created for exporting facility
-}
instance CE.ToNamedRecord Facility
instance CE.DefaultOrdered Facility


instance ToJSON Facility where
  toJSON facility = object
    [ "facility_id" .= facility_id facility
    , "studio_id" .= studio_id facility
    , "name" .= name facility
    , "facility_type" .= facility_type facility
    , "min_booking_duration" .= min_booking_duration facility
    ]



instance FromRow Facility where
  fromRow = Facility <$> field <*> field <*> field <*> field <*> field


instance ToRow Facility where
  toRow (Facility _ studio_id name facility_type min_booking_duration ) =
    toRow (studio_id, name, facility_type, min_booking_duration)



data Booking = Booking {
  booking_id :: Int,
  facility_id_booking :: Int,
  user_id :: Int,
  slot_id :: Int,
  booking_date :: UTCTime,  -- date and time formate did not match with DB hence added following instances
  status :: String,
  booking_otp :: Maybe String
} deriving (Show, Generic)


instance ToJSON Booking where
  toJSON b = object
    [ "booking_id" .= booking_id b
    , "facility_id_booking" .= facility_id_booking b
    , "user_id" .= user_id b
    , "slot_id" .= slot_id b
    , "booking_date" .= formatTime defaultTimeLocale "%Y-%m-%d" (booking_date b)
    , "status" .= status b
    , "booking_otp" .= booking_otp b
    ]

instance FromJSON Booking where
  parseJSON = withObject "Booking" $ \v -> Booking
    <$> v .: "booking_id"
    <*> v .: "facility_id_booking"
    <*> v .: "user_id"
    <*> v .: "slot_id"
    <*> (v .: "booking_date" >>= parseDate)
    <*> v .: "status"
    <*> v .: "booking_otp"
    where
      parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"



instance FromRow Booking where
  fromRow = Booking <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Booking where
  toRow (Booking bookingId facilityId userId slotId bookingDate status otp) =
    toRow (bookingId, facilityId, userId, slotId, bookingDate, status, otp)



data CancelingReason = CancelingReason {
  reason :: String  -- if status has been changed to cancel, user need to specify reason 
} deriving (Show, Generic)

instance ToJSON CancelingReason
instance FromJSON CancelingReason


data BookingOTP = BookingOTP {
  booking_id_otp :: Int,
  otp :: String -- to confirm user booking and verified by admin
} deriving (Show, Generic)

instance ToJSON BookingOTP
instance FromJSON BookingOTP

instance FromRow BookingOTP where
  fromRow = BookingOTP <$> field <*> field

instance ToRow BookingOTP where
  toRow (BookingOTP bookingId otp) =
    toRow (bookingId, otp)


data BookingStatus = BookingStatus {
  book_id_status :: Int,
  booking_status :: String  -- to specify availability of slot, facility 
} deriving (Show, Generic)

instance ToJSON BookingStatus
instance FromJSON BookingStatus

instance FromRow BookingStatus where
  fromRow = BookingStatus <$> field <*> field


instance ToRow BookingStatus where
  toRow (BookingStatus book_id booking_status) =
    toRow (book_id, booking_status)


data Group = Group {  -- to club multiple facility for user
  group_id :: Maybe Int,
  facility_id_group :: Int,
  room_count :: Int,
  min_booking_duration_group :: Int -- Shared attribute for group
} deriving (Show, Generic)

instance ToJSON Group
instance FromJSON Group

instance FromRow Group where
  fromRow = Group <$> field <*> field <*> field <*> field

instance ToRow Group where
  toRow (Group gid fid rc mbd) =
    toRow (gid, fid, rc, mbd)


data DateDuration = DateDuration {
  duration_id :: Int, -- availability of particular slots, facility on date
  start_date :: UTCTime,
  end_date :: UTCTime
} deriving (Show, Generic)

instance ToJSON DateDuration


instance FromJSON DateDuration where
  parseJSON = withObject "DateDuration" $ \v -> DateDuration
    <$> v .: "duration_id"
    <*> (v .: "start_date" >>= parseDate)
    <*> (v .: "end_date" >>= parseDate)
    where
      parseDate :: String -> Aeson.Parser UTCTime
      parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"



instance ToRow DateDuration where
  toRow (DateDuration duration_id start end) = toRow (duration_id, start , end)

instance FromRow DateDuration where
  fromRow = DateDuration <$> field <*> field <*> field


data SearchQuery = SearchQuery {
  query :: String -- user search facility by name 
} deriving (Show, Generic)

instance ToJSON SearchQuery
instance FromJSON SearchQuery

data TimeRange = TimeRange {
  start_time :: UTCTime,  -- facility available to book in this range
  end_time :: UTCTime
} deriving (Show, Generic)

instance ToJSON TimeRange
instance FromJSON TimeRange


data FacilityRating = FacilityRating {  -- rate facility
  rating_id :: Maybe Int,
  facility_id_rating :: Int,
  user_id_rating :: Int,
  rating :: Int,
  review :: Text
} deriving (Show, Generic)

instance ToJSON FacilityRating
instance FromJSON FacilityRating

instance FromRow FacilityRating where
  fromRow = FacilityRating <$> field <*> field <*> field <*> field <*> field

instance ToRow FacilityRating where
  toRow (FacilityRating ratingId facilityIdRating userIdRating rating review) =
    [ toField ratingId
    , toField facilityIdRating
    , toField userIdRating
    , toField rating
    , toField review
    ]


data MaintenanceSlot = MaintenanceSlot {  -- specify slot in which particular facility can not be booked
  facility_id_maintenance :: Int,
  slot_id_maintenance :: Int,
  start_time_maintenance :: UTCTime,
  end_time_maintenance :: UTCTime
} deriving (Show, Generic)

instance ToJSON MaintenanceSlot
instance FromJSON MaintenanceSlot


instance FromRow MaintenanceSlot where
  fromRow = MaintenanceSlot <$> field <*> field <*> field <*> field

instance ToRow MaintenanceSlot where
  toRow (MaintenanceSlot slot_id facility_id start_time end_time) =
    toRow (slot_id, facility_id, start_time, end_time)