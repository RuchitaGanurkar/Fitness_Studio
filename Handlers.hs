{-# OPTIONS_GHC -Wno-unused-imports                   #-}
{-# OPTIONS_GHC -Wno-unused-do-bind                   #-}
{-# OPTIONS_GHC -Wno-unused-matches                   #-}
{-# OPTIONS_GHC -Wno-deprecations                     #-}
{-# OPTIONS_GHC -Wno-name-shadowing                   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns              #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables  #-}
{-# OPTIONS_GHC -Woverlapping-patterns                #-}


{-# LANGUAGE BlockArguments                           #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}


module Handlers (
  addFacility
, getFacilities
, getFacility
, updateFacility
, deleteFacility
, bookFacility
, cancelBooking
, getUserBookings
, getBooking
, getBookingStatus
, activateBooking
, createGroup
, addFacilityToGroup
, removeFacilityFromGroup
, deleteGroup
, setFacilityHoliday
, setGroupHoliday
, removeHoliday
, updateGroupFacility
, searchFacility
, searchFacilityByTime
, addFacilityRating
, getFacilityRatings
, addUser
, getUser
, addAdmin
, getAdmin
, exportFacilityData

) where


import Models
import Connection

import           Data.Time.Clock (UTCTime)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ( encode, ToJSON, object, (.=), toJSON )
import           Data.Maybe (listToMaybe)
import           Data.Text (pack, Text)
import           Web.Scotty(ActionM, json, param, raw, setHeader, rescue, raise, ScottyException)
import           Control.Exception (try, Exception, SomeException (SomeException))
import qualified Data.Csv as CE (encodeDefaultOrderedByName, DefaultOrdered, ToNamedRecord, decodeByName)
import qualified Database.PostgreSQL.Simple as D
import qualified Data.ByteString.Lazy as BL




addUser :: D.Connection -> User -> IO()
addUser conn user = do
 _ <-  D.execute conn "INSERT INTO users (user_name, user_email, user_password, user_register_date, user_selected_facility, user_selected_group) VALUES (?,?,?,?,?,?)"
  (user_name user, user_email user, user_password user, user_register_date user, user_facility user, user_group user)
 return ()


addAdmin :: D.Connection -> Admin -> IO()
addAdmin conn admin = do
 _ <- D.execute conn "INSERT INTO admins (admin_name, admin_email, admin_password, admin_given_facility, admin_created_group) VALUES (?,?,?,?,?)"
   (admin_name admin, admin_email admin, admin_password admin, admin_facility admin, admin_group admin)
 return()


getUser :: D.Connection -> IO [User]
getUser conn =
  D.query_ conn "SELECT * FROM users"


getAdmin :: D.Connection -> IO [Admin]
getAdmin conn =
  D.query_ conn "SELECT * FROM admins"



addFacility :: D.Connection -> Facility -> IO ()
addFacility conn facility = do
  _ <- D.execute conn "INSERT INTO facilities (studio_id, name, facility_type, min_booking_duration) VALUES (?, ?, ?, ?)"
    (studio_id facility, name facility, facility_type facility, min_booking_duration facility)
  return ()



getFacilities :: D.Connection -> IO [Facility]
getFacilities conn =
  D.query_ conn "SELECT * FROM facilities"



getFacility :: D.Connection -> Int -> IO (Maybe Facility)
getFacility conn facilityId = do
   result <- D.query conn "SELECT * FROM facilities WHERE facility_id = ?" (D.Only facilityId)
   return $ listToMaybe result


updateFacility :: D.Connection -> Int -> Facility -> IO ()
updateFacility conn facilityId facility = do
  D.execute conn "UPDATE facilities SET studio_id = ?, name = ?, facility_type = ? , min_booking_duration = ? WHERE facility_id = ?"
    (studio_id facility, name facility, facility_type facility, min_booking_duration facility, facilityId)
  return ()

deleteFacility :: D.Connection -> Int -> IO ()
deleteFacility conn facilityId = do
  D.execute conn "DELETE FROM facilities WHERE facility_id = ?" (D.Only facilityId)
  return ()



getUserBookings :: D.Connection -> IO [Booking]
getUserBookings conn =
  D.query_ conn "SELECT * FROM bookings"

bookFacility :: D.Connection -> Int -> Booking -> IO ()
bookFacility conn facilityId booking = do
  D.execute conn "INSERT INTO bookings (facility_id, user_id, slot_id, booking_date, status, otp) VALUES (?, ?, ?, ?, ?, ?)"
    (facilityId, user_id booking, slot_id booking, booking_date booking, status booking, booking_otp booking)
  return ()

getBooking :: D.Connection -> Int -> IO (Maybe Booking)
getBooking conn bookingId = do
  result <- D.query conn "SELECT * FROM bookings WHERE booking_id = ?" (D.Only bookingId)
  return $ case result of
    [booking] -> Just booking
    _         -> Nothing


cancelBooking :: D.Connection -> Int -> CancelingReason -> IO ()
cancelBooking conn bookingId cancelingReason = do
  D.execute conn "UPDATE bookings SET status = 'CANCELLED' WHERE booking_id = ? "
    (reason cancelingReason, bookingId )
  return ()

getBookingStatus :: D.Connection -> Int -> IO (Maybe BookingStatus)
getBookingStatus conn bookingId = do
  result <- D.query conn "SELECT status FROM bookings WHERE booking_id = ?" (D.Only bookingId)
  return $ case result of
    [booking] -> Just booking
    _         -> Nothing


activateBooking :: D.Connection -> BookingOTP -> IO ()
activateBooking conn bookingOTP = do
  D.execute conn "UPDATE bookings SET status = 'ACTIVE' WHERE booking_id = ? AND otp = ?"
    (booking_id_otp bookingOTP, otp bookingOTP)
  return ()


createGroup :: D.Connection -> Group -> IO ()
createGroup conn group = do
  D.execute conn "INSERT INTO groups (facility_id, room_count, min_booking_duration) VALUES (?, ?, ?)"
    (facility_id_group group, room_count group, min_booking_duration_group group)
  return ()

addFacilityToGroup :: D.Connection -> Int -> IO ()
addFacilityToGroup conn facilityId = do
  [D.Only (groupId :: Int)] <- D.query conn "SELECT facility_id FROM groups LIMIT 5" ()
  D.execute conn "UPDATE groups SET facility_id = ? WHERE group_id = ?" (facilityId :: Int , groupId)
  return ()


removeFacilityFromGroup :: D.Connection -> Int -> IO ()
removeFacilityFromGroup conn facilityId = do
  D.execute conn "UPDATE groups SET facility_id = NULL WHERE facility_id = ?" (D.Only facilityId)
  return ()


deleteGroup :: D.Connection -> Int -> IO ()
deleteGroup conn groupId = do
  D.execute conn "DELETE FROM groups WHERE group_id = ?" (D.Only groupId)
  return ()


updateGroupFacility :: D.Connection -> Facility -> IO ()
updateGroupFacility conn facility = do
  D.execute conn "UPDATE facilities SET studio_id = ?, name = ?, facility_type = ?, min_booking_duration = ? WHERE facility_id = ?"
    (studio_id facility, name facility, facility_type facility, min_booking_duration facility, facility_id facility)
  return ()


setFacilityHoliday :: D.Connection -> Int -> DateDuration -> IO ()
setFacilityHoliday conn facilityId dateDuration = do
  D.execute conn "INSERT INTO date_duration (facility_id, start_date, end_date) VALUES (?, ?, ?)"
    (facilityId, start_date dateDuration, end_date dateDuration)
  return ()

setGroupHoliday :: D.Connection -> Int -> DateDuration -> IO ()
setGroupHoliday conn groupId dateDuration = do
  D.execute conn "INSERT INTO date_duration (group_id, start_date, end_date) VALUES (?, ?, ?)"
    (groupId, start_date dateDuration, end_date dateDuration)
  return ()

removeHoliday :: D.Connection -> Int -> IO ()
removeHoliday conn durationId = do
  D.execute conn "DELETE FROM date_duration WHERE duration_id = ?" (D.Only durationId)
  return ()


searchFacility :: D.Connection -> SearchQuery -> IO [Facility]
searchFacility conn searchQuery =
  D.query conn "SELECT * FROM facilities WHERE name LIKE ?" (D.Only $ "%" ++ query searchQuery ++ "%")

searchFacilityByTime :: D.Connection -> TimeRange -> IO [Facility]
searchFacilityByTime conn timeRange =
  D.query conn "SELECT * FROM facilities WHERE facility_id IN (SELECT facility_id FROM time_slots WHERE start_time >= ? AND end_time <= ?)"
  (start_time timeRange, end_time timeRange)

addFacilityRating :: D.Connection -> Int -> FacilityRating -> IO ()
addFacilityRating conn facilityId facilityRating = do
  D.execute conn "INSERT INTO facility_ratings (facility_id, user_id, rating, review) VALUES (?, ?, ?, ?)"
    (facilityId, user_id_rating facilityRating, rating facilityRating, review facilityRating)
  return ()

getFacilityRatings :: D.Connection -> Int -> IO [FacilityRating]
getFacilityRatings conn facilityId =
  D.query conn "SELECT * FROM facility_ratings WHERE facility_id = ?" (D.Only facilityId)



exportFacilityData :: ActionM ()
exportFacilityData = do
  exportType <- (param "exportType" :: ActionM Text) `rescue` (\(_ :: ScottyException) -> return "csv")
  let export_type = exportType
  facilitiesOrError <- liftIO $ withDB getFacilities
  case facilitiesOrError of
    Left _ -> raise "Failed to fetch facilities data."
    Right facilities ->
      case export_type of
        "csv" -> do
          let csvFile = CE.encodeDefaultOrderedByName facilities
          setHeader "Content-Type" "text/csv"
          setHeader "Content-Disposition" "attachment; filename=facility.csv"
          raw csvFile


        "json" -> do
          let jsonFile = encode facilities
          setHeader "Content-Type" "application/json"
          setHeader "Content-Disposition" "attachment; filename=facility.json"
          raw jsonFile

    _ -> raise "Please state format to export data (csv / json / xlsx)"



-- http://localhost:3000/admin/export_facility_data/?exportType=csv
-- https://www.stackbuilders.com/blog/csv-encoding-decoding/

