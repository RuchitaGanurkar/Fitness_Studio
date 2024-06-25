{-# OPTIONS_GHC -Wno-unused-do-bind                  #-}
{-# OPTIONS_GHC -Wno-deprecations                    #-}
{-# OPTIONS_GHC -Wno-unused-imports                  #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}


{-# LANGUAGE ScopedTypeVariables                     #-}
{-# LANGUAGE OverloadedStrings                       #-}

module Routes where

import Handlers (
  addFacility,
  getFacilities,
  getFacility,
  updateFacility,
  deleteFacility,
  bookFacility,
  cancelBooking,
  getUserBookings,
  getBookingStatus,
  activateBooking,
  createGroup, 
  addFacilityToGroup,
  removeFacilityFromGroup,
  deleteGroup,
  removeHoliday,
  setGroupHoliday,
  updateGroupFacility,
  setFacilityHoliday,
  searchFacility,
  searchFacilityByTime,
  addFacilityRating, 
  getFacilityRatings,
  addUser,
  addAdmin,
  getUser,
  getAdmin,
  exportFacilityData
   
  )

import Connection (withDB)
import Models ( Facility (facility_id) , Booking, CancelingReason, BookingOTP, Group, DateDuration, TimeRange, SearchQuery, FacilityRating, User, Admin)

import Web.Scotty (liftIO, ScottyM, status, raise, rescue, json, jsonData, post, get, param, put, delete, ActionM, ActionM, raw, setHeader, ScottyException)
import Control.Exception (try, Exception, SomeException (SomeException))
import Network.HTTP.Types (status500)
import Data.Time (getCurrentTime)
import Network.HTTP.Types.Status





appRoutes :: ScottyM ()
appRoutes = do

  get "/user" $ do
    result <- liftIO $ withDB getUser 
    case result of 
      Left _ -> raise "Failed To Fetch List Of User"
      Right users -> json users



  get "/admin" $ do
    
    result <- liftIO $ withDB getAdmin 
    case result of 
      Left _ -> raise "Failed To Fetch List Of Admin"
      Right admins -> json admins



  get "/admin/facilities" $ do
    
    result <- liftIO $ withDB getFacilities
    case result of 
      Left err -> do
        status status500 
        json $ show err
      Right facilities -> json facilities




  get "/admin/facility/:facility_id" $ do
    
    facilityId  <- param "facility_id"
    result <- liftIO $ withDB $ \connect ->  getFacility connect facilityId
    case result of 
      Left err -> do 
        status status500
        json $ show err 
      Right Nothing -> do 
        status status400 
        json ("Facility Not Found" :: String) 
      Right (Just facility) -> json facility
 
 


  post "/user" $ do 
    users <- jsonData `rescue` (\(_ :: ScottyException) -> raise "Invalid User Data") :: ActionM User 
    result <- liftIO $ withDB $ \connect -> addUser connect users
    case result of 
      Left err -> do 
        status status500
        json $ show err 
      Right _ -> json users


  post "/admin" $ do 
    admins <- jsonData `rescue` (\(_ :: ScottyException)-> raise "Invalid Admin Data") :: ActionM Admin 
    result <- liftIO $ withDB $ \connect -> addAdmin connect admins
    case result of 
      Left err -> do 
        status status500
        json $ show err 
      Right _ -> json admins



  post "/admin/facility" $ do
    facility <- jsonData `rescue` (\(_ :: ScottyException) -> raise "Invalid Facility Data" ) :: ActionM Facility
    result <- liftIO $ withDB $ \connect -> addFacility connect facility
    case result of 
      Left err -> do 
        status status500
        json $ show err 
      Right _ -> json facility



  put "/admin/facility/:facility_id" $ do
    facilityId <- param "facility_id"
    facility <- jsonData `rescue` (\(_ :: ScottyException ) -> raise "Invalid Facility Id" )
    result <- liftIO $ withDB $ \connect ->  updateFacility connect facilityId facility
    case result of 
      Left err -> do 
        status status500
        json $ show err 
      Right _ -> json facility



  delete "/admin/facility/:studioId" $ do
    studioId <- param "studioId"
    result <- liftIO $ withDB $ \connect -> deleteFacility connect studioId
    case result of 
      Left err -> do 
        status status500
        json $ show err 
      Right _ -> json ("Facility Deleted As Per Studio Id" :: String)




  get "/user/bookings" $ do
   
    result <- liftIO $ withDB $ \connect -> getUserBookings connect
    case result of 
      Left err -> do 
        status status500 
        json $ show err 
      Right bookings -> json bookings





  post "/user/facility/:facility_id/book" $ do
    facilityId <- param "facility_id"
    booking <- jsonData `rescue` (\(_ :: ScottyException) -> raise  "Invalid Reason" ) :: ActionM Booking
    result <- liftIO $ withDB $ \connect -> bookFacility connect facilityId booking
    case result of
      Left err -> do 
        status status500 
        json $ show err 
      Right _ -> json booking 




  post "/user/cancel_booking/:booking_id" $ do
   
    bookingId <- param "booking_id"
    cancelingReason <- jsonData `rescue` (\ (_  :: ScottyException)-> raise "Invalid Reason" ) :: ActionM CancelingReason
    result <- liftIO $ withDB $ \connect -> cancelBooking connect bookingId cancelingReason
    case result of
      Left err -> do
        status status500
        json $ show err
      Right _ -> json ("Booking Cancelled" :: String)





  get "/user/booking_status/:booking_id" $ do
   
    bookingId <- param "booking_id"
    result <- liftIO $ withDB $ \connect -> getBookingStatus connect bookingId
    case result of 
      Left err -> do 
        status status500 
        json $ show err 
      Right Nothing -> do 
        status status404 
        json ("Booking Not Found" :: String)
      Right (Just bookingStatus) -> json bookingStatus





  post "/user/activate_booking" $ do
   
    bookingOtp <- jsonData `rescue` (\(_ :: ScottyException) -> raise "Invalid OTP" ):: ActionM BookingOTP
    result <- liftIO $ withDB $ \connect -> activateBooking connect bookingOtp
    case result of
      Left err -> do
        status status500
        json $ show err
      Right _ -> json ("Booking activated" :: String)





  post "/admin/facility/group" $ do
   
    group <- jsonData `rescue` (\(_ :: ScottyException) -> raise "Invalid Group") :: ActionM Group
    result <- liftIO $ withDB $ \connect -> createGroup connect group
    case result of
      Left err -> do
        status status500
        json $ show err
      Right _ -> json group





  post "/admin/add_facility_to_group/:facility_id" $ do
    
    facilityId <- param "facility_id"
    result <- liftIO $ withDB $ \connect -> addFacilityToGroup connect facilityId
    case result of
      Left err -> do
        status status500
        json $ show err
      Right _ -> json ("Facility added to group" :: String)







  post "/admin/remove_facility_from_group/:facility_id" $ do
   
    facilityId <- param "facility_id"
    result <- liftIO $ withDB $ \connect -> removeFacilityFromGroup connect facilityId
    case result of
     Left err -> do
       status status500
       json $ show err
     Right _ -> json ("Facility Removed From Group" :: String)




  delete "/admin/group/:group_id" $ do
   
    groupId <- param "group_id"
    result <- liftIO $ withDB $ \connect -> deleteGroup connect groupId
    case result of 
      Left err -> do 
        status status500 
        json $ show err 
      Right _ -> json ("Group Deleted" :: String)
 






  put "/admin/update_group_facility" $ do
    
    facility <- jsonData `rescue` (\(_ :: ScottyException) -> raise "Invalid Facility Group") :: ActionM Facility
    result <- liftIO $ withDB $ \connect ->  updateGroupFacility connect facility
    case result of
      Left err -> do
        status status500 
        json $ show err 
      Right _ -> json facility
    
    






  post "/admin/facility_holiday/:facility_id" $ do
    
    facilityId <- param "facility_id"
    dateDuration <- jsonData `rescue` (\(_ :: ScottyException)-> raise "Invalid Date" ):: ActionM DateDuration
    result <- liftIO $ withDB $ \connect ->  setFacilityHoliday connect facilityId dateDuration
    case result of
      Left err -> do
        status status500
        json $ show err
      Right _ -> json ("Holiday Set For Facility" :: String)






  post "/admin/group_holiday/:group_id" $ do
    
    groupId <- param "group_id"
    dateDuration <- jsonData `rescue` (\(_ :: ScottyException) -> raise "Invalid Date") :: ActionM DateDuration
    result <- liftIO $ withDB $ \connect ->  setGroupHoliday connect groupId dateDuration
    case result of
      Left err -> do
        status status500
        json $ show err
      Right _ -> json ("Holiday Set For Group" :: String)




  delete "/admin/holiday/:duration_id" $ do
    
    durationId <- param "duration_id"
    result <- liftIO $ withDB $ \connect ->  removeHoliday connect durationId
    case result of
      Left err -> do
        status status500
        json $ show err
      Right _ -> json ("Holiday Removed" :: String)




  post "/user/search_facility" $ do
   
    searchQuery <- jsonData  `rescue` (\(_ :: ScottyException) -> raise "Invalid Search Term") :: ActionM SearchQuery
    result <- liftIO $ withDB $ \connect -> searchFacility connect searchQuery
    case result of 
      Left err -> do 
        status status500
        json $ show err
      Right facilities -> json facilities
    



  post "/user/search_facility_by_time" $ do
   
    timeRange <- jsonData `rescue`(\(_ :: ScottyException) -> raise "Invalid Time" ):: ActionM TimeRange
    result <- liftIO $ withDB $ \connect ->  searchFacilityByTime connect timeRange
    case result of 
      Left err -> do 
        status status500
        json $ show err
      Right facilities -> json facilities
    



  post "/user/rate_facility/:facility_id" $ do
    
    facilityId <- param "facility_id"
    facilityRating <- jsonData `rescue` (\(_ :: ScottyException) -> raise "Invalid Facility Rating" ) :: ActionM FacilityRating
    result <- liftIO $ withDB $ \connect -> addFacilityRating connect facilityId facilityRating
    case result of
      Left err-> do
        status status500
        json $ show err
      Right _ -> json facilityRating




  get "/user/facility_ratings/:facility_id" $ do
   
    facilityId <- param "facility_id"
    result <- liftIO $ withDB $ \connect ->  getFacilityRatings connect facilityId
    case result of 
      Left err -> do 
        status status500 
        json $ show err 
      Right ratings -> json ratings





  get "/admin/export_facility_data" $ do
    exportFacilityData

