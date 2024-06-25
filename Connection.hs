{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE OverloadedStrings      #-}

module Connection (initDB, withDB, query_, execute, execute_, Config(..)) where

import           Data.Int
import           Control.Exception (SomeException, try)
import qualified Database.PostgreSQL.Simple as D





-- Database Configuration
data Config = Config { connectionString :: D.ConnectInfo }

-- Database Parameters
initDB :: IO D.Connection
initDB = D.connect D.defaultConnectInfo {
    D.connectHost = "localhost",
    D.connectDatabase = "fitness_studio_main",
    D.connectUser = "postgres",
    D.connectPassword = "qwerty"
  }



-- Initialise Database With Exception
withDB :: (D.Connection -> IO a) -> IO (Either SomeException a)
withDB action = do
    conn <- initDB
    result <- try (action conn)
    D.close conn
    return result




-- Query Command With Exception
query_ :: (D.FromRow r, D.ToRow q) => D.Query -> q -> IO ( Either SomeException [r] )
query_ q params = withDB $ \conn -> D.query conn q params



-- Execute Command With Exception (Return Number Of Rows Affected)
execute :: D.ToRow q => D.Query -> q -> IO ( Either SomeException Int64 )
execute q params = withDB $ \conn -> D.execute conn q params



-- Execute_ Command With Exception
execute_ :: (D.ToRow q) => D.Query -> q -> IO ( Either SomeException Int64 )
execute_ q params = withDB $ \conn -> D.execute conn q params



-- Followed official documentation