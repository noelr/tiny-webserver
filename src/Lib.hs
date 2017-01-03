{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib (go) where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
import System.Process
import Data.Aeson
import Data.ByteString.Lazy as B
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import Data.Text
import Data.List
import Data.Maybe


data AppConfig = AppConfig { index :: Maybe Text
                           , static :: Maybe Text
                           , services :: [Service]
                           } deriving (Show, Generic)


instance FromJSON AppConfig where
  parseJSON (Object o) =
    AppConfig <$> o .:? "index"
              <*> o .:? "static"
              <*> o .:? "services" .!= []


data Service = Service { name :: Text, url :: Text, port :: Int } deriving (Show, Generic)
instance FromJSON Service
instance ToJSON Service


go :: IO ()
go = do
  conf <- eitherDecode <$> B.readFile "conf/services.json"
  case conf of
    Right conf -> run conf
    Left err -> print err


run :: AppConfig -> IO ()
run conf = do
  let names = fmap (Data.Text.unpack . name) (Lib.services conf)
  sequence_ $ fmap startFiles names
  let indexFile = fmap Data.Text.unpack (Lib.index conf)

  scotty 5000 $ do
    middleware logStdoutDev
    staticMiddleware $ fmap Data.Text.unpack (Lib.static conf)
    get "/services" $ Web.Scotty.json $ Lib.services conf
    get "/" $
      case indexFile of
        Nothing -> text "404 No index file defined"
        Just f -> file f


staticMiddleware s =
  case s of
    Nothing -> return ()
    Just f -> middleware $ staticPolicy (noDots >-> addBase f)


startFiles :: String -> IO ProcessHandle
startFiles s =
  case s of
    "files" -> spawnCommand "cd ../tiny-files/; stack exec tiny-files-exe"
    "append" -> spawnCommand "cd ../tiny-append/; stack exec tiny-append-exe"
    _ -> spawnCommand $ "echo 'Unknown service " ++ s ++ "'"
