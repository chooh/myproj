{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import System.Environment
import System.IO

-- * api

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item

type WeatherAPI = "weather" :> QueryParam "q" CityName :> QueryParam "appid" APIkey :> Get '[JSON] WeatherData

data WeatherData = WeatherData {
    base :: String,
    --clouds: {all: 100}
    cod :: Int,
    coord :: Coord,
    dt :: Int,
    id :: Int,
    main :: WeatherMain,
    name :: CityName,
    --sys: {type: 2, id: 2019646, country: "GB", sunrise: 1632289602, sunset: 1632333586}
    timezone :: Int,
    visibility :: Int
    --weather: [{id: 804, main: "Clouds", description: "overcast clouds", icon: "04d"}]
} deriving (Eq, Show, Generic)

instance ToJSON WeatherData
instance FromJSON WeatherData

type CityName = String

type APIkey = String

data Coord = Coord { lat :: Float
                   , lon :: Float
                   } deriving (Eq, Show, Generic)

instance ToJSON Coord
instance FromJSON Coord

data WeatherMain = WeatherMain {
    temp :: Float,
    feels_like :: Float,
    temp_min :: Float,
    temp_max :: Float,
    pressure :: Float,
    humidity :: Float
  } deriving (Eq, Show, Generic)

instance ToJSON WeatherMain
instance FromJSON WeatherMain

weatherApi :: Proxy WeatherAPI
weatherApi = Proxy

itemApi :: Proxy ItemApi
itemApi = Proxy

getCurrentWeather = client weatherApi
getCurrentWeatherInLondon = getCurrentWeather (Just "London")

runQ = do
  manager <- newManager tlsManagerSettings
  -- FIXME читать переменные окружения надо где-то в другом месте
  appid <- lookupEnv "API_KEY"
  res <- runClientM (getCurrentWeatherInLondon appid) (mkClientEnv manager (BaseUrl Https "api.openweathermap.org" 443 "/data/2.5"))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (weather) -> do
      print weather

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  getItems :<|>
  getItemById

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \ case
  0 -> return exampleItem
  _ -> throwError err404

exampleItem :: Item
exampleItem = Item 0 "example item"

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data a + b = Foo a b

type X = Int + Bool
