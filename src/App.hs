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
import Control.Monad.IO.Class

-- * api

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

getCurrentWeatherClient = client weatherApi

--- FIXME Как бы так ещё сделать функцию, чтобы на вход был только город, а на выходе уже
-- Either WeatherData:
--getCurrentWeather` city = do
  --manager <- newManager tlsManagerSettings
  --appid <- lookupEnv "API_KEY"
  --res <- runClientM (getCurrentWeatherClient (Just city ) appid) (mkClientEnv manager (BaseUrl Https "api.openweathermap.org" 443 "/data/2.5"))

runQ = do
  manager <- newManager tlsManagerSettings
  -- FIXME читать переменные окружения надо где-то в другом месте
  appid <- lookupEnv "API_KEY"
  res <- runClientM (getCurrentWeatherClient (Just "London") appid) (mkClientEnv manager (BaseUrl Https "api.openweathermap.org" 443 "/data/2.5"))
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
mkApp = return $ serve weatherApi server

server :: Server WeatherAPI
server =
  getCurrentWeatherServer

getCurrentWeatherServer :: Maybe CityName -> Maybe APIkey -> Handler WeatherData
getCurrentWeatherServer _ _ = do
    filecontent <- liftIO (readFile "weather.json")
    return (WeatherData filecontent)

--getCurrentWeatherServer city _ = do
  --manager <- newManager tlsManagerSettings
  --appid <- lookupEnv "API_KEY"
  --runClientM (getCurrentWeatherClient city appid) (mkClientEnv manager (BaseUrl Https "api.openweathermap.org" 443 "/data/2.5"))
