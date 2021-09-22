module Main where

-- FIXME пришлось сделать qualified чтобы не втащить функцию App.main от WeatherData
import qualified App

main :: IO ()
main = App.run
