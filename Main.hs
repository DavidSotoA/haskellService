{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson (Value(..), object, (.=))
import GHC.Generics
import Data.Maybe

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Control.Monad
import Control.Applicative
import Database.PostgreSQL.Simple.URL
import Control.Exception
import System.IO
import System.IO.Error
import Control.Monad.IO.Class
import Data.Text.Lazy.Encoding (decodeUtf8)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai (Application)

import qualified Web.Scotty as S

import System.Environment

connStr = "host=localhost dbname=pruebahaskell user=postgres password=postgres"
success= "success"
error'= "error"

data Menu = Menu { idMenu :: Maybe Int, name :: Maybe String, description :: Maybe String, price :: Maybe Int, restaurant :: Maybe Int } deriving (Show,Generic)
instance ToJSON Menu
instance FromJSON Menu

data Resultado= Resultado{tipo :: Maybe String, mensaje :: Maybe String} deriving (Show,Generic)
instance ToJSON Resultado

data Client= Client{username :: Maybe String,
                    nameClient :: Maybe String,
                    lastname :: Maybe String,
                    idClient:: Maybe String,
                    email:: Maybe String,
                    phone:: Maybe String,
                    cellphone:: Maybe String,
                    password:: Maybe String }
                    deriving (Show,Generic)

instance ToJSON Client
instance FromJSON Client

instance FromRow Menu where
  fromRow = Menu <$> field <*> field <*> field <*> field <*> field

instance ToRow Menu where
  toRow d = [toField (idMenu d), toField (name d), toField (description d), toField (price d), toField (restaurant d)]

instance FromRow Client where
  fromRow = Client <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Client where
  toRow d = [toField (username d),
             toField (nameClient d),
             toField (lastname d),
             toField (idClient d),
             toField (email d),
             toField (phone d),
             toField (cellphone d),
             toField (password d)]

getAllMenus :: Connection -> IO [Menu]
getAllMenus c = do
  list <- (query_ c "select * from menu" :: IO [Menu])
  return list

matchesId :: Int -> Menu -> Bool
matchesId id menu = case idMenu menu of
        Nothing -> False
        Just int -> int == id

nullValueClient :: Client->Bool
nullValueClient client=
  if isNothing (username client) || isNothing (nameClient client) || isNothing (lastname client) || isNothing (idClient client) || isNothing (email client) || isNothing (password client)
    then True
    else False

main = catch servicios handler

servicios :: IO ()
servicios =do
    putStrLn "Conectando..."
    conn <- connectPostgreSQL connStr
    putStrLn "conectao..."
    --env <- getEnvironment
    --let port = maybe 8080 read $ lookup "PORT" env
    S.scotty 8087 $ do
      S.get "/" $ do
        S.text "Bienvenido a un servicio REST construido con Haskell, ingrese a /menus para ver la lista de menus"

      S.get "/menus" $ do
        variable <- liftIO (getAllMenus conn)
        S.json variable

      S.get "/menus/:id" $ do
        id <- S.param "id"
        allMenus <- liftIO (getAllMenus conn)
        S.json (filter (matchesId id) allMenus)

      S.post "/menus" $ do
        menu <- (S.jsonData :: S.ActionM Menu)
        response <- liftIO (execute conn "insert into menu (name,description,price,restaurant) values (?,?,?,?)" ((name menu), (description menu), (price menu),(restaurant menu)))
        S.json (Resultado {tipo= Just success, mensaje= Just "menu agregado"})

      S.post "/cliente" $ do
        client <- (S.jsonData :: S.ActionM Client)
        --existUserName <- liftIO (query conn "select username from client where username='?'" (username client)) :: S.ActionM Client
        if nullValueClient client
          then S.json (Resultado {tipo= Just error', mensaje= Just "uno o mas campos estan vacios"})
          else
            if (isNothing (phone client)) && (isNothing (cellphone client))
              then do
                response <- liftIO (execute conn "insert into client (username,name,lastname,id,email,password) values (?,?,?,?,?,?)" ((username client),(nameClient client),(lastname client),(idClient client),(email client),(password client)))
                S.json (Resultado {tipo= Just success, mensaje= Just "cliente agregado"})
              else if isNothing (cellphone client)
                then do
                  response <- liftIO (execute conn "insert into client (username,name,lastname,id,email,phone,password) values (?,?,?,?,?,?,?)" ((username client),(nameClient client),(lastname client),(idClient client),(email client),(phone client),(password client)))
                  S.json (Resultado {tipo= Just success, mensaje= Just "cliente agregado"})
                else if (isNothing (phone client))
                  then do
                    response <- liftIO (execute conn "insert into client (username,name,lastname,id,email,cellphone,password) values (?,?,?,?,?,?,?)" ((username client),(nameClient client),(lastname client),(idClient client),(email client),(cellphone client),(password client)))
                    S.json (Resultado {tipo= Just success, mensaje= Just "cliente agregado"})
                  else do
                    response <- liftIO (execute conn "insert into client (username,name,lastname,id,email,phone,cellphone,password) values (?,?,?,?,?,?,?,?)" ((username client),(nameClient client),(lastname client),(idClient client),(email client),(phone client),(cellphone client),(password client)))
                    S.json (Resultado {tipo= Just success, mensaje= Just "cliente agregado"})

handler :: SqlError -> IO ()
handler err = putStrLn "entreeeeeeeeeeeeeeeeeeee"
----------------------------------------------------------------------------------------------------------------------------
pruebasUnitarias :: IO()
pruebasUnitarias= hspec spec

kkk=do
  S.get "/" $ do
    S.text ("Bienvenido a un servicio REST construido con Haskell, ingrese a /menus para ver la lista de menus")

serviciosPrueba :: IO Application
serviciosPrueba = do
  putStrLn "Conectando..."
  conn <- connectPostgreSQL connStr
  putStrLn "conectao..."
  S.scottyApp $ do

    S.get "/" $ do
      S.text ("Bienvenido a un servicio REST construido con Haskell, ingrese a /menus para ver la lista de menus")

    S.get "/menus" $ do
      variable <- liftIO (getAllMenus conn)
      S.json variable

    S.get "/menus/:id" $ do
      id <- S.param "id"
      allMenus <- liftIO (getAllMenus conn)
      S.json (filter (matchesId id) allMenus)

    S.post "/menus" $ do
      menu <- (S.jsonData :: S.ActionM Menu)
      response <- liftIO (execute conn "insert into menu (name,description,price,restaurant) values (?,?,?,?)" ((name menu), (description menu), (price menu),(restaurant menu)))
      S.json (Resultado {tipo= Just success, mensaje= Just "menu agregado"})


spec :: Spec
spec = with serviciosPrueba $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` "Bienvenido a un servicio REST construido con Haskell, ingrese a /menus para ver la lista de menus"
