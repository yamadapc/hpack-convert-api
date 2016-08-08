{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Hpack.Convert.API
  where

import           Control.Monad.IO.Class
import           Data.Aeson (Value(..), object, (.=))
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Hpack.Config
import           Hpack.Convert
import           Network.HTTP.Types.Status
import           System.Environment
import           Web.Spock

curlExample :: Text -> Text
curlExample host = "curl " <> host <> " -F \"cabalfile=@./hpack-convert-api.cabal\""

runHpackConvertApi :: IO ()
runHpackConvertApi = do
    port <- (fromMaybe 3000 . fmap read) <$> lookupEnv "PORT"
    host <- (Text.pack . fromMaybe ("localhost" <> show port)) <$> lookupEnv "HOST"
    runSpock port $ spockT id $ do
        get "/" $ do
            text $ Text.unlines [ "Available methods:"
                                , "POST /"
                                , "  Converts cabal files sent through the `cabalfile` field"
                                , "  to a package.yaml"
                                , "  Example:"
                                , "  " <> curlExample host'
                                ]
        post "/" $ do
            fs <- files
            case HashMap.lookup "cabalfile" fs of
                Nothing -> resError $ Text.unlines [ "Missing `cabalfile` parameter"
                                                   , "Example:"
                                                   , curlExample host'
                                                   ]
                Just UploadedFile{..} -> do
                    !epkg <- liftIO (fromPackageDescriptionString <$> readFile uf_tempLocation)
                    case epkg of
                        Left err -> resError "Invalid `cabalfile` .cabal file"
                        Right pkg -> bytes (encodePackage pkg)
  where
    resError e = do
        setStatus status422
        text e
