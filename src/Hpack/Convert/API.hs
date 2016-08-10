{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Hpack.Convert.API
  where

import           Control.Monad.IO.Class
import           Data.Aeson                    (Value (..), object, (.=))
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Hpack.Config
import           Hpack.Convert
import           Network.HTTP.Types.Status
import           System.Environment
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Hamlet
import           Web.Spock

curlExample :: Text -> Text
curlExample host = "curl " <> host <> " -F \"cabalfile=@./hpack-convert-api.cabal\""

getHome host = do
    lazyBytes $ renderHtml $ [shamlet|
<html>
  <head>
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
  <body>
    <div class="container">
      <h1> Hpack Convert
      <h4> Convert with <code>curl</code>
      <pre><code>#{curlExample host}</code></pre>
      <h4> Paste your cabal file bellow
      <.row>
        <.col-md-6>
          <form action="/form" method="POST">
            <.form-group>
              <textarea .form-control name="cabalfile" id="cabalfile" rows="30" style="resize: none; min-height: 50vh;" />
            <.form-group>
              <button class="btn btn-primary" type="submit">
                Convert
        <.col-md-6>
          <pre><code class="result"># Result</code></pre>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.1.0/jquery.min.js" />
    <script>
      \$('form').submit(function(e) {
        e.preventDefault();
        \$.post('/form', {cabalfile: $('textarea').val()}, function(res) {
          \$('.result').text(res);
        }).fail(function(err) {
          \$('.result').text(JSON.stringify(err, null, 2));
        });
      });
        |]

    -- text $
    --     Text.unlines [ "Available methods:"
    --                  , "POST /"
    --                  , "  Converts cabal files sent through the `cabalfile` field"
    --                  , "  to a package.yaml"
    --                  , "  Example:"
    --                  , "  " <> curlExample host
    --                  ]

runHpackConvertApi :: IO ()
runHpackConvertApi = do
    port <- (fromMaybe 3000 . fmap read) <$> lookupEnv "PORT"
    host <- (Text.pack . fromMaybe ("localhost:" <> show port)) <$> lookupEnv "HOST"
    runSpock port $ spockT id $ do
        get "/" (getHome host)

        post "/form" $ do
            pkg <- param' "cabalfile"
            let !epkg = fromPackageDescriptionString pkg
            case epkg of
                Left err -> resError "Invalid `cabalfile` .cabal file"
                Right pkg -> bytes (encodePackage pkg)

        post "/" $ do
            fs <- files
            case HashMap.lookup "cabalfile" fs of
                Nothing -> resError $ Text.unlines [ "Missing `cabalfile` parameter"
                                                   , "Example:"
                                                   , curlExample host
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
