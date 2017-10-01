{-# LANGUAGE OverloadedStrings #-}
module MustacheTemplate (serveMustachTemplate, tupleToValue) where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Microstache as Stache
import qualified Network.Wai as Wai
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status200)
import qualified Data.Aeson as Aeson


(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

(+|+) :: Text -> Text -> Text
(+|+) = T.append


tupleToValue :: Aeson.ToJSON v => [(Text, v)] -> Aeson.Value
tupleToValue = Aeson.object . map (uncurry (Aeson..=))


-- TODO: handle case where file doesn't in a nice way.
serveMustachTemplate :: Aeson.Value -> Wai.Application
serveMustachTemplate values req continue = do
    template <- compileFile (Wai.pathInfo req)
    continue $ renderTemplate template values


renderTemplate :: Stache.Template -> Aeson.Value -> Wai.Response
renderTemplate template values =
    Stache.renderMustache template values
        |> encodeUtf8
        |> Wai.responseLBS status200 [(hContentType, "text/html")]


toFilePath :: [Text] -> String
toFilePath path =
    T.unpack $ "build/" +|+ T.intercalate "/" path


compileFile :: [Text] -> IO Stache.Template
compileFile =
    Stache.compileMustacheFile . toFilePath
