{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PictureClickFromSvg where

#ifdef wasi_HOST_OS
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
#else
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (Response (responseBody), httpLbs, newManager, parseRequest)
import Network.HTTP.Client.TLS (tlsManagerSettings)
#endif

import Codec.Picture (encodePng)
import Control.Monad.State.Strict
import Data.Base64.Types (extractBase64)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Base64 (encodeBase64')
import Data.Function
import Data.Map qualified as Map
import Graphics.Svg (parseSvgFile)
import Graphics.Text.TrueType (Dpi)
import Language.Javascript.JSaddle
import Lib (generateSporcle, makePng)
import Miso hiding (events, initialAction, logLevel, model, mountPoint, subs, update, view)
import Miso.String

start :: JSM ()
-- TODO I'd prefer to use record syntax, but don't know how to do so while avoiding importing field selectors
start = startApp $ App initialModel update view [] defaultEvents NoOp Nothing Off

data Model = Model
    { output :: Maybe Output
    }
    deriving (Eq, Show)

data Output = Output
    { png :: MisoString
    , sporcle :: MisoString
    }
    deriving (Eq, Show)

data Action
    = NoOp
    | ReadFile
    | SetOutput Output
    deriving (Eq, Show)

initialModel :: Model
initialModel = Model{output = Nothing}

update :: Action -> Model -> Effect Action Model
update = \case
    NoOp -> pure
    ReadFile -> (#>) do
        doc <- parseSvgFile "dummySvgFilePath" <$> getSvgFile
        case doc of
            Nothing -> pure NoOp
            Just doc -> do
                png <- ms . extractBase64 . encodeBase64' . encodePng . fst <$> liftIO (makePng dpi doc)
                let sporcle = ms . fst $ generateSporcle doc
                pure $ SetOutput Output{..}
    SetOutput o -> \Model{} -> noEff Model{output = Just o, ..}

view :: Model -> View Action
view Model{..} =
    div_
        []
        [ form_
            []
            [ input_
                [ type_ "file"
                , id_ "fileInput"
                , accept_ "image/svg+xml"
                , onChange (const ReadFile)
                ]
            ]
        , br_ []
        , output & maybe (text "no PNG output") \Output{..} ->
            div_
                []
                [ img_
                    [ src_ $ "data:image/png;base64," <> png
                    , style_ $ -- TODO use separate stylesheet? how to load it with jsaddle-warp?
                        Map.fromList
                            [ ("height", "200px") -- TODO more responsive
                            ]
                    ]
                , br_ []
                , text sporcle
                ]
        ]

-- TODO this doesn't seem to make any difference...
-- report upstream? or just simplify my library?
dpi :: Dpi
dpi = 1

#ifdef wasi_HOST_OS
-- TODO inspired somewhat by https://github.com/dmjio/miso/blob/master/examples/file-reader/Main.hs
-- but we use message-passing since all attempts at passing args via FFI have caused runtime errors
-- if `onChange` give us access to the actual file objects from the underlying JS event, would all be simpler...
foreign import javascript safe "const reader = new FileReader(); reader.onload = function(e) { window.dispatchEvent(new CustomEvent('file-loaded', {detail: e.target.result})) }; reader.readAsText(document.getElementById('fileInput').files[0]);"
    getSvgFileJs :: IO ()
getSvgFile :: JSM ByteString
getSvgFile = do
    mv <- liftIO newEmptyMVar
    windowAddEventListener "file-loaded" $ liftIO . putMVar mv
    liftIO getSvgFileJs
    fmap fromMisoString . fromJSValUnchecked
        =<< unsafeGetProp "detail"
        =<< makeObject
        =<< liftIO (takeMVar mv)
#else
-- TODO obviously, this is a hack
-- though it's sort of fine as a proof-of-concept - we don't really care about the `jsaddle-warp` target that much
getSvgFile :: JSM ByteString
getSvgFile = liftIO do
    let url = "https://raw.githubusercontent.com/georgefst/picture-click-from-svg/f1b04e01693caadc7fef51342bbb9e41deb1c422/examples/oxford-colleges.svg"
    let req = fromMaybe (error "failed to parse static URL") $ parseRequest url
    q <- httpLbs req =<< newManager tlsManagerSettings
    pure $ BL.toStrict q.responseBody
#endif
