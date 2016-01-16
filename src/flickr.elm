import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)
import Time exposing (..)
import Window

-- Types

type alias HttpTask = Task Http.Error String
type alias FlickrSearchArgs = List (String, String)
type alias Url = String
type alias Keyword = String
type alias WindowDims = (Int, Int)

type alias Photo =
  { id : String
  , title : String
  }

type alias Size =
  { source : String
  , width : Int
  , height : Int
  }

-- VIEW

view : Int -> Keyword -> Url -> Html
view height keyword imgUrl =
  div [ style (imgStyle height imgUrl) ]
    [ input
      [ placeholder "Github User Name"
      , Attr.value keyword
      , on "input" targetValue (Signal.message queryMailbox.address)
      , style myStyle
      ]
      []
    ]

myStyle : List (String, String)
myStyle =
  [ ("width", "100%")
  , ("height", "40px")
  , ("padding", "10px 0")
  , ("font-size", "2em")
  , ("text-align", "center")
  ]

imgStyle : Int -> Url -> List (String, String)
imgStyle height src =
  [ ("background-image", "url('" ++ src ++ "')")
  , ("background-repeat", "no-repeat")
  , ("background-attachment", "fixed")
  , ("background-position", "center")
  , ("width", "100%")
  , ("height", toString height ++ "px")
  ]

-- WIRING

main : Signal Html
main =
  Signal.map3 view Window.height queryMailbox.signal resultsMailbox.signal

resultsMailbox : Signal.Mailbox String
resultsMailbox =
  Signal.mailbox ""

queryMailbox : Signal.Mailbox String
queryMailbox =
  Signal.mailbox "Jupiter"

port requestImgs : Signal (Task Http.Error ())
port requestImgs =
  let
    imageRetrievalSignal = createSignalForImageRetrievalTask getImage queryMailbox.signal
  in
    Signal.sampleOn queryMailbox.signal imageRetrievalSignal
      |> Signal.map (\httpTask -> httpTask `andThen` Signal.send resultsMailbox.address)

createSignalForImageRetrievalTask : (WindowDims -> Keyword -> HttpTask) -> Signal Keyword -> Signal (HttpTask)
createSignalForImageRetrievalTask getImage querySignal =
    createHttpTaskSignal getImage Window.dimensions querySignal

createHttpTaskSignal : (WindowDims -> Keyword -> HttpTask) -> Signal WindowDims -> Signal Keyword -> Signal (HttpTask)
createHttpTaskSignal getImage sampled events =
  Signal.map2 getImage sampled events

-- Given the current window dimensions and a search keyword, get a list of photos
-- from Flickr, grab the first photo, find the available sizes for that photo,
-- and return a sized photo that most appropriately fits the window dimensions.
getImage : WindowDims -> Keyword -> HttpTask
getImage dimensions keyword =
  let searchArgs =
    [ ("sort", "random"), ("per_page", "10"), ("tags", keyword) ]
  in
    Http.get photoListDecoder (getFlickrUrlFor "search" searchArgs)
      `andThen` selectPhoto
      `andThen` \photo ->
        Http.get sizeListDecoder (getFlickrUrlFor "getSizes" [ ("photo_id", photo.id) ])
      `andThen` pickSize dimensions

-- JSON DECODERS

photoListDecoder : Json.Decoder (List Photo)
photoListDecoder =
  Json.at ["photos","photo"] <| Json.list <| photoDecoder

photoDecoder : Json.Decoder Photo
photoDecoder =
  Json.object2 Photo
    ("id" := Json.string)
    ("title" := Json.string)

sizeListDecoder : Json.Decoder (List Size)
sizeListDecoder =
  Json.at ["sizes","size"] <| Json.list <| sizeDecoder

sizeDecoder : Json.Decoder Size
sizeDecoder =
  let number =
    Json.oneOf [ Json.int, Json.customDecoder Json.string String.toInt ]
  in
    Json.object3 Size
      ("source" := Json.string)
      ("width" := number)
      ("height" := number)

--  FLICKR URLS

getFlickrUrlFor : String -> FlickrSearchArgs -> Url
getFlickrUrlFor method searchArgs =
  Http.url "https://api.flickr.com/services/rest/" <|
    [ ("format", "json")
    , ("nojsoncallback", "1")
    , ("api_key", "9be5b08cd8168fa82d136aa55f1fdb3c")
    , ("method", "flickr.photos." ++ method)
    ] ++ searchArgs

-- HANDLE RESPONSES

selectPhoto : List Photo -> Task Http.Error Photo
selectPhoto photos =
  case photos of
    photo :: _ -> succeed photo
    [] -> fail (Http.UnexpectedPayload "expecting 1 or more photos from Flickr")

pickSize : (Int,Int) -> List Size -> HttpTask
pickSize (width,height) sizes =
  let sizeRating size =
    let penalty =
      if size.width > width || size.height > height then 400 else 0
    in
      abs (width - size.width) + abs (height - size.height) + penalty
  in
    case List.sortBy sizeRating sizes of
      size :: _ -> succeed size.source
      [] -> fail (Http.UnexpectedPayload "expecting 1 or more image sizes to choose from")
