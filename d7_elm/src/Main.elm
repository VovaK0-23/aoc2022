module Main exposing (..)

import Browser
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Node exposing (Dir, DirId, Node, RootNode)
import Parser
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { input : String
    , showCard : Bool
    , root : RootNode
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , showCard = False
      , root = initRoot
      , error = ""
      }
    , Cmd.none
    )


initRoot =
    Node.createRoot "/" "/"



-- UPDATE


type Msg
    = FileRequested
    | FileSelected File
    | FileLoaded String
    | CardToggled


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileRequested ->
            ( model
            , Select.file [ "text/txt" ] FileSelected
            )

        FileSelected file ->
            ( model
            , Task.perform FileLoaded (File.toString file)
            )

        FileLoaded input ->
            ( case Parser.parse input initRoot of
                Ok root ->
                    { model
                        | input = input
                        , error = ""
                        , root = root
                        , showCard = True
                    }

                Err message ->
                    { model
                        | input = input
                        , error = message
                    }
            , Cmd.none
            )

        CardToggled ->
            ( { model | showCard = not model.showCard }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button ([ onClick FileRequested, style "margin" "1rem" ] ++ borderStyle) [ text "Load input file" ]
        , div []
            [ button ([ onClick CardToggled, style "margin" "0 1rem 1rem" ] ++ borderStyle) [ text "Toggle input" ]
            , text model.error
            , div []
                [ div [ style "display" "flex" ]
                    [ card
                        [ style "max-width" "40%", style "margin" "0 1rem" ]
                        model.showCard
                        model.input
                    , card
                        [ style "max-width" "40%", style "margin" "0 1rem" ]
                        model.showCard
                        ("$ ls /\n" ++ printNode model.root (Node.findNodeDepthLevel model.root))
                    , div []
                        [ card [ style "margin" "0 1rem" ] (model.input /= "") ("Part1: " ++ part1 model.root)
                        , card [ style "margin" "1rem" ] (model.input /= "") ("Part2: " ++ part2 model.root)
                        ]
                    ]
                ]
            ]
        ]


card : List (Attribute Msg) -> Bool -> String -> Html Msg
card attributes isVisible content =
    div
        (toStyle
            [ ( "display"
              , if isVisible then
                    "block"

                else
                    "none"
              )
            , ( "padding", "1rem" )
            , ( "max-height", "50vh" )
            , ( "height", "fit-content" )
            , ( "overflow", "auto" )
            ]
            ++ borderStyle
            ++ attributes
        )
        [ pre [ style "margin" "0" ] [ text content ] ]


borderStyle : List (Attribute msg)
borderStyle =
    toStyle
        [ ( "border", "1px solid black" )
        , ( "border-radius", "4px" )
        ]



-- HELPERS


part1 : RootNode -> String
part1 root =
    let
        dirsUnder100k =
            List.filter (\size -> size <= 100000) (calculateSizes root)
    in
    dirsUnder100k |> List.sum |> String.fromInt


part2 : RootNode -> String
part2 root =
    findDirToDelete root (calculateSizes root) |> Maybe.withDefault 0 |> String.fromInt


calculateSizes : RootNode -> List Int
calculateSizes root =
    let
        size node =
            List.sum
                (List.map (\file -> file.size) (Node.itemOf node).files)

        updateDict : RootNode -> Int
        updateDict node =
            Node.childrenOf node
                |> List.map updateDict
                |> List.sum
                |> (+) (size node)
    in
    List.map updateDict (Node.flatList root)
        |> List.sortBy (\x -> x)


findDirToDelete : RootNode -> List Int -> Maybe Int
findDirToDelete root sizes =
    let
        freeSpace =
            sizes
                |> List.reverse
                |> List.head
                |> Maybe.withDefault 0
                |> (-) 70000000

        neededSpace =
            30000000 - freeSpace

        suitableSizes =
            List.filter (\x -> x >= neededSpace) sizes
    in
    List.head suitableSizes


toStyle : List ( String, String ) -> List (Attribute msg)
toStyle list =
    list |> List.map (\( x, y ) -> style x y)


printNode : Node Dir -> (DirId -> Int) -> String
printNode (Node.Node dir children) findLevel =
    let
        indent =
            String.repeat (findLevel dir.id) "  "

        childLines =
            children
                |> List.map (\child -> printNode child findLevel)
                |> String.join "\n"

        printFile : Node.File -> String
        printFile file =
            indent ++ "  " ++ file.name ++ " (file, size=" ++ String.fromInt file.size ++ ")"

        fileLines =
            dir.files
                |> List.map printFile
                |> String.join "\n"
    in
    indent ++ dir.name ++ " (dir)\n" ++ fileLines ++ "\n" ++ childLines
