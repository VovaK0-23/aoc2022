module Parser exposing (parse)

import Node exposing (File, RootNode, createChild, findNodeById, itemOf, updateDir)


type Line
    = Ls
    | Cd String
    | FileInfo String Int
    | SubDir String


parse : String -> RootNode -> Result String RootNode
parse input root =
    List.foldl parseLine ( Ok root, [] ) (parseInput input)
        |> Tuple.first


parseLine : Line -> ( Result String RootNode, List String ) -> ( Result String RootNode, List String )
parseLine line ( root_, path ) =
    case root_ of
        Ok root ->
            case line of
                Cd name ->
                    case name of
                        "/" ->
                            ( Ok root, [] )

                        ".." ->
                            ( Ok root, dropRight 1 path )

                        x ->
                            ( createSubdir x root path, path ++ [ x ] )

                SubDir name ->
                    ( createSubdir name root path, path )

                FileInfo name size ->
                    ( createFile name size root path, path )

                Ls ->
                    ( Ok root, path )

        Err m ->
            ( Err m, path )


parseInput : String -> List Line
parseInput input =
    input
        |> String.split "\n"
        |> List.filter (\line -> String.trim line /= "")
        |> List.map parseInputLine


parseInputLine : String -> Line
parseInputLine line =
    case String.split " " line of
        [ "$", "cd", name ] ->
            Cd name

        [ "dir", name ] ->
            SubDir name

        [ "$", "ls" ] ->
            Ls

        [ size, name ] ->
            FileInfo name (Maybe.withDefault 0 (String.toInt size))

        _ ->
            Ls


createSubdir : String -> RootNode -> List String -> Result String RootNode
createSubdir name root path =
    let
        dirId =
            List.foldr (\path_ name_ -> "/" ++ path_ ++ name_) ("/" ++ name) path
    in
    case findNodeById (currentDirId path) root of
        Just parent ->
            case findNodeById dirId root of
                Nothing ->
                    createChild dirId
                        (itemOf parent).id
                        name
                        []
                        root

                Just _ ->
                    Ok root

        Nothing ->
            Err ("Error creating Subdir: Cannot find parent " ++ currentDirId path)


createFile : String -> Int -> RootNode -> List String -> Result String RootNode
createFile name size root path =
    case findNodeById (currentDirId path) root of
        Just y ->
            updateDir (\dir -> { dir | files = File name size :: dir.files }) (itemOf y).id root

        Nothing ->
            Err ("Error creating File: Cannot find " ++ currentDirId path)


currentDirId path =
    case path of
        [] ->
            "/"

        _ ->
            List.foldr (\a b -> "/" ++ a ++ b) "" path


dropRight : Int -> List a -> List a
dropRight num list =
    list |> List.reverse |> List.drop num |> List.reverse
