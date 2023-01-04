module Node exposing (..)

{-|

    Author of the original snippet: richardhaven https://github.com/richardhaven/elm-example-recursive-type

    This is a sample system for modeling recursive, hierarchical elements
    The definition of Node comes from Chad Gilbert in an answer on Stack Overflow
    on 5 October 2016
            type Node a = Node a (List (Node a))
            type alias Item = { id: String, text: String }
            map : (a -> b) -> Node a -> Node b
            map f (Node item children) =
                Node (f item) (List.map (map f) children)

-}


type alias DirId =
    String


type alias File =
    { name : String, size : Int }


type alias Dir =
    { id : DirId
    , parentId : DirId
    , name : String
    , files : List File
    }


type Node a
    = Node a (List (Node a))


type alias DirList =
    List (Node Dir)


type alias RootNode =
    Node Dir


itemOf : Node Dir -> Dir
itemOf (Node item _) =
    item


childrenOf : Node Dir -> DirList
childrenOf (Node _ children) =
    children


createRoot : DirId -> String -> RootNode
createRoot rootId rootName =
    Node
        { id = rootId
        , parentId = ""
        , name = rootName
        , files = []
        }
        []


{-| Creates a new Dir and returns the updated rootNode
-}
createChild : DirId -> DirId -> String -> List File -> RootNode -> Result String RootNode
createChild dirId parentId name files root =
    let
        newDir =
            Node
                { id = dirId
                , parentId = parentId
                , name = name
                , files = files
                }
                []
    in
    case getValidationErrors newDir root of
        Just errorMessage ->
            Err errorMessage

        Nothing ->
            Ok
                (nestedNodeMap
                    (\aNode ->
                        if (itemOf aNode).id == parentId then
                            Node (itemOf aNode) (newDir :: childrenOf aNode)

                        else
                            aNode
                    )
                    root
                )


getValidationErrors : Node Dir -> RootNode -> Maybe String
getValidationErrors newChild root =
    if findNodeById (itemOf newChild).id root /= Nothing then
        Just ("Dir id " ++ (itemOf newChild).id ++ " already exists")

    else if findNodeById (itemOf newChild).parentId root == Nothing then
        Just ("Parent id " ++ (itemOf newChild).parentId ++ " not found for " ++ (itemOf newChild).id)

    else
        Nothing


{-| Updates the indicated item and returns the new rootNode
{model | rootNode = updateChild (\\item -> {item | title = "Fred") "child1223" model.rootNode}
This wrapper around nestedDirMap prevents accidentally forgetting to test for a specific
Dir id and modifying every Dir.
We return a Result even though we could return the new RootNode for consistency with the other methods
Do NOT change the parentId here; use changeParent
-}
updateDir : (Dir -> Dir) -> DirId -> RootNode -> Result String RootNode
updateDir aFunction targetId root =
    let
        targetNodeMaybe =
            findNodeById targetId root
    in
    case targetNodeMaybe of
        Nothing ->
            Err ("Cannot find target item id " ++ targetId)

        Just (Node targetDir _) ->
            Ok
                (nestedDirMap
                    (\item ->
                        let
                            oldParentId =
                                item.parentId

                            oldId =
                                item.id
                        in
                        if item.id == targetDir.id then
                            (\anDir -> { anDir | id = oldId, parentId = oldParentId }) <| aFunction item

                        else
                            item
                    )
                    root
                )


isDirMember : DirId -> RootNode -> Bool
isDirMember targetId root =
    List.foldl
        (\node isFound ->
            if isFound then
                True

            else
                (itemOf node).id == targetId
        )
        False
        (flatList root)


{-| Returns a (flat) List of Dirs matching the title string
this can be an empty List
{model | searchResults = findNodesByTitle model.searchText model.rootNode}
-}
findNodesByName : String -> RootNode -> DirList
findNodesByName name root =
    let
        lowerTitle =
            String.toLower name
    in
    if String.length name == 0 then
        []

    else
        flatDirFilter (\anDir -> String.contains lowerTitle (String.toLower anDir.name)) root


{-| Returns a Maybe (Node Dir) with the found Dir or Nothing
-}
findNodeById : DirId -> RootNode -> Maybe (Node Dir)
findNodeById targetId root =
    let
        matches =
            List.filter (\aNode -> (itemOf aNode).id == targetId) (flatList root)
    in
    List.head matches


{-| Returns a depth level of a node
-}
findNodeDepthLevel : RootNode -> DirId -> Int
findNodeDepthLevel (Node dir children) dirId =
    if dir.id == dirId then
        1

    else
        let
            level =
                children
                    |> List.map (\child -> findNodeDepthLevel child dirId)
                    |> List.filter (\l -> l > 0)
                    |> List.head
        in
        case level of
            Just l ->
                l + 1

            Nothing ->
                0


{-| returns a single, non-nested List (like List.concatMap) with all items from every generation
-}
flatList : RootNode -> DirList
flatList root =
    let
        rest =
            List.concatMap (\child -> flatList child) (childrenOf root)
    in
    root :: rest


flatDirMap : (Dir -> Dir) -> RootNode -> DirList
flatDirMap aFunction root =
    List.map (\child -> Node (aFunction (itemOf child)) (childrenOf root)) (flatList root)


flatNodeMap : (Node Dir -> Node Dir) -> Node Dir -> DirList
flatNodeMap aFunction root =
    List.map (\aNode -> aFunction aNode) (flatList root)


flatDirFilter : (Dir -> Bool) -> RootNode -> DirList
flatDirFilter aFunction root =
    List.filter (\aNode -> aFunction (itemOf aNode)) (flatList root)


flatDirFoldl : (Dir -> a -> a) -> a -> RootNode -> a
flatDirFoldl aFunction acc root =
    List.foldl (\aNode -> aFunction (itemOf aNode)) acc (flatList root)


{-| returns a new version of the Node parameter (e.g. rootNode)
with all nodes' items coming through the passed function
-}
nestedDirMap : (Dir -> Dir) -> RootNode -> RootNode
nestedDirMap aFunction root =
    let
        newChildren =
            List.map (\child -> nestedDirMap aFunction child) (childrenOf root)
    in
    Node (aFunction (itemOf root)) newChildren


{-| returns a new version of the Node parameter (e.g. rootNode)
with all nodes coming through the passed function
createChild and deleteChild use this to alter the children List
of the child's parent
-}
nestedNodeMap : (Node Dir -> Node Dir) -> RootNode -> RootNode
nestedNodeMap aFunction root =
    let
        newChildren =
            List.map (\child -> nestedNodeMap aFunction child) (childrenOf root)
    in
    aFunction (Node (itemOf root) newChildren)


{-| returns a new version of the Node parameter (e.g. rootNode)
as a Maybe with only those nodes approved by the function.
The result must be a Maybe because even the root might be omitted.
Note that any omitted Node's children are also omitted even if they
would have been approved.
-}
nestedDirFilter : (Dir -> Bool) -> RootNode -> Maybe (Node Dir)
nestedDirFilter aFunction root =
    let
        newChildren =
            List.filterMap (\child -> nestedDirFilter aFunction child) (childrenOf root)
    in
    if aFunction (itemOf root) then
        Just (Node (itemOf root) newChildren)

    else
        Nothing
