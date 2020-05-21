port module Messages exposing (Msg(..),update,saveToStorage,save)

import Model exposing (Model,State(..),newGame)
import Dict exposing (Dict)
import Color exposing (..)
import Grid exposing (..)
import Time
import Set exposing (Set)

port save : String -> Cmd msg

saveToStorage : Model -> (Model, Cmd Msg)
saveToStorage model =
    (model, save (Model.encode 2 model))

-- Update

type Msg
    = Flag (Float,Float)
    | Reveal (Float,Float)
    | NewGame
    | Tick Time.Posix
    | Noop
    | Pause
    | Continue
    | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Flag (x,y) ->
            if model.status == Playing || model.status == New then
                (flag (x,y) model, Cmd.none)
            else
                (model,Cmd.none)
        
        Reveal (x,y) ->
            if model.status == Playing || model.status == New then
                (reveal (x,y) model, Cmd.none)
            else
                (model,Cmd.none)
        
        NewGame ->
            if model.status == Lost || model.status == Won then
                (newModel model, Cmd.none)
            else
                (model,Cmd.none)
        
        Tick newTime ->
            case model.status of
                Playing ->
                    saveToStorage <| {model | curr = Time.millisToPosix <| Time.posixToMillis model.curr + 1000}
                _ ->
                    saveToStorage <| model

        Pause ->
            case model.status of
                Playing ->
                    ({model | status = Paused}, Cmd.none)
                _ ->
                    (model,Cmd.none)
        
        Continue ->
            case model.status of
                Paused ->
                    ({model | status = Playing}, Cmd.none)
                _ ->
                    (model, Cmd.none)
        
        Reset ->
            let
                m = newModel model
                nm = {m | games = 0, wins = 0}
            in
                (nm,Cmd.none)
            

        _ ->
            (model, Cmd.none)


newModel model =
    let
        (grid,seed)=
            newGame model.seed
    in
        {model | grid = grid, seed = seed, status = New, start = Time.millisToPosix 0, curr = Time.millisToPosix 0, flags = 0}


flag : (Float,Float) -> Model -> Model 
flag (x,y) model =
    let
        nx =  (round x - 195 ) // 60
               
        ny =  (round y - 10 ) // 60
                
        (grid,inc) = flag_cell model.grid (nx, ny)
    in
        { model | grid = grid, status = Playing , flags = model.flags + inc}


flag_cell : Grid Color -> (Int,Int) -> (Grid Color,Int)
flag_cell grid pos =
    let
        dict = Dict.fromList <| List.map (\cell -> (cell.pos,cell)) grid

        (f_cell, f) =
            case (Dict.get pos dict) of 
                Nothing ->
                    (blankCell covered, False)
                Just c ->
                    ({ c | flag = (if (c.rev || c.flag) then False else True), val = (if c.rev then c.val else (if c.flag then covered else flagged))}, c.flag)
        
        r_grid = List.map (\cell -> if cell.pos == pos then f_cell else cell) grid
        
        inc = 
            if f_cell.flag && not f then
                1
            else
                if f && not f_cell.flag then
                    -1
                else
                    0
    in
        (r_grid, inc)


reveal : (Float,Float) -> Model -> Model
reveal (x,y) model =
    let
        nx = (round x - 195 ) // 60

        ny = (round y - 10) // 60

        grid = reveal_cell model.grid (nx,ny)
            
        dict = Dict.fromList <| List.map (\cell -> (cell.pos,cell)) grid  

        lost = (\c -> c.mine && not c.flag) <| Maybe.withDefault (blankCell covered) <| Dict.get (nx,ny) dict

        safe c = xor c.mine c.rev

        won = List.foldl (&&) True <| List.map safe grid

        status = if lost then Lost else (if won then Won else Playing)

        games = if lost || won then model.games + 1 else model.games

        wins = if (won && not lost) then model.wins + 1 else model.wins

    in
        { model | grid = grid, status = status, games = games, wins = wins }


reveal_cell : Grid Color -> (Int, Int) -> Grid Color
reveal_cell grid pos =
    let
        dict = Dict.fromList <| List.map (\cell -> (cell.pos,cell)) grid

        getNeigh d key =
            case (Dict.get key d) of
                Nothing ->
                    0
                Just c ->
                    if c.mine then
                        1
                    else
                        0

        cellsn (a,b) =
            [(a-1,b-1),(a-1,b),(a-1,b+1),(a,b-1),(a,b+1),(a+1,b-1),(a+1,b),(a+1,b+1)]
        
        neighbors (x,y) =
            List.sum (List.map (getNeigh dict) (cellsn (x,y)))
        
        r_cell =
            case (Dict.get pos dict) of
                Nothing ->
                    blankCell covered
                Just c ->
                    c
        
        neigh = neighbors r_cell.pos
    in
        if (neigh == 0) && not (r_cell.mine || r_cell.flag) then
            let
                intToTupDict = Dict.fromList <| List.indexedMap Tuple.pair <| Dict.keys dict
                tupToIntDict = Dict.fromList <| List.map (\(x,y) -> (y,x)) <| List.indexedMap Tuple.pair <| Dict.keys dict
                myFilter n =
                    case Dict.get n intToTupDict of
                        Nothing ->
                            False
                        Just p ->
                            (neighbors p) == 0
                
                graph =
                    let
                        temp_dict = Dict.map (\k v -> cellsn v) intToTupDict
                    in
                        Dict.map (\k v -> List.map (Maybe.withDefault -1 << (flip Dict.get <| tupToIntDict)) v) temp_dict

                root_find n = 
                    let
                        p = Maybe.withDefault (-1,-1) <| Dict.get n intToTupDict
                        c = Maybe.withDefault (blankCell covered) <| Dict.get p dict
                    in
                        if c == r_cell then
                            n
                        else
                        root_find (n+1)
                
                root = root_find 0

                tup_list = List.map 
                    ( (intToTupDict
                    |> flip Dict.get)
                    >> Maybe.withDefault (-1,-1)
                    )
                    (findDeps myFilter root graph)
                
                cell_list = List.map 
                    ( (dict 
                    |> flip Dict.get)
                    >> Maybe.withDefault (blankCell covered)
                    )
                    tup_list

                r_cell_list = List.map (\c -> {c | rev = not c.flag, neigh = neighbors c.pos, val = if c.flag then flagged else uncovered <| neighbors c.pos }) cell_list
                        
                n_dict = Dict.fromList <| List.map (\c -> (c.pos,c)) r_cell_list

                replace_cell c =
                    case Dict.get c.pos n_dict of
                        Nothing ->
                            c
                        Just nc ->
                            nc
            in
                List.map replace_cell grid
        else
            let
                n_cell =
                    if r_cell.flag || r_cell.rev then
                        r_cell 
                    else
                        { r_cell | rev = True, val = uncovered neigh, neigh = neigh}
        
                lost = (r_cell.mine && not r_cell.flag)
        
                loseGrid = List.map (\cell -> {cell | flag = False, val = (if cell.mine then exploded else uncovered <| neighbors cell.pos), rev = (if cell.mine then False else True), neigh = neighbors cell.pos}) grid

                new_grid = List.map (\cell -> if cell.pos == pos then n_cell else cell) grid

            in
                if lost then loseGrid else new_grid


findDeps f root graph =
    findDepsHelp f Set.empty [root] graph

findDepsHelp : (comparable -> Bool) -> Set comparable -> List comparable -> Dict comparable (List comparable) -> List comparable
findDepsHelp f visited unvisited graph =
    case unvisited of
        [] ->
            Set.toList visited
        next :: rest ->
            if Set.member next visited then
                findDepsHelp f visited rest graph
            else
                let
                    newVisited =
                        Set.insert next visited
                    nextDeps = 
                        if f next then
                            Maybe.withDefault [] (Dict.get next graph)
                        else
                            []
                    newUnvisited =
                        nextDeps ++ rest 
                in
                    findDepsHelp f newVisited newUnvisited graph        


flip : (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x
