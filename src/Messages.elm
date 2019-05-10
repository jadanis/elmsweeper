port module Messages exposing (..)

import Model exposing (..)
import Dict
import Color exposing (..)
import Grid exposing (..)
import Time

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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model.status of 
        Playing ->
            case msg of
                Flag (x,y) ->
                    let
                        nx = 30 * (round x // 30)
               
                        ny = 30 * (round y // 30)
                
                        grid = flag model.grid (nx, ny)
                    in
                        ({model | grid = grid }, Cmd.none)
        
                Reveal (x,y) ->
                    let
                        nx = 30 * (round x // 30)

                        ny = 30 * (round y // 30)

                        grid = reveal model.grid (nx,ny)
            
                        dict = Dict.fromList <| List.map (\cell -> (cell.pos,cell)) grid  

                        lost = (\c -> c.mine && not c.flag) <| Maybe.withDefault (blankCell covered) <| Dict.get (nx,ny) dict

                        safe c = xor c.mine c.rev

                        won = List.foldl (&&) True <| List.map safe grid

                        status = if lost then Lost else (if won then Won else Playing)

                        games = if lost || won then model.games + 1 else model.games

                        wins = if (won && not lost) then model.wins + 1 else model.wins

                    in
                        ({ model | grid = grid, status = status, games = games, wins = wins }, Cmd.none)
        
                Tick newTime ->
                    ({model | curr = Time.millisToPosix <| Time.posixToMillis model.curr + 1000 }, Cmd.none)
                    --    |> saveToStorage
                
                _ ->
                    (model, Cmd.none)

        Lost ->
            case msg of 
                NewGame ->
                    let
                        (grid,seed)=
                            newGame model.seed   
                    in
                        ({model | grid = grid, seed = seed, status = Playing , start = Time.millisToPosix 0, curr = Time.millisToPosix 0}, Cmd.none)
                    
                _ ->
                    (model, Cmd.none)

        Won ->
            case msg of 
                NewGame ->
                    let
                        (grid,seed)=
                            newGame model.seed   
                    in
                        ({model | grid = grid, seed = seed, status = Playing, start = Time.millisToPosix 0, curr = Time.millisToPosix 0}, Cmd.none)
                    
                _ ->
                    (model, Cmd.none)




flag : Grid Color -> (Int,Int) -> Grid Color
flag grid pos =
    let
        dict = Dict.fromList <| List.map (\cell -> (cell.pos,cell)) grid

        f_cell =
            case (Dict.get pos dict) of 
                Nothing ->
                    blankCell covered
                Just c ->
                    { c | flag = (if (c.rev || c.flag) then False else True), val = (if c.rev then c.val else (if c.flag then covered else flagged))}
    in
        List.map (\cell -> if cell.pos == pos then f_cell else cell) grid


reveal : Grid Color -> (Int, Int) -> Grid Color
reveal grid pos =
    let
        dict = Dict.fromList <| List.map (\cell -> (cell.pos,cell)) grid

        r_cell =
            case (Dict.get pos dict) of 
                Nothing ->
                    blankCell covered
                Just c ->
                    if c.flag || c.rev then
                        c 
                    else
                        { c | rev = True, val = (uncovered c.neigh) }
        
        lost =
            case (Dict.get pos dict) of
                Nothing -> False
                Just c -> (c.mine && not c.flag)
        
        loseGrid = List.map (\cell -> {cell | flag = False, val = (if cell.mine then exploded else uncovered cell.neigh), rev = (if cell.mine then False else True)}) grid

        new_grid = List.map (\cell -> if cell.pos == pos then r_cell else cell) grid

    in
        if lost then loseGrid else new_grid
