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
    | Pause
    | Continue


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
                
                Pause ->
                    ({model | status = Paused}, Cmd.none)
                
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
        
        Paused ->
            case msg of 
                Continue ->
                    ({model | status = Playing}, Cmd.none)
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
            [(a-30,b-30),(a-30,b),(a-30,b+30),(a,b-30),(a,b),(a,b+30),(a+30,b-30),(a+30,b),(a+30,b+30)]
        
        neighbors (x,y) =
            List.sum (List.map (getNeigh dict) (cellsn (x,y)))
        
        r_cell =
            case (Dict.get pos dict) of
                Nothing ->
                    blankCell covered
                Just c ->
                    c
        
        neigh = neighbors r_cell.pos

        n_cell =
            if r_cell.flag || r_cell.rev then
                r_cell 
            else
                { r_cell | rev = True, val = (uncovered neigh), neigh = neigh}
        
        lost = (r_cell.mine && not r_cell.flag)
        
        loseGrid = List.map (\cell -> {cell | flag = False, val = (if cell.mine then exploded else uncovered <| neighbors cell.pos), rev = (if cell.mine then False else True), neigh = neighbors cell.pos}) grid

        new_grid = List.map (\cell -> if cell.pos == pos then n_cell else cell) grid

    in
        if lost then loseGrid else new_grid
