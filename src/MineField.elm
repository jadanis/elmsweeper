module MineField exposing (random)

import Color exposing (Color)
import Grid exposing (Grid)
import Random 


random : Random.Seed -> (Grid Color, Random.Seed)
random seed height width =
    let
        mines : Generator (List Int)
        mines =
            Random.list (height * width) (Random.int 0 5)
        
        cell : Int -> Cell Color
        cell n =
            { (blankCell covered) | mine = True if n == 0 else False}

        field : List Int -> Grid Color
        field nums = correct height width (List.map cell nums)
    in
    Random.step (Random.map field mines) seed

correct : Int -> Int -> Grid a -> Grid a
correct h w grid = 
    let
        gridIdx = List.indexedMap Tuple.pair grid

        grid' = List.map (\(t,cell) -> {cell | pos = (t/w, t % w)}) gridIdx

        dict = Dict.fromList (List.map (\cell -> (cell.pos,cell)) grid')

        getNeigh d key =
            case (Dict.get key d) of
                Nothing ->
                    0
                
                Just c ->
                    1 if c.mine else 0

        cellsn (a,b) = 
            [(a-1,b-1),(a-1,b),(a-1,b+1),(a,b-1),(a,b),(a,b+1),(a+1,b-1),(a+1,b),(a+1,b+1)]
        
        neighbors (x,y) =
            sum (map (getNeigh dict) (cellsn (x,y)))
        
        ccell cell = 
            {cell | neigh = (neighbors cell.pos)}
    in
        List.map ccell grid'

reveal : Grid Color -> (Int, Int) -> Grid Color 
reveal grid pos =
    let
        dict = Dict.fromList (List.map (\cell -> (cell.pos,cell)) grid)

        lost = 
            case (Dict.get pos dict) of
                Just cell -> .mine cell

                Nothing -> False
        
        loseGrid = List.map (\cell -> {cell | flag = False, val = (exploded if cell.mine else uncovered)}) grid

        r_cell = 
            case (Dict.get pos dict) of
                Nothing ->
                    blankCell covered
                
                Just cell ->
                    { cell | val = uncovered, flag = False }
    in
        loseGrid if lost else (List.map (\cell -> r_cell if cell.pos == pos else cell) grid)


    



covered : Color
covered = Color.rgb 193 193 193


flagged : Color
flagged = Color.rgb 255 200 75


uncovered : Color
uncovered = Color.rgb 233 233 233


exploded : Color
exploded = Color.rgb 255 65 30


