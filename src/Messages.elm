module Messages exposing (Msg(..))

-- Update

type Msg
    = Start
    | Flag (Int,Int)
    | Reveal (Int,Int)
    | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start ->
            (newGame model.seed, Cmd.none)
        
        Flag (x, y) ->
            let
                grid = List.map (\cell -> {cell | flag = (True if (x,y) == cell.pos else False), val = (flagged if (x,y) == cell.pos else False)})
            in
                ({model | grid = grid }, Cmd.none)
            
        
        Reveal (x, y) ->
            ...
        
        Noop ->
            (model, Cmd.none)
