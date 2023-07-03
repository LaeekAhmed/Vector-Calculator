import Round

type alias Model =
    { time : Float
    , state : State
    , chars1 : List String
    , chars2 : List String 
    , chars3 : List String -- input for scaling factor/standard matrix
    , chars4 : List String -- input for standard matrix
    , points : List (Float,Float)
    , window : Window -- do not change (used for resizing window)
    , box1 : (Float,Float)
    , box2 : (Float,Float)
    , b1 : String
    , zoom_max : Float
    , zoom_min : Float
    , scaleZ : Float
    , vector : Vector
    , results : Vector
    , highlight : Maybe Button
    }

type Button = AddB | SubtractB | ScaleB | MagB | TransfB | ZoomInB | ZoomOutB | HelpB
type alias Point = (Float, Float)

init : Model
init = { state = Menu
       , chars1 = []
       , chars2 = []
       , chars3 = [] -- input for scaling factor/standard matrix
       , chars4 = [] -- input for standard matrix
       , points = []
       , box1 = (0,0)
       , box2 = (0,0)
       , b1 = ""
       , results = V (0,0)
       , vector = V (0,0)
       -- do not change these
       , time = 0
       , zoom_max = 1
       , zoom_min = 1
       , scaleZ = 1
       , highlight = Nothing
       , window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
        }
     
type Msg = Tick Float
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         | KeyDown String
         | KeyUp String
         | NoOp
         | NewTap (Float,Float)
         | Focus1 
         | Focus2
         | Focus3 -- msg for TypingBox3 for scaling factor
         | Focus4 -- msg for TypingBox3 for std matrix
         | MenutoHelp 
         | Help1to2 
         | Help1toMenu 
         | Help2toMenu 
         | Help2to1 
         | MenutoAppMenu
         | AppMenutoMenu
         | B1
         | Add
         | Subtract
         | Scale
         | Transform
         | Magnitude
         | Zoom_in
         | Zoom_out
         | MouseEnter Button
         | MouseLeave Button

type State = Menu 
           | AppMenu
           | Help1 
           | Help2
           | TypingBox1 
           | TypingBox2 
           | TypingBox3 -- for scaling factor
           | NotTyping 
           | Adding
           | Subtracting
           | Scaling
           | Transforming
           | MagnitudeCalc
           | Zoom_inf
           | Zoom_outf
           
type Vector = V (Float,Float)
type Matrix = M (List (List Int))

--vector Operations:-------------------------------------------------------------------------------------
add : Vector -> Vector -> Vector
add (V (a,b)) (V (c,d)) = V (a+c,b+d)

sub : Vector -> Vector -> Vector
sub (V (a,b)) (V (c,d)) = V (a-c,b-d)

scaleVec : Float -> Vector -> Vector
scaleVec i (V (a,b)) = V (i * a, i * b)

innerPro : Vector -> Vector -> Float
innerPro (V (x,y)) (V (a,b)) = x * a + y * b

trans : (List (List Int)) -> Vector -> Vector
trans m (V (x,y)) = case m of
                       ([a,b]::[c,d]::[]) -> V (x*(toFloat a)+y*(toFloat b),(toFloat c)*x+(toFloat d)*y)
                       _ -> V (x,y)
                     
mag (V (x,y)) = Round.round 2 (sqrt(x*x + y*y))

v1 : Vector
v1 = V (2,2)

{-
example ([1,2]::[3,4]::[])) 
⌈1 2⌉
⌊3 4⌋
-}
m1 = ([[5,0],[0,1]])

---------------------------------------------------------------------------------------------------------------
-- helper func for converting maybe to float
conv1 : Maybe Float -> Float
conv1 x = case x of
           Just a -> a
           Nothing -> 0

-- cord returns the coordinates from the user input
cord : List (String) -> Vector
cord lst = case lst of
-- example   ["1","5", ",", "0", "5"] = ("1"::"5"::","::"0"::"5"::[]) -> V (15,05) # WARNING : input is added reversely 
             (x::","::z::[]) -> V (conv1 (String.toFloat z), conv1 (String.toFloat x))
             (a::x::","::z::[]) -> V (conv1 (String.toFloat z), conv1 (String.toFloat (x++a)))
             (a::","::b::z::[]) -> V (conv1 (String.toFloat (z++b)), conv1 (String.toFloat a))
             (a::x::","::b::z::[]) -> V (conv1 (String.toFloat (z++b)), conv1 (String.toFloat (x++a)))
             (a::x::"-"::","::b::z::"-"::[]) -> V (conv1 (String.toFloat ("-"++z++b)), conv1 (String.toFloat ("-"++x++a)))
             (a::x::"-"::","::b::[]) -> V (conv1 (String.toFloat b), conv1 (String.toFloat ("-"++x++a)))
             (a::","::b::z::"-"::[]) -> V (conv1 (String.toFloat ("-"++z++b)), conv1 (String.toFloat a))
             (a::x::","::b::z::"-"::[]) -> V (conv1 (String.toFloat ("-"++z++b)), conv1 (String.toFloat (x++a)))
             (a::x::"-"::","::b::z::[]) -> V (conv1 (String.toFloat (z++b)), conv1 (String.toFloat ("-"++x++a)))
             _ -> V (0,0)
            
-- func to convert the input for scaling factor
sclr : List (String) -> Float
sclr lst = case lst of 
-- example  ["2,".","5"] = ("2"::"."::"5"::[]) -> 2.5 # WARNING : input is added reversely
            (x::"."::y::"-"::[]) -> (conv1 (String.toFloat y) + ((conv1 (String.toFloat x))/10))*(-1)
            (x::"."::y::"+"::[]) -> conv1 (String.toFloat y) + ((conv1 (String.toFloat x))/10)
            (x::"."::y::[]) -> conv1 (String.toFloat y) + ((conv1 (String.toFloat x))/10)
            (x::[]) -> conv1 (String.toFloat x)
            _ -> 1

adds a = a * 2
subs a = a * 0.5

mat : List (String) -> (List (List Int))
mat lst = case lst of
            ("]"::"]"::a::","::b::"["::","::"]"::c::","::d::"["::"["::[]) -> [[Maybe.withDefault 0 (String.toInt d),Maybe.withDefault 0 (String.toInt c)],[Maybe.withDefault 0 (String.toInt b),Maybe.withDefault 0 (String.toInt a)]]
            _ -> [[1,0],[0,1]]


-- dispalying the vectors
draw colour (V (a,b)) = if (a<25 && b>25) then 
                               group [ line (0,0) (50*(a/b),50) |> outlined (solid 0.5) colour
                                --,circle 1 |> filled colour |> rotate (degrees 40) |> move (50*(a/b),50)
                               ] |> move (-40,-5)
                        else if (a>25 && b<25) then 
                               group [ line (0,0) (50,50*(b/a)) |> outlined (solid 0.5) colour
                                --,circle 1 |> filled colour |> rotate (degrees 40) |> move (50,50*(b/a))
                               ] |> move (-40,-5)
                        else if (a<25 && b<(-25)) then 
                             group [ line (0,0) (-50*(a/b),-50) |> outlined (solid 0.5) colour
                                --,circle 1 |> filled colour |> rotate (degrees 40) |> move (-50*(a/b),-50)
                               ] |> move (-40,-5)
                        else if (a<(-25) && b<25) then 
                             group [ line (0,0) (-50,-50*(b/a)) |> outlined (solid 0.5) colour
                                --,circle 1 |> filled colour |> rotate (degrees 40) |> move (-50,-50*(b/a))
                               ] |> move (-40,-5)
                        else if ((a>25 && b>25) && (b/a)>1) then 
                               group [ line (0,0) (50*(a/b),50) |> outlined (solid 0.5) colour
                                --,circle 1 |> filled colour |> rotate (degrees 40) |> move (50*(a/b),50)
                               ] |> move (-40,-5)
                        else if ((a>25 && b>25) && (a/b)>1) then 
                               group [ line (0,0) (50,50*(b/a)) |> outlined (solid 0.5) colour
                                --,circle 1 |> filled colour |> rotate (degrees 40) |> move (50,50*(b/a))
                               ] |> move (-40,-5)
                        else if ((a<(-25) && b<(-25)) && (a/b)>1) then 
                               group [ line (0,0) (50,50*(b/a)) |> outlined (solid 0.5) colour
                               -- ,circle 1 |> filled colour |> rotate (degrees 40) |> move (50,50*(b/a))
                               ] |> move (-40,-5)
                        else if ((a<(25) && b<(-25)) && (-b/a)>1) then 
                               group [ line (0,0) (50*(a/b),50) |> outlined (solid 0.5) colour
                               -- ,circle 1 |> filled colour |> rotate (degrees 40) |> move (50,50*(b/a))
                               ] |> move (-40,-5)
                        else 
                              group [ line (0,0) (2*a,2*b) |> outlined (solid 0.5) colour
                                ,circle 1 |> filled colour |> rotate (degrees 40) |> move (2*a,2*b)
                               ] |> move (-40,-5)
                             
-- depending on the state you can turn on and off typing
allowTyping model = model.state /= NotTyping 

-- depending on state you can turn on and off animation (the Tick message)
isAnimating model = model.state /= TypingBox1

textBox col width height isHighlighted chars =
  [ rect width height |> filled white
  , text (String.join "" <| List.reverse chars ) |> centered |> size 4 |> filled black 
      |> clip (rect width height |> ghost)
  -- colours the outline with dark blue when clicked
  , rect width height |> outlined (solid 1) (if isHighlighted then col else black)
  ] |> group
  
-- code is keyboard input, soFar is present text
typeAndDelete soFar code =
    if String.length code == 1 then 
        code :: soFar 
    else if code == "Backspace" then
        List.drop 1 soFar
    else soFar
--------------------------------------------------------------------------------------------------------------    
update msg model =
    case msg of
        -- don't change these unless you really need to 
        Tick t -> ( { model | time = t }, Cmd.none )
        WindowResize mWH ->
          case mWH of
            Just ( w, h ) ->
              ( { model | window = didResize model.window w h
                  }
              , Cmd.none
              )
            -- need to get viewport size after the app starts
            Nothing ->
              ( model
              , getViewportSize
              )
        ReturnPosition message ( x, y ) ->
            let
                ( newModel, userCmds ) =
                    update
                        (message (convertCoords model.window ( x, y ) ))
                        model
            in
            ( newModel, userCmds )
        NoOp -> ( model, Cmd.none )
        MenutoHelp  ->
            case model.state of
                Menu  ->
                    ({ model | state = Help1  }, Cmd.none)

                otherwise ->
                    (model,Cmd.none)
        Help1to2  ->
            case model.state of
                Help1  ->
                    ({ model | state = Help2}, Cmd.none)

                otherwise ->
                    (model,Cmd.none)
        Help1toMenu  ->
            case model.state of
                Help1  ->
                    ({ model | state = Menu}, Cmd.none)

                otherwise ->
                    (model,Cmd.none)
        Help2toMenu  ->
            case model.state of
                Help2  ->
                    ({ model | state = Menu}, Cmd.none)

                otherwise ->
                    (model,Cmd.none)
        Help2to1  ->
            case model.state of
                Help2  ->
                   ({ model | state = Help1}, Cmd.none)

                otherwise ->
                    (model,Cmd.none)
        MenutoAppMenu  ->
            case model.state of
                Menu  ->
                   ({ model | state = AppMenu}, Cmd.none)

                otherwise ->
                    (model,Cmd.none)
        AppMenutoMenu  ->
            case model.state of
                AppMenu  ->
                   ({ model | state = Menu}, Cmd.none)

                otherwise ->
                    (model,Cmd.none)
        NewTap (x,y) -> ( { model | points = (x,y) :: model.points }, Cmd.none )
        Focus1 -> ( { model | state = TypingBox1 }, Cmd.none )
        Focus2 -> ( { model | state = TypingBox2 }, Cmd.none )
        Focus3 -> ( { model | state = Scaling, results = scaleVec (sclr model.chars3) (cord model.chars2) }, Cmd.none )
        Focus4 -> ( { model | state = Transforming, results = trans (mat model.chars3) (cord model.chars2) }, Cmd.none )
        Add -> ( { model | state = Adding, results = add (cord model.chars1) (cord model.chars2), vector = add (scaleVec model.zoom_max (cord model.chars1)) (scaleVec model.zoom_max (cord model.chars2)) }, Cmd.none )
        Subtract -> ( { model | state = Subtracting, results = sub (cord model.chars1) (cord model.chars2), vector = sub (scaleVec model.zoom_max (cord model.chars1)) (scaleVec model.zoom_max (cord model.chars2)) }, Cmd.none )
        Scale -> ( { model | state = Scaling, results = scaleVec (sclr model.chars3) (cord model.chars2), vector = scaleVec (sclr model.chars3) (scaleVec model.zoom_max (cord model.chars2)) }, Cmd.none )
        Magnitude -> ( { model | state = MagnitudeCalc }, Cmd.none )
        Transform -> ( { model | state = Transforming, results = trans (mat model.chars3) (cord model.chars2), vector = trans (mat model.chars3) (scaleVec model.zoom_max (cord model.chars2)) }, Cmd.none )
        B1 -> ({ model | b1 = "Testing Take out" },Cmd.none)
        Zoom_in -> ({ model | state = Zoom_inf, zoom_max = model.zoom_max*2},Cmd.none)
        Zoom_out -> ({ model | state = Zoom_outf, zoom_max = model.zoom_max*0.5 },Cmd.none)
        -- get keyboard input
        KeyUp _ -> (model,Cmd.none)
        KeyDown code -> ( case model.state of 
                            TypingBox1 -> { model | chars1 = typeAndDelete model.chars1 code }
                            TypingBox2 -> { model | chars2 = typeAndDelete model.chars2 code}
                            Scaling -> { model | chars3 = typeAndDelete model.chars3 code}
                            Transforming -> { model | chars3 = typeAndDelete model.chars3 code }
                            _ -> model
                        ,Cmd.none)
        MouseEnter something -> 
          ( { model | highlight = Just something }
          , Cmd.none )
    
        MouseLeave _ -> 
          ( { model | highlight = Nothing }
          , Cmd.none )
       
-- myShapes returns list
myShapes model =
        -- return is the home page
        let return = [ rect 1000 1000 |> filled (rgb 151 193 255)
                        , graph1
                        ,draw (rgb 86 0 255) (scaleVec model.zoom_max (cord model.chars1))
                        ,draw (rgb 0 197 0) (scaleVec model.zoom_max (cord model.chars2))
                        , calculator
                          |> rotate (degrees 350)
                          |> move (5,5)
                        ,[ rect 0.5 13 |> filled red |> rotate (degrees 45) |> move (-42,55)
                          ,triangle 1.5 |> filled red |> rotate (degrees 45) |> move (-36,58)
                          ,rect 0.5 17 |> filled red |> rotate (degrees 135) |> move (-42,52)
                          ,triangle 1.5 |> filled red |> rotate (degrees 135) |> move (-47,60)
                        ]
                          |> group
                          |> move (2,0)
                        , [ circle 5 |> filled black |> makeTransparent 0.5 |> move(if model.highlight == Just ZoomInB then (1,-1) else (0,0))
                            ,circle 5 |> filled grey
                            ,text "➕" |> size 5 |> filled black |> move (-2.6,-1.7)
                          ] 
                          |> group
                          |> move(5,-50)
                          |> notifyEnter (MouseEnter ZoomInB)
                          |> notifyLeave (MouseLeave ZoomInB)
                          |> notifyTap Zoom_in
                          
                        , [ circle 5 |> filled black |> makeTransparent 0.5 |> move(if model.highlight == Just ZoomOutB then (1,-1) else (0,0))
                            ,circle 5 |> filled grey
                            ,text "➖" |> size 5 |> filled black |> move (-2.6,-1.7)
                          ] 
                          |> group
                          |> move(-85,-50)
                          |> notifyEnter (MouseEnter ZoomOutB)
                          |> notifyLeave (MouseLeave ZoomOutB)
                          |> notifyTap Zoom_out
                        
                        ,text "Vector Calculator"
                              |> customFont "consolas"
                              |> centered
                              |> size 7
                              |> filled blue
                              |> makeTransparent 0.5
                              |> move (1,54)
                        ,text "Vector Calculator"
                              |> customFont "consolas"
                              |> centered
                              |> size 7
                              |> filled black
                              |> move (3,55)
                         ,text "Enter the coordinates, Format : x,y"
                              |> customFont "times new roman"
                              |> centered
                              |> size 5
                              |> filled black
                              |> move (50,35)
                         , [ roundedRect 50 15 5 |> filled darkGreen |> makeTransparent 0.5 |> move(if model.highlight == Just AddB then (2,-2) else (0,0))
                             , roundedRect 50 15 5 |> filled green 
                             , text "(+) Add" |> centered |> size 7 |> filled white |> move(0,-2)
                             ]
                               |> group
                               |> scale 0.7
                               |> move (30,-20) 
                               |> notifyEnter (MouseEnter AddB)
                               |> notifyLeave (MouseLeave AddB)
                               |> notifyTap Add
                        , model.b1 |> text |> size 5 |> filled black |> move (-85,45)
                        , [ roundedRect 50 15 5 |> filled darkGreen |> makeTransparent 0.5 |> move(if model.highlight == Just SubtractB then (2,-2) else (0,0))
                             , roundedRect 50 15 5 |> filled green 
                            , text "(−) Subtract" |> centered |> size 7 |> filled white |> move(0,-2)
                            ]
                              |> group
                              |> scale 0.7
                              |> move (70,-20) 
                              |> notifyEnter (MouseEnter SubtractB)
                              |> notifyLeave (MouseLeave SubtractB)
                              |> notifyTap Subtract
                        , [ roundedRect 50 15 5 |> filled darkGreen |> makeTransparent 0.5 |> move(if model.highlight == Just ScaleB then (2,-2) else (0,0))
                             , roundedRect 50 15 5 |> filled green 
                            , text "(⇅) Scale" |> centered |> size 7 |> filled white |> move(0,-2)
                            ]
                              |> group
                              |> scale 0.7
                              |> move (30,-33) 
                              |> notifyEnter (MouseEnter ScaleB)
                              |> notifyLeave (MouseLeave ScaleB)
                              |> notifyTap Scale
                        , [ roundedRect 50 15 5 |> filled darkGreen |> makeTransparent 0.5 |> move(if model.highlight == Just MagB then (2,-2) else (0,0))
                             , roundedRect 50 15 5 |> filled green 
                            , text "|u| Magnitude" |> centered |> size 7 |> filled white |> move(0,-2)
                            ]
                              |> group
                              |> scale 0.7
                              |> move (70,-33) 
                              |> notifyEnter (MouseEnter MagB)
                              |> notifyLeave (MouseLeave MagB)
                              |> notifyTap Magnitude

                        , [ roundedRect 70 15 5 |> filled darkGreen |> makeTransparent 0.5 |> move(if model.highlight == Just TransfB then (2,-2) else (0,0))
                             , roundedRect 70 15 5 |> filled green 
                            , text "(↪) Transformation" |> centered |> size 7 |> filled white |> move(0,-2)
                            ]
                              |> group
                              |> scale 0.7
                              |> move (50,-47) 
                              |> notifyEnter (MouseEnter TransfB)
                              |> notifyLeave (MouseLeave TransfB)
                              |> notifyTap Transform
                              
                        ,(List.map ( \ pos -> circle 3 |> filled red |> move pos ) <| model.points)
                              |> group 
                              |> move (0.25*collageWidth,0)
                              |> rotate (0.1 * sin (10*model.time))
                              |> move (-0.25*collageWidth,0)
                        -- model.chars1 will be the input for the first box
                        , textBox (rgb 86 0 255) 30 10 (model.state == TypingBox1) model.chars1 |> move (45,25)
                              |> notifyTap Focus1
                        , textBox (rgb 0 197 0) 30 10 (model.state == TypingBox2) model.chars2 |> move (45,5)
                              |> notifyTap Focus2
                        ]
        in case model.state of
              Menu ->  [ Surena6.Menu.myShapes model
                          |>group
                  ,group
                  [
                       circle 10
                            |> filled blue
                            |> makeTransparent 0.5 
                            |> move(if model.highlight == Just HelpB then (88,70) else (86,72))
                       ,question
                  ]
                     |> scale 0.7
                     |> move (25,5)
                     |> notifyEnter (MouseEnter HelpB)
                     |> notifyLeave (MouseLeave HelpB)
                     |> notifyTap MenutoHelp
                  
                  ,group
                  [
                       roundedRect 50 15 5 |> filled green 
                       ,text "Start!" 
                         |> size 10
                         |> customFont "consolas"
                         |> filled white 
                         |> move (-9,-4)
                  ]
                     |> move (0,-10)
                     |> notifyTap MenutoAppMenu]
              AppMenu -> return ++ 
                            [List.map makeScaleX 
                                    (List.range 1 13)
                                     |> group 
                                  ,List.map makeScaleY 
                                    (List.range 1 13)
                                    |> group]
              TypingBox1 -> return ++ 
                            [List.map makeScaleX 
                                    (List.range 1 13)
                                     |> group 
                                  ,List.map makeScaleY 
                                    (List.range 1 13)
                                    |> group]
              TypingBox2 -> return ++ 
                            [List.map makeScaleX 
                                    (List.range 1 13)
                                     |> group 
                                  ,List.map makeScaleY 
                                    (List.range 1 13)
                                    |> group]
              TypingBox3 -> [] ++ 
                            [List.map makeScaleX 
                                    (List.range 1 13)
                                     |> group 
                                  ,List.map makeScaleY 
                                    (List.range 1 13)
                                    |> group]
              NotTyping -> return ++ 
                            [List.map makeScaleX 
                                    (List.range 1 13)
                                     |> group 
                                  ,List.map makeScaleY 
                                    (List.range 1 13)
                                    |> group]
              Adding -> return
                        ++ 
                       [draw purple (add (scaleVec model.zoom_max (cord model.chars1)) (scaleVec model.zoom_max (cord model.chars2)))
                       ,text ("Result : "++ Debug.toString model.results)
                              |> customFont "consolas"
                              |> size 5
                              |> filled black
                              |> move (-93,-60)
                       ]++ 
                         [ text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (8, 0)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (16, 0)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (24, 0)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (32, 0)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (40, 0)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (48, 0)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (56, 0)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (64, 0)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (72, 0)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (80, 0)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (88, 0)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (96, 0)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (104, 0) 
                         
                         
                         ,text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 8)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 16)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 24)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 32)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 40)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 48)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 56)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 64)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 72)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 80)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 88)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 96)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 104)]
              Subtracting -> return 
                         ++ 
                       [draw purple (sub (scaleVec model.zoom_max (cord model.chars1)) (scaleVec model.zoom_max (cord model.chars2)))
                       ,text ("Result : "++ Debug.toString model.results)
                              |> customFont "consolas"
                              |> size 5
                              |> filled black
                              |> move (-93,-60)
                       ]++ 
                         [ text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (8, 0)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (16, 0)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (24, 0)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (32, 0)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (40, 0)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (48, 0)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (56, 0)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (64, 0)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (72, 0)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (80, 0)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (88, 0)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (96, 0)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (104, 0) 
                         
                         
                         ,text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 8)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 16)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 24)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 32)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 40)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 48)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 56)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 64)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 72)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 80)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 88)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 96)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 104)]
              Scaling -> 
                     return 
                     ++
                     [
                     text "Enter the coordinates & Scaling Factor : " 
                              |> customFont "times new roman"
                              |> centered
                              |> size 5
                              |> filled black 
                              |> move (52,13)
                      ,text "please enter b/w [0..9]" 
                              |> customFont "times new roman"
                              |> centered
                              |> size 5
                              |> filled black 
                              |> move (55,-8)
                     ,textBox hotPink 15 10 (model.state == Scaling) model.chars3 |> move (80,5)|> notifyTap Focus3
                     ,draw hotPink (scaleVec (sclr model.chars3) (scaleVec model.zoom_max (cord model.chars2)))
                     ,text ("Result : "++ Debug.toString (scaleVec (sclr model.chars3) (cord model.chars2)))
                              |> customFont "consolas"
                              |> size 5
                              |> filled black
                              |> move (-93,-60)
                     ]++ 
                         [ text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (8, 0)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (16, 0)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (24, 0)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (32, 0)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (40, 0)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (48, 0)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (56, 0)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (64, 0)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (72, 0)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (80, 0)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (88, 0)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (96, 0)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (104, 0) 
                         
                         
                         ,text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 8)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 16)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 24)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 32)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 40)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 48)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 56)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 64)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 72)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 80)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 88)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 96)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 104)]
              MagnitudeCalc -> return 
                            ++
                            [
                              text ("Result: |V1| = "++ mag (cord model.chars1) ++ " ,|V2| = "++mag (cord model.chars2))
                              |> customFont "consolas"
                              |> size 5
                              |> filled black
                              |> move (-93,-60)
                              ]++ 
                         [ text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (8, 0)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (16, 0)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (24, 0)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (32, 0)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (40, 0)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (48, 0)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (56, 0)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (64, 0)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (72, 0)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (80, 0)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (88, 0)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (96, 0)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (104, 0) 
                         
                         
                         ,text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 8)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 16)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 24)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 32)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 40)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 48)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 56)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 64)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 72)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 80)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 88)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 96)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 104)]
              Transforming -> 
                     return 
                     ++
                     [
                     text "Enter the coordinates & Std Matrix : " 
                              |> customFont "times new roman"
                              |> centered
                              |> size 5
                              |> filled black 
                              |> move (52,13)
                              
                     ,text "Format : [[a,b],[c,d]] (single digits only)" 
                              |> customFont "times new roman"
                              |> centered
                              |> size 5
                              |> filled black 
                              |> move (55,-8)
                     ,textBox hotPink 25 10 (model.state == Transforming) model.chars3 |> move (80,5)|> notifyTap Focus4
                     ,text ("Result : "++ Debug.toString (trans (mat model.chars3) (cord model.chars2)))
                              |> customFont "consolas"
                              |> size 5
                              |> filled black
                              |> move (-93,-60)
                     ,draw hotPink (trans (mat model.chars3) (scaleVec model.zoom_max (cord model.chars2)))
                     ]++ 
                         [ text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (8, 0)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (16, 0)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (24, 0)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (32, 0)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (40, 0)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (48, 0)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (56, 0)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (64, 0)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (72, 0)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (80, 0)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (88, 0)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (96, 0)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (104, 0) 
                         
                         
                         ,text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 8)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 16)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 24)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 32)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 40)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 48)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 56)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 64)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 72)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 80)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 88)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 96)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 104)]
              
              Zoom_inf -> return 
                     ++ [ text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (8, 0)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (16, 0)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (24, 0)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (32, 0)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (40, 0)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (48, 0)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (56, 0)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (64, 0)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (72, 0)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (80, 0)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (88, 0)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (96, 0)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (104, 0) 
                         
                         
                         ,text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 8)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 16)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 24)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 32)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 40)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 48)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 56)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 64)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 72)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 80)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 88)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 96)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 104)]
              Zoom_outf -> return 
                     ++ [ text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (8, 0)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (16, 0)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (24, 0)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (32, 0)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (40, 0)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (48, 0)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (56, 0)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (64, 0)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (72, 0)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (80, 0)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (88, 0)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (96, 0)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-97,-9)
                            |> move (104, 0) 
                         
                         
                         ,text (String.fromFloat ((-24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 8)
                         ,text (String.fromFloat ((-20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 16)
                         ,text (String.fromFloat ((-16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 24)
                         ,text (String.fromFloat ((-12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 32)
                         ,text (String.fromFloat ((-8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 40)
                         ,text (String.fromFloat ((-4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 48)
                         ,text (String.fromFloat ((0) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 56)
                         ,text (String.fromFloat ((4) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 64)
                         ,text (String.fromFloat ((8) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 72)
                         ,text (String.fromFloat ((12) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 80)
                         ,text (String.fromFloat ((16) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 88)
                         ,text (String.fromFloat ((20) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 96)
                         ,text (String.fromFloat ((24) / model.zoom_max)) |> size 4 |> filled black |> move (-45,-62)
                            |> move (0, 104)] 
              Help1  ->
                  [ Moham50.Help01.myShapes model
                    |>group
                  , group
                        [
                          triangle 7
                            |> filled lightBlue
                            |> move(110,-30)

                        ]
                           |> move (-25, -25)
                           |> notifyTap Help1to2
                  , home_button1
                           |> move (25, -25)
                           |> notifyTap Help1toMenu
                  ]
              Help2  ->
                  [ Moham50.Help02.myShapes model
                    |>group
                  , home_button2
                           |> move (-25, -25)
                           |> notifyTap Help2toMenu
                  , group
                        [
                             triangle 7
                              |> filled lightBlue
                              |> rotate 110
                              |> move(-110,-30)

                        ]
                           |> move (25, -25)
                           |> notifyTap Help2to1
                        ]

-- graphs & funcs: -------------------------------------------------------------------------------------------------------
graph1= group [ line (-50,0) (50,0)
                    |> outlined (solid 0.5) black |> move (-40,-5)
                , line (0,-50) (0,50)
                    |> outlined (solid 0.5) black |> move (-40,-5)
                , List.map makeGraphHor
                   (List.range 1 25)
                   |> group
                , List.map makeGraphVer
                   (List.range 1 25)
                   |> group
              ]
                            
makeGraphHor idx = 
    line (-50,-52) (50,-52) |> outlined (solid 0.5) black |> move (-40,-5) |> makeTransparent 0.25
      |> move (0, 4 * toFloat idx)
      
makeGraphVer idx = 
    line (-52,-50) (-52,50) |> outlined (solid 0.5) black |> move (-40,-5) |> makeTransparent 0.25
      |> move (4 * toFloat idx, 0)
      
makeScaleX idx = 
    text (String.fromFloat (-28 + 4 * toFloat idx)) |> size 4 |> filled black |> move (-97,-9)
      |> move (8 * toFloat idx, 0)
      
makeScaleY idx = 
    text (String.fromFloat (-28 + 4 * toFloat idx)) |> size 4 |> filled black |> move (-45,-62)
      |> move (0, 8 * toFloat idx)
------------------------------------------------------------------------------------------------------------
zoom_in = group [ circle 5 |> filled grey,
                  text "➕" |> size 5 |> filled black |> move (-3.5,-1.5)
                ] |> move(5,-50)
               
zoom_out = group [ circle 5 |> filled grey,
                  text "➖" |> size 5 |> filled black |> move (-3.5,-1.5)
                ] |> move(-85,-50)

home_button1 = group
                  [
                  circle 10
                  |> filled lightBlue
                  |> move(60,72)
                  ,curve (-60.64,-6.08) [Pull (-60.8,1.4400) (-60.96,8.96)
                  ,Pull (-52.64,16.96) (-44.32,24.96)
                  ,Pull (-35.68,17.12) (-27.04,9.28)
                  ,Pull (-27.04,1.5999) (-27.04,-6.08)
                  ,Pull (-33.28,-6.24) (-39.52,-6.4)
                  ,Pull (-39.52,0.6399) (-39.52,7.68)
                  ,Pull (-44,7.68) (-48.48,7.68)
                  ,Pull (-48.48,0.48) (-48.48,-6.72)
                  ,Pull (-54.56,-6.560) (-60.64,-6.4)
                  ,Pull (-60.64,-6.24) (-60.64,-6.08)]
                  |> filled white
                  |> move (194,172)
                  |> scale 0.4
                  ]
               
home_button2 = group
                  [
                  circle 10
                  |> filled lightBlue
                  |> move(110,72)
                  ,curve (-60.64,-6.08) [Pull (-60.8,1.4400) (-60.96,8.96)
                  ,Pull (-52.64,16.96) (-44.32,24.96)
                  ,Pull (-35.68,17.12) (-27.04,9.28)
                  ,Pull (-27.04,1.5999) (-27.04,-6.08)
                  ,Pull (-33.28,-6.24) (-39.52,-6.4)
                  ,Pull (-39.52,0.6399) (-39.52,7.68)
                  ,Pull (-44,7.68) (-48.48,7.68)
                  ,Pull (-48.48,0.48) (-48.48,-6.72)
                  ,Pull (-54.56,-6.560) (-60.64,-6.4)
                  ,Pull (-60.64,-6.24) (-60.64,-6.08)]
                  |> filled white
                  |> move (319,172)
                  |> scale 0.4
                  ]
                              
calculator = group [ rect 10 15 
                            |> outlined (solid 0.5) black 
                            |> move (35,55)
                           , rect 8 3.5 
                            |> outlined (solid 0.5) black 
                            |> move (35,59) 
                           , square 2
                            |> filled black 
                            |> move (35,49) 
                           , square 2 
                            |> filled black 
                            |> move (38,49) 
                           , square 2 
                            |> filled black 
                            |> move (32,49) 
                           , square 2 
                            |> filled black 
                            |> move (32,52) 
                           , square 2 
                            |> filled black 
                            |> move (35,52) 
                           , square 2 
                            |> filled black 
                            |> move (38,52) 
                           , square 2 
                            |> filled black 
                            |> move (32,55) 
                           , square 2 
                            |> filled black 
                            |> move (35,55) 
                           , square 2 
                            |> filled black 
                            |> move (38,55) 
                        ]
question = group [
          circle 10
              |> filled blue
              |> move(86,72)

          ,circle 1.5
              |> filled white
              |> move(86,65)

          ,curve (-58.4,30.72) [Pull (-62.08,35.2) (-65.76,39.68)
                            ,Pull (-63.68,41.28) (-61.6,42.88)
                            ,Pull (-59.04,44.480) (-56.48,46.08)
                            ,Pull (-53.44,46.879) (-50.4,47.68)
                            ,Pull (-47.51,47.84) (-44.64,48)
                            ,Pull (-42.08,47.68) (-39.52,47.36)
                            ,Pull (-37.44,46.72) (-35.36,46.08)
                            ,Pull (-33.6,44.96) (-31.84,43.84)
                            ,Pull (-30.08,42.08) (-28.32,40.32)
                            ,Pull (-27.52,38.72) (-26.72,37.12)
                            ,Pull (-26.4,35.2) (-26.08,33.28)
                            ,Pull (-26.08,31.68) (-26.08,30.08)
                            ,Pull (-26.56,28) (-27.04,25.92)
                            ,Pull (-27.84,24.16) (-28.64,22.4)
                            ,Pull (-29.92,20.799) (-31.2,19.2)
                            ,Pull (-32.16,17.92) (-33.12,16.64)
                            ,Pull (-33.92,15.52) (-34.72,14.4)
                            ,Pull (-35.51,13.120) (-36.32,11.84)
                            ,Pull (-37.28,10.24) (-38.24,8.64)
                            ,Pull (-38.72,7.0400) (-39.2,5.44)
                            ,Pull (-39.52,3.5200) (-39.84,1.6)
                            ,Pull (-46.88,1.28) (-53.92,0.96)
                            ,Pull (-53.92,3.36) (-53.92,5.76)
                            ,Pull (-53.28,7.52) (-52.64,9.28)
                            ,Pull (-51.68,11.04) (-50.72,12.8)
                            ,Pull (-49.44,14.4) (-48.16,16)
                            ,Pull (-47.04,17.28) (-45.92,18.56)
                            ,Pull (-44.8,19.84) (-43.68,21.12)
                            ,Pull (-42.56,22.4) (-41.44,23.68)
                            ,Pull (-40.64,25.28) (-39.84,26.88)
                            ,Pull (-39.84,28.32) (-39.84,29.76)
                            ,Pull (-40.48,31.200) (-41.12,32.64)
                            ,Pull (-42.23,33.92) (-43.36,35.2)
                            ,Pull (-44.48,35.68) (-45.6,36.16)
                            ,Pull (-47.2,36.16) (-48.8,36.16)
                            ,Pull (-50.4,35.68) (-52,35.2)
                            ,Pull (-53.44,34.24) (-54.88,33.28)
                            ,Pull (-56.64,32) (-58.4,30.72)]
                  |> filled white
                  |> scale 0.25
                  |> move (97.5,67) ]
                        
-- use these variables for your collage size
collageWidth = 192
collageHeight = 128

appTitle = "My App"

