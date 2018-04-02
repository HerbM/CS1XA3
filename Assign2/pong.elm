
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Html exposing (..)
import Html.Attributes exposing (..)
import Html as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Mouse as Mouse
import Time exposing (Time, second)
import Keyboard as Key

{- Model -}
type alias Model = {

        on : Bool
        ,posball : {x : Int, y : Int}
        ,dy : Int
        ,dx : Int
        ,tops : Int
        ,bottoms : Int
        ,topx : Int
        ,bottomx : Int      }

init : (Model,Cmd.Cmd Msg)
init = ({ on = False, posball = {x = 20, y = 20}, dy = 1, dx = 1, tops = 0, bottoms = 0, topx = 500, bottomx = 500},Cmd.none)

{- Update -}
type Msg = KeyMsg Key.KeyCode
          | Tick Float

update msg model = 
    case msg of
        Tick time ->

         if model.on == False then (model, Cmd.none)
          else 
            if model.posball.x < 70 && model.dx < 0 then --left side bounce
                ({ model | dx = -model.dx },Cmd.none)
                  else
                    if model.posball.x > 900 && model.dx > 0 then --right side  bounce
                      ({ model | dx = -model.dx },Cmd.none)
                        else 
                          if model.posball.y < 20 && model.dy < 0 && abs(model.topx + 100 - model.posball.x - 25) < 100 then -- top bounce
                            ({ model | dy = -model.dy },Cmd.none)  
                              else
                                if model.posball.y > 800 then --bottom gone
                                  ({ model | tops = model.tops + 1, posball = { x = 500, y = 400}},Cmd.none)
                                    else
                                      if model.posball.y < -25 then -- top gone
                                        ({ model | bottoms = model.bottoms + 1, posball = { x = 500, y = 400} },Cmd.none)
                                        else 
                                          if (model.posball.y > 690) && (model.posball.y < 720) && model.dy > 0 && abs(model.bottomx + 100 - model.posball.x - 25) < 100 then -- bottom bounce
                                            ({ model | dy = -model.dy },Cmd.none)  
                                              else if model.bottoms >= 3 || model.tops >= 3 then 
                                                ({ model | dy = 0, dx = 0, on = False },Cmd.none)
                                                    else ({ on = model.on,  posball = { x = round <| (toFloat(model.posball.x) + (time/2)*(toFloat(model.dx))), y = round <| (toFloat(model.posball.y) + (time/2)*(toFloat(model.dy))) }, dy = model.dy, dx = model.dx, tops = model.tops, bottoms = model.bottoms, topx = model.topx, bottomx = model.bottomx },Cmd.none)

        KeyMsg keyCode ->
          case keyCode of
            39 -> ({ model | bottomx = model.bottomx+150},Cmd.none)
            37 -> ({ model | bottomx = model.bottomx-150},Cmd.none)
            65 -> ({ model | topx = model.topx-150},Cmd.none)
            68 -> ({ model | topx = model.topx+150},Cmd.none)
            32 -> ({ model | on = True, tops = 0, bottoms = 0, dy = 1, dx = 1},Cmd.none)
            _ -> (model,Cmd.none)

majorSt : Html.Attribute msg
majorSt = Html.Attributes.style [("color","Black"), ("text-align", "center")] 
    
{- View -}
view : Model -> Html.Html Msg
view model = 


  if not model.on && model.tops ==0 && model.bottoms == 0 then
    div [] [

    h1 [majorSt] [ Html.text ("WELCOME TO PONG 2.0!!") ]
    ,h2 [majorSt] [ Html.text ("Controls: Top Player move with A and D, Bottom Player use left and right arrows")]
    ,h2 [majorSt] [ Html.text ("First to 3 wins")]
    ,h2 [majorSt] [Html.text ("Press Space to Begin") ]

      ]

       else  if model.tops == 3 then

         div [] [

              h1 [majorSt] [ Html.text ("TOP PLAYER WINS!!!") ]
              ,h2 [majorSt] [ Html.text ("Press space to play again") ]
               ]

                  else if model.bottoms == 3 then 
                        div [] [

                            h1 [majorSt] [ Html.text ("BOTTOM PLAYER WINS!!!") ]
                            ,h2 [majorSt] [ Html.text ("Press space to play again") ]
                                  ]

                    else 


                      svg [ Svg.Attributes.width "1000", Svg.Attributes.height "800"]
                            [ 
                                image [ x (toString(model.posball.x)), y (toString(model.posball.y)) , xlinkHref "soccerball.jpg", Svg.Attributes.width "100" , Svg.Attributes.height "100"] [],
                                rect [x (toString model.bottomx), y "770", Svg.Attributes.width "200", Svg.Attributes.height "20"] []
                                ,rect [x (toString model.topx), y "0", Svg.Attributes.width "200", Svg.Attributes.height "20"] []
                                    ]





subscriptions : Model -> Sub Msg
subscriptions model = 
        Sub.batch
        [Anim.diffs Tick
        ,Key.downs KeyMsg
        ]


{- Main -}
main : Program Never Model Msg
main = Html.program
          { init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
            }

