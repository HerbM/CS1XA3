module SvgAnimation exposing (..)
import Html exposing (..)
import Html.Attributes
import Html as Html
import Platform.Cmd as Cmd
import Html.Events exposing (..)
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Mouse
import Keyboard

{- Model
   Hold (x,y) coordinatees for a circle
 -}
type alias Model = {
        position : { x : Int, y : Int }
        ,counter : Int

                   }

{- Wrap current Time as Float -}
type Msg 
      = Tick Float
        |MouseMsg Mouse.Position

{- Define a starting position
   (ends up being irrelavent) -}
init : (Model, Cmd Msg)
init = ({ position = {x = 300, y = 300}, counter = 0},Cmd.none)

{- Update
   Change position based on current time
 -}

update msg model =
  case msg of
      Tick time ->
        let
          posX = round <| 300 + 200 * cos (toFloat(model.counter)*(time/1000))
          posY = round <| 300 + 200 * sin (toFloat(model.counter)*(time/1000))
          modelN = { position = {x = posX, y = posY}, counter = model.counter}
        in (modelN,Cmd.none)


      MouseMsg coordinatees ->

          let 
            modely = 
              if abs(coordinatees.x - model.position.x) <= 200 && abs(coordinatees.y - model.position.y) <= 200 then
                { position = model.position, counter = model.counter + 1}
              else
                { position = model.position, counter = model.counter}
          in (modely,Cmd.none)



{- Subscriptions
   Subscribe to time using AnimationFrame for
   smooth rendering
 -}
subscriptions model = 
      Sub.batch
        [Anim.times Tick
        ,Mouse.clicks MouseMsg
        ]
{- View
   Render circle based on current position
   held in model
 -}
view : Model -> Html.Html Msg
view model = 

    let
      counter = model.counter
      posX = toString model.position.x
      posY = toString model.position.y
    in 
     div []
        [ h1 [] [ Html.text ("WElCOME TO CATCH THE STAR!!") ]
        ,h2 [] [Html.text ("Your job is to click on the flying Star as many times as possible.")]
        ,h2 [] [Html.text ("Everytime you catch it, you will move up a level, which means the star will be faster!")]
        ,h2 [] [Html.text ("Click on the star to begin")]
        ,h2 [] [Html.text ("How far can you reach?")]
        ,h1 [] [Html.text ("Your Current Level is:")]
        , h1 [] [ Html.text (toString model.counter)]
        , svg [width "600",height "600"]
          [circle [cx posX,cy posY, r "50", fill "red"] []]
        ]

      






{- Main -}
main : Program Never Model Msg
main = Html.program
       {init = init,
        update = update,
        view   = view,
        subscriptions = subscriptions }
