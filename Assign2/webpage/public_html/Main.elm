module KeyboardExample exposing (..)

import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Keyboard as Key
import Html as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Mouse as Mouse

{- Model -}
type alias Model = {
        position : { x : Int, y : Int }
                   }

init : (Model,Cmd.Cmd Msg)
init = ({ position = {x = 300, y = 300}},Cmd.none)

{- Update -}
type Msg = MouseMsg Mouse.Position
update (MouseMsg pos) model = ({ position = {x = pos.x, y = pos.y}},Cmd.none)
{- Increment / Decrement counter with left and right keys -}

                            
{- View -}
view : Model -> Html.Html Msg
view model = let
      posX = toString model.position.x
      posY = toString model.position.y
    in svg
      [ width "1200", height "1200"]
      [ ellipse [ cx posX, cy posY, rx "20", ry "10", fill "Blue"] [] ]




subscriptions : Model -> Sub Msg
subscriptions model =  Mouse.moves MouseMsg


{- Main -}
main : Program Never Model Msg
main = Html.program
          { init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
            }

