in exposing (..)

 

import Html exposing (..)

import Html.Attributes exposing (..)

myStyle1 : Attribute msg
myStyle1 = 
 style
    [ ("backgroundColor", "blue")
    , ("height", "100px")
    ,("color", "white")
    ,("text-align","center")   
    ]

orange : Attribute msg
orange = 
 style
    [ ("backgroundColor", "tomato")
    ,("color", "white")    
    ]    


main = div [orange] [
        header [myStyle1] [
        h1 [] [text "My Ugly Nonsense Webpage"]
                  ],     

        section [orange] [

        h2 [] [text "First section of Nonsense"],

        p [style [("display", "block"), ("margin-left", "auto"), ("text-align","right")]] [text "asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdfasdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf"],


        ol [style [("text-align","left")]] [
                h2 [] [text "Useless List"],
                li [] [text "Link 1"],
                li [] [text "Link 2"],
                li [] [text "Link 3"],
                li [] [text "Link 4"],
                li [] [text "Link 5"],
                li [] [text "Link 6"],
                li [] [text "Link 7"]            ]
                   ],

        aside [orange] [
        h2 [] [text "Second Section of More Nonsense"],
        p [style [("text-align","justify")]] [text "fat man sees small door he knows he cannot fit through tears flow free now"]
                   ],

        footer [orange] [
                h4 [style [("text-align","center")]] [text "Copyright NonsenseCompany© 2018"]
                   ]
        ]















