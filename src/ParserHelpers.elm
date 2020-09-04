module ParserHelpers exposing (many)

import Parser exposing (..)


many : Parser a -> Parser (List a)
many p =
    loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Step (List a) (List a))
manyHelp p vs =
    oneOf
        [ succeed (\v -> Loop (v :: vs))
            |= p
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse vs))
        ]
