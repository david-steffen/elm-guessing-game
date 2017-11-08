import Debug exposing (log)
import Html as H exposing (Html, i)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Random

type State
    = Playing
    | Stopped
    | Finished

type Msg
    = Play
    | Stop
    | Change String
    | Guess
    | NewNumber Int

type alias Model =
    { number : Maybe Int
    , guess : Maybe Int
    , state : State
    , message : Maybe String
    , guesses : List String
    }

init : (Model, Cmd Msg)
init =
    { number = Nothing
    , guess = Nothing
    , state = Stopped
    , message = Nothing
    , guesses = []
    } ! []

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            { model
                | state = Playing
                } ! [ Random.generate NewNumber (Random.int 1 100) ]
        Stop ->
            { model
                | number = Nothing
                , state = Stopped
                } ! []
        Change newValue->
            let
                val = String.toInt newValue |> Result.toMaybe
            in
                { model
                    | guess = val
                    } ! []
        Guess ->
            checkGuess model ! []
        NewNumber number ->
            { model
                | number = Just number
                } ! []

checkGuess : Model -> Model
checkGuess ({number, guess, guesses} as m) =
    if number == guess then
        { m | state = Finished }
    else
        case guess of
            Nothing ->
                { m | message = Just "Please choose a number" }
            Just num ->
                if isWithinRange num then
                    let
                        comparedGuess = compareGuess num number
                    in
                        { m
                            | message = Just comparedGuess
                            , guesses = addGuessToList comparedGuess num m
                            }
                else
                    { m | message = Just "Guess must be between 1 and 100" }


isWithinRange guess =
    if guess < 0 then
        False
    else if guess > 100 then
        False
    else
        True


addGuessToList message guess ({ guesses } as m) =
    let
        guessString = "You guessed : " ++ toString guess ++ " - " ++ message
    in
        guessString :: guesses

compareGuess guess number =
    case number of
        Nothing ->
            Debug.crash "No number was generated"
        Just num ->
            case compare guess num of
                LT -> "Too small!"
                EQ -> "You win!"
                GT -> "Too big!"

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
-- View
-- ----


view : Model -> Html Msg
view model =
    H.div [ A.id "container" ]
        [ H.h1 []
            [ H.span [ A.class "fa icon-elm" ] []
            , H.text " Guessing Game"
            ]
        , page model
        -- , H.div [] [ H.text <| toString model.guess ]
        -- , H.div [] [ H.text <| toString model.number ]
        -- , H.div [] [ H.text <| toString model.message ]
        ]

page : Model -> Html Msg
page model =
    case model.state of
        Playing ->
            play model
        Stopped ->
            landing
        Finished ->
            finished model

play : Model -> Html Msg
play model =
    H.div []
        [ H.input [ A.type_ "number", A.placeholder "Enter your guess here", E.onInput Change ] []
        , H.button [ E.onClick Guess ] [ H.text "Guess" ]
        , displayMessage model.message
        , displayGuesses model.guesses
        ]

finished : Model -> Html Msg
finished ({number, guesses} as m) =
    case number of
        Nothing ->
            Debug.crash "No number was generated"
        Just num ->
            H.div []
                [ H.h3 [] [ H.text "Congrats!"]
                , H.p [] [ H.text "You guessed right" ]
                , H.p [] [ H.text <| "Answer - " ++ (toString num)]
                , H.p [] [ H.text <| "Number of guesses - " ++ (toString <| List.length guesses)]
                , H.button [ E.onClick Play ] [ H.text "Play again" ]
                ]

landing : Html Msg
landing =
    H.div []
        [ H.button [ E.onClick Play ] [ H.text "Start" ]

        ]

displayMessage message =
    let
        content =
            case message of
                Nothing -> ""
                Just msg -> msg
    in
        H.div []
            [ H.strong [] [ H.text content ]
            ]

displayGuesses guesses =
    H.div []
        [ H.h4 []
            [ H.text "Previous Guesses - "
            , H.span [] [ H.text <| "attempts " ++ (toString <| List.length guesses) ]
            ]
        , H.div [] (List.map (\x -> H.div [] [ H.text x ]) guesses)
        ]
-- Main
-- ----
main : Program Never Model Msg
main =
  H.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
