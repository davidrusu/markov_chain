import Html exposing (br, div, button, text, input, textarea, Attribute, p, span)
import Html.Attributes exposing (value, tabindex, style)
import Html.Events exposing (..)
import Dict
import List
import String
import Random
import Json.Decode exposing (customDecoder)

import Css exposing (Styles, setViewport)
import Css.Flex as Flex
import Css.Display as Display exposing (display)
import Css.Shadow as Shadow
import Css.Background as Background
import Css.Text as Text
import Css.Font as Font
import Css.Padding as Padding
import Css.Dimension as Dim
import Css.Margin as Margin

centered : Styles -> Styles
centered styles =
  styles
    |> display Display.Flex
    |> Flex.justifyContent Flex.JCCenter
    |> Flex.alignItems Flex.AICenter

type alias State = String
                 
initState : State
initState = ""
            
errorState : State
errorState = "ERROR!!!"

type alias MarkovChain = Dict.Dict State (List (Float, State))
type alias Model = { data : String
                   , markovChain : MarkovChain
                   , memory : Int
                   , seed : Random.Seed
                   , trainingData : String
                   , errorMsg : Maybe String
                   }

initTrainingData : String
initTrainingData = """
By A Foreigner

I like Canadians.
They are so unlike Americans.
They go home at night.
Their cigarettes don't smell bad.
Their hats fit.
They really believe that they won the war.
They don't believe in Literature.
They think Art has been exaggerated.
But they are wonderful on ice skates.
A few of them are very rich.
But when they are rich they buy more horses
Than motor cars.
Chicago calls Toronto a puritan town.
But both boxing and horse-racing are illegal
In Chicago.
Nobody works on Sunday.
Nobody.
That doesn't make me mad.
There is only one Woodbine.
But were you ever at Blue Bonnets?
If you kill somebody with a motor car in Ontario
You are liable to go to jail.
So it isn't done.
There have been over 500 people killed by motor cars
In Chicago
So far this year.
It is hard to get rich in Canada.
But it is easy to make money.
There are too many tea rooms.
But, then, there are no cabarets.
If you tip a waiter a quarter
He says 'Thank you.'
Instead of calling the bouncer.
They let women stand up in the street cars.
Even if they are good-looking.
They are all in a hurry to get home to supper
And their radio sets.
They are a fine people.
I like them.

Ernest Hemingway
"""

init : Model
init = { markovChain = trainMarkovChain <| tokenizeData initTrainingData
       , memory = 1
       , trainingData = initTrainingData
       , data = ""
       , seed = Random.initialSeed 0
       , errorMsg = Nothing
       }

suggestionStyle =
  style [ ("backgroundColor", "#aacccc")
        , ("borderRadius", "3px")
        , ("display", "inline-block")
        , ("padding", "5px")
        , ("margin", "3px")
        ]

viewSuggestion : Signal.Address Action -> State -> Html.Html
viewSuggestion address state = div [suggestionStyle, onClick address (TakeSuggestion state) ] [ text state ]

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let styles =
        [("height", "100%")]
         
          -- |> Margin.all 10 10 10 10
      inputStyle =
        [ ("width", "49%")
        , ("height", "500px")
        , ("marginRight", "0")
        , ("marginLeft", "0")
        , ("padding", "5px")
        , ("border", "none")
        , ("backgroundColor", "#faf7f0")
        , ("display", "inline-block")
        , ("resize", "none")
        , ("fontSize", "14px")
        ]
      errorString = case model.errorMsg of
                      Nothing -> ""
                      Just s  -> s
  in 
    div [style styles]
          [ div [] [ text <| "CurrentState: " ++ (toString <| currentState model.data) ]
          , div [] [ text <| "Prediction State: " ++ (toString <| predictionState model.data) ]
          , div [] [ text <| "Previous State: " ++ (toString <| previousState model.data) ]
          , div [style [("display", "inline-block")]]
                  [ button [ style [("display", "inline-block")], onClick address Daydream ] [ text "Daydream" ]
                  , div [style [("display", "inline-block")]] [ text errorString ]
                  , div [style [("display", "inline-block")]] <| [ span [ style [("display", "inline-block")] ] [] ] ++ (List.map (viewSuggestion address) <| topSuggestions 100 model)
                  ]
          , br [] []
          , textarea [ style inputStyle, tabindex -1, onInput address Input, onTab address TakeTopSuggestion, value model.data] []
          , textarea [ style inputStyle, onInput address TrainingDataInput, value model.trainingData] []
          , button [ onClick address TrainMarkovChain ] [ text "Train Markov Chain" ]
    ]

preventDefaultOptions : Options
preventDefaultOptions = { defaultOptions | preventDefault <- True }

tabDecoder : Json.Decode.Decoder Int
tabDecoder = customDecoder keyCode (\code -> case code of
                                               9 -> Result.Ok code
                                               _ -> Result.Err "Not a tab")

onTab : Signal.Address a -> a -> Attribute
onTab address action = onWithOptions "keydown" preventDefaultOptions tabDecoder (\_ -> Signal.message address action)

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue = on "input" targetValue (\str -> Signal.message address (contentToValue str))
      
type Action = NoOp
            | TrainMarkovChain
            | Daydream
            | Input String
            | TrainingDataInput String
            | TakeTopSuggestion
            | TakeSuggestion State

tokenSeperator a b =
    if endsInSpace a || a == initState then
      ""
    else
      " "
    
              
appendState : String -> State -> String
appendState data state =
  let
    curState = currentState data
    newData = if | endsInSpace data -> data
                 | otherwise -> String.slice 0 -(String.length curState) data

    data' = String.concat [newData, tokenSeperator newData state, state, " "]
  in
    data'
  
            
handleAction : Action -> Model -> Model
handleAction action model =  
    case action of
      NoOp -> model
      TakeSuggestion state ->
        { model | data <- appendState model.data state }
      TakeTopSuggestion ->
        let
          data' = case topSuggestion model of
                    Just state -> appendState model.data state
                    Nothing -> model.data
        in
          { model | data <- data' }
      Input string             -> { model | data <- string }
      TrainingDataInput string -> { model | trainingData <- string
                                          , markovChain <- trainMarkovChain (tokenizeData string) }
      TrainMarkovChain         -> { model | markovChain <- trainMarkovChain (tokenizeData model.trainingData) }
      Daydream                 ->
        let
          predictState = predictionState model.data
        in
          case nextState model.seed model.markovChain predictState of
            (Just token, newSeed) -> { model | data <- appendState model.data token
                                             , seed <- newSeed}
            (Nothing, newSeed)    -> { model | seed <- newSeed}

updateErrorMsg : Model -> Model
updateErrorMsg model =
  case topSuggestion model of
    Just _  -> { model | errorMsg <- Nothing }
    Nothing ->
      let
        msg = if previousState model.data == initState then
                String.concat [ "Interesting, I've never seen someone start with '"
                              , currentState model.data
                              , "' and initstate is '"
                              , initState
                              , "'" ]
              else 
                let
                  problemState = nStatesBack 0 model.data
                  prevState = previousState model.data
                  problem = String.concat [ prevState
                                          , tokenSeperator prevState problemState
                                          , problemState ]
                in
                  "I'm Stuck! feed me some examples that start with '" ++ problem ++ "'"
      in
        { model | errorMsg <- Just msg }

update : Action -> Model -> Model
update action = handleAction action >> updateErrorMsg

updateNextState : State -> Maybe (List (Float, State)) -> Maybe (List (Float, State))
updateNextState nextState maybeNextStates =
  case maybeNextStates of
    Nothing         -> Just [(1, nextState)]
    Just nextStates ->
      case List.filter (\(_, s) -> s == nextState) nextStates of
        []       ->
          Just <| (1, nextState) :: nextStates
        [(p, s)] as states ->
          let
            statesLen = toFloat <| List.length states
            newP = (p * statesLen + 1) / (statesLen + 1)
            otherStates = List.filter (\(_, s) -> s /= nextState) nextStates
          in
            Just <| (newP, nextState) :: otherStates
        otherwise -> Nothing -- This shouldn't happen, something went wrong

updateState : State -> State -> MarkovChain -> MarkovChain
updateState state nextState mc = Dict.update state (updateNextState nextState) mc

trainMarkovChain : List State -> MarkovChain
trainMarkovChain input =
  let
    (prev, markovChain) = List.foldl (\state (p, mc) -> (state, updateState p state mc)) (initState, Dict.empty) input
  in
    markovChain

tokenizeData : String -> List State
tokenizeData data = List.filter (\token -> token /= "") <| String.words data

probability : Random.Generator Float
probability = Random.float 0 1

-- essentially computes the integral of the probability distribution
-- e.g. [(0.1, a), (0.5, b), (0.3, c), (0.1, d)] -> [(0.1, a), (0.6, b), (0.9, c), (1, d)]
-- This makes drawing a random state weighted by this probability distribution much easier.
-- Just generate 0 < p < 1 and take the first state where cumulative p value is greater than p

cumulativePValues : List (Float, State) -> List (Float, State)
cumulativePValues nextStates =
  let
    f (p, state) ((accumP, _) :: _ as past) = (p + accumP, state)::past
    cumulativeStates = List.foldl f [(0, initState)] nextStates
  in
    List.reverse cumulativeStates

suggestionsForState : MarkovChain -> State -> List State
suggestionsForState mc state =
  case Dict.get state mc of
    Nothing -> []
    Just nextStates ->           -- nextStates is a list of (p value, state)
      List.sortBy fst nextStates -- sort by the p values
        |> List.reverse          -- we want highest p values first
        |> List.map snd          -- drop the p values and only keep the states

topSuggestion : Model -> Maybe State   
topSuggestion model = List.head <| suggestions model

topSuggestions : Int -> Model -> List State
topSuggestions n model = List.take n <| suggestions model

endsInSpace : String -> Bool
endsInSpace = String.endsWith " "
                         
suggestions : Model -> List State
suggestions model =
  let predictState = predictionState model.data
      curState = currentState model.data
      allSuggestions = suggestionsForState model.markovChain predictState
  in List.filter (String.startsWith curState) allSuggestions
      
nextState : Random.Seed -> MarkovChain -> State -> (Maybe State, Random.Seed)
nextState seed mc state =
  case Dict.get state mc of
    Nothing -> (Nothing, seed)
    Just nextStates ->
      let
        (p, newSeed) = Random.generate probability seed
        nextState = -- the first state whose commulative p value is greater or equal to the random p value 'p'
          cumulativePValues nextStates 
            |> List.filter (\(cumulativeP, _) -> cumulativeP >= p)
            |> List.head
      in
        case nextState of
          Nothing      -> (Nothing, newSeed) -- shouldn't ever happen, but w/e
          Just (_, ns) -> (Just ns, newSeed)

currentState : String -> State
currentState data =
  if endsInSpace data then
    initState
  else
    nStatesBack 0 data

predictionState : String -> State
predictionState data =
  if endsInSpace data then
    nStatesBack 0 data
  else
    nStatesBack 1 data

previousState : String -> State
previousState data = nStatesBack 1 data

nStatesBack : Int -> String -> State
nStatesBack n data = case List.head <| List.drop n <| List.reverse <| tokenizeData data of
                       Nothing    -> initState
                       Just state -> state

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

modelSignal : Signal Model
modelSignal = Signal.foldp update init actions.signal

main : Signal Html.Html
main = Signal.map (view actions.address) modelSignal
