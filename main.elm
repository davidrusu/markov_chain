import Html exposing (div, button, text, input, textarea, Attribute, p, span)
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
          [ div [] [ text <| "CurrentState: " ++ (toString <| currentState model) ]
          , div [] [ text <| "Prediction State: " ++ (toString <| predictionState model) ]
          , div [] [ text errorString ]
          , div [] [ button [ onClick address Daydream ] [ text "Daydream" ]
                   , div [] <| [ span [ style [("display", "inline-block")]] [] ] ++ (List.map (viewSuggestion address) <| topSuggestions 100 model)
                   ]
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

appendState : Model -> State -> String
appendState model state =
  let
    curState = currentState model
    seperator = case curState of
                  initState -> ""
                  _         -> " "
    
    newData = if | endsInSpace model.data -> model.data
                 | otherwise -> String.slice 0 -(String.length curState) model.data

    data' = String.concat [newData, seperator, state, " "]
  in
    data'
  
            
handleAction : Action -> Model -> Model
handleAction action model =  
    case action of
      NoOp -> model
      TakeSuggestion state ->
        { model | data <- appendState model state }
      TakeTopSuggestion ->
        let
          data' = case topSuggestion model of
                    Just state -> appendState model state
                    Nothing -> model.data
        in
          { model | data <- data' }
      Input string             -> { model | data <- string }
      TrainingDataInput string -> { model | trainingData <- string
                                          , markovChain <- trainMarkovChain (tokenizeData string) }
      TrainMarkovChain         -> { model | markovChain <- trainMarkovChain (tokenizeData model.trainingData) }
      Daydream                 ->
        let
          predictState = predictionState model
        in
          case nextState model.seed model.markovChain predictState of
            (Just token, newSeed) -> { model | data <- appendState model token
                                             , seed <- newSeed}
            (Nothing, newSeed)    -> { model | seed <- newSeed}

updateErrorMsg : Model -> Model
updateErrorMsg model =
  let
    problem = (previousState model) ++ " " ++ (currentState model)
  in
    case topSuggestion model of
      Nothing -> { model | errorMsg <- Just <| "I'm Stuck! feed some examples that start with '" ++ problem ++ "'" }
      Just _  -> { model | errorMsg <- Nothing }

update : Action -> Model -> Model
update action = handleAction action >> updateErrorMsg

updateNextState : State -> Maybe (List (Float, State)) -> Maybe (List (Float, State))
updateNextState nextState maybeNextStates = case maybeNextStates of
                                              Nothing         -> Just [(1, nextState)]
                                              Just nextStates -> case List.filter (\(_, state) -> state == nextState) nextStates of
                                                                   []        -> Just <| (1, nextState) ::  nextStates
                                                                   [(p, s)]  -> Just <| (p+1, nextState) :: List.filter (\(_, s) -> s /= nextState) nextStates
                                                                   otherwise -> Nothing -- Something went wrong

updateState : State -> State -> MarkovChain -> MarkovChain
updateState state nextState mc = Dict.update state (updateNextState nextState) mc

normalizeNextStates : List (Float, State) -> List (Float, State)
normalizeNextStates nextStates = let totalP = List.sum <| List.map fst nextStates in
                                 List.map (\(p, s) -> (p/totalP, s)) nextStates

normalizeProbabilities : MarkovChain -> MarkovChain
normalizeProbabilities mc = Dict.map (\_ -> normalizeNextStates)  mc

trainMarkovChain : List State -> MarkovChain
trainMarkovChain input = let (prev, markovChain) = List.foldl (\state (p, mc) -> (state, updateState p state mc)) (initState, Dict.empty) input in
                         markovChain |> normalizeProbabilities

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
    f (p, state) ((accumP, _)::_ as past) = (p + accumP, state)::past
    cumulativeStates = List.foldl f [(0, initState)] nextStates
  in List.reverse cumulativeStates

suggestionsForState : MarkovChain -> State -> List State
suggestionsForState mc state = List.map snd <| List.reverse <| List.sortBy fst <| case Dict.get state mc of
                                                                                    Nothing -> []
                                                                                    Just nextStates -> nextStates


topSuggestion : Model -> Maybe State   
topSuggestion model = List.head <| suggestions model

topSuggestions : Int -> Model -> List State
topSuggestions n model = List.take n <| suggestions model

endsInSpace : String -> Bool
endsInSpace = String.endsWith " "
                         
suggestions : Model -> List State
suggestions model =
  let predictState = predictionState model
      curState = currentState model
      allSuggestions = case suggestionsForState model.markovChain predictState  of
                         --[] -> suggestionsForState model.markovChain (previousState model)
                         xs -> xs
  in List.filter (String.startsWith curState) allSuggestions
      
nextState : Random.Seed -> MarkovChain -> State -> (Maybe State, Random.Seed)
nextState seed mc state = case Dict.get state mc of
                       Nothing -> (Nothing, seed)
                       Just nextStates -> let (p, newSeed) = Random.generate probability seed
                                              nextState = List.head <| List.filter (\(cumulativeP, state) -> cumulativeP > p) <| cumulativePValues nextStates in
                                          case nextState of
                                            Nothing      -> (Nothing, newSeed)
                                            Just (_, ns) -> (Just ns, newSeed)
                                             
-- this should return the initState if list is empty
currentState : Model -> State
currentState model = if endsInSpace model.data then
                       initState
                     else
                       nStatesBack 0 model

predictionState model = if endsInSpace model.data then
                          nStatesBack 0 model
                        else
                          nStatesBack 1 model
                     
previousState : Model -> State
previousState model = nStatesBack 1 model

nStatesBack : Int -> Model -> State
nStatesBack n model = case List.head <| List.drop n <| List.reverse <| tokenizeData model.data of
                       Nothing    -> initState
                       Just state -> state

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

modelSignal : Signal Model
modelSignal = Signal.foldp update init actions.signal

main : Signal Html.Html
main = Signal.map (view actions.address) modelSignal
