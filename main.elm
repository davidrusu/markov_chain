import Html exposing (div, button, text, input, textarea)
import Html.Attributes exposing (value, tabindex)
import Html.Events exposing (onClick, on, targetValue, onKeyPress, onBlur, onKeyDown, onWithOptions, defaultOptions, keyCode)
import StartApp.Simple as StartApp
import Dict
import List
import String
import Random
import Json.Decode exposing (customDecoder)

type alias State = String
initState = ""

type alias  MarkovChain = Dict.Dict State (List (Float, State))
initTrainingData = """
The Project Gutenberg EBook of Les Miserables, by Victor Hugo

This eBook is for the use of anyone anywhere at no cost and with
almost no restrictions whatsoever.  You may copy it, give it away or
re-use it under the terms of the Project Gutenberg License included
with this eBook or online at www.gutenberg.org
"""
                   
model = { markovChain = trainMarkovChain (tokenizeData initTrainingData)
        , memory = 1
        , trainingData = initTrainingData
        , data = "a"
        , seed = Random.initialSeed 0}

view address model =
  div []
    [ -- div [] [ text <| toString model.markovChain ]
      div [] [ text <| "CurrentState: " ++ (toString <| currentState model) ]
    , div [] [ text <| "Suggestions:" ++ (String.join " " <| suggestions 4 model) ]
    , div [] [ textarea [ tabindex -1, onInput address Input, onTab address TakeSuggestion, value model.data] []
             , button [ onClick address Daydream ] [ text "Daydream" ]
             ]
    , div [] [ textarea [ onInput address TrainingDataInput, value model.trainingData] []
             , button [ onClick address TrainMarkovChain ] [ text "Train Markov Chain" ]
             ]
    ]

preventDefaultOptions = { defaultOptions | preventDefault <- True }
  
onTab address action = onWithOptions "keydown" preventDefaultOptions (customDecoder keyCode (\code ->  case code of
                                                                                                         9 -> Result.Ok code
                                                                                                         _ -> Result.Err "Not a tab")) (\code -> Signal.message address action)
  
onInput address contentToValue =
    on "input" targetValue (\str -> Signal.message address (contentToValue str))
      
type Action = NoOp | TrainMarkovChain | Daydream | Input String | TrainingDataInput String | TakeSuggestion

update action model =
  case action of
    NoOp                     -> model
    TakeSuggestion           -> let newData = case String.endsWith " " model.data of -- suggestionsForState model.markovChain (currentState model)  of
                                                False -> String.slice 0 -(String.length <| currentState model) model.data
                                                True  -> model.data
                                    seperator = case currentState model of
                                                  initState -> ""
                                                  _         -> " "
                                in
                                { model | data <- String.concat [newData, seperator, topSuggestion model, " "] }
    Input string             -> { model | data <- string }
    TrainingDataInput string -> { model | trainingData <- string }
    TrainMarkovChain         -> { model | markovChain <- trainMarkovChain (tokenizeData model.trainingData) }
    Daydream                 -> let curState = currentState model in
                                  case nextState model.seed model.markovChain <| curState of
                                    (Just token, newSeed) -> { model | data <- String.concat [ model.data, " ", token ]
                                                             , seed <- newSeed }
                                    (Nothing, newSeed)    -> { model | seed <- newSeed
                                                             , data <- String.concat [model.data, "b"] }

updateNextState : State -> Maybe (List (Float, State)) -> Maybe (List (Float, State))
updateNextState nextState = \mStates -> case mStates of
                                          Nothing         -> Just [(1, nextState)]
                                          Just nextStates -> case List.filter (\(_, s) -> s == nextState) nextStates of
                                                               []        -> Just <| (1, nextState) ::  nextStates
                                                               [(p, s)]  -> Just <| (p+1, nextState) :: List.filter (\(_, s) -> s /= nextState) nextStates

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

tokenizeData data = List.filter (\token -> token /= "") <| String.words data

probability : Random.Generator Float
probability = Random.float 0 1

accumPValues : List (Float, State) -> List (Float, State)
accumPValues nextStates = let (_, states) = List.foldl (\(p, state) (accumP, past) -> (p + accumP, (p + accumP, state)::past)) (0, [(0, initState)]) nextStates in List.reverse states

suggestionsForState : MarkovChain -> State -> List State
suggestionsForState mc state = List.map snd <| List.reverse <| List.sortBy fst <| case Dict.get state mc of
                                                                                    Nothing -> []
                                                                                    Just nextStates -> nextStates

topSuggestion model = case List.head <| suggestions 1 model of
                        Nothing -> "Something went bad"
                        Just suggestion -> suggestion

suggestions n model = List.take n <| case suggestionsForState model.markovChain (currentState model)  of
                                       [] -> suggestionsForState model.markovChain (previousState model)
                                       xs -> xs

nextState : Random.Seed -> MarkovChain -> State -> (Maybe State, Random.Seed)
nextState seed mc state = case Dict.get state mc of
                       Nothing -> (Nothing, seed)
                       Just nextStates -> let (p, newSeed) = Random.generate probability seed
                                              nextState = List.head <| List.filter (\(cumulativeP, state) -> cumulativeP > p) <| accumPValues nextStates in
                                          case nextState of
                                            Nothing      -> (Nothing, newSeed)
                                            Just (_, ns) -> (Just ns, newSeed)
                                             
-- this should return the initState if list is empty
currentState model = nStatesBack 0 model
                     
previousState model = nStatesBack 1 model

nStatesBack n model = case List.head <| List.drop n <| List.reverse <| tokenizeData model.data of
                       Nothing    -> initState
                       Just state -> state

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

modelSignal = Signal.foldp update model actions.signal

main = Signal.map (view actions.address) modelSignal
