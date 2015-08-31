import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Dict
import List
import String
import Random

type alias State = String

type alias  MarkovChain = Dict.Dict State (List (Float, State))
initTrainingData = """
Overeating food addiction is far, far worse because of dramatically-reduced lifespan from many forms of cancer, chronic heart diseases and the lost of limbs and eyesight from Type II diabetes. A neighbor of my childhood home whom worked at Spectra Physics died within six months of uncontrolled diabetes. I also lost a great uncle whom went the same way more recently within about 10 months.
In northern Canada and American Indian reservations, that might be true. Some places, it's heroin and others, it's meth, paint thinner or glue. (Police, nurses and social workers typically know what's going around.)
From a health perspective, most doctors would probably argue smoking. Philip Morris International is targeting Asian countries where they are as ubiquitous in most realms of life as was soda advertisements to kids in schools.
Alcohol abuse culture is prevalent in military, universities and sales/management.
I think the impact varies with location and social circles, and is hard to qualtitatively measure (apart from lives, hospital visits/bills, lost wages) because it's very qualitative.
"""
model = { markovChain = trainMarkovChain (tokenizeData initTrainingData)
        , memory = 1
        , trainingData = initTrainingData
        , data = "a"
        , seed = Random.initialSeed 0}

view address model =
  div []
    [ div [] [ text <| toString model.markovChain ]
    , div [] [ text <| case currentState model of
                         Nothing -> "No cur state"
                         Just curToken -> toString <| nextState model.seed model.markovChain curToken ]
    , div [] [ text model.data ]
    , button [ onClick address Increment ] [ text "+" ]
    , button [ onClick address Decrement ] [ text "-" ]
    ]


  
type Action = Increment | Decrement

updateNextState : State -> Maybe (List (Float, State)) -> Maybe (List (Float, State))
updateNextState nextState = \mStates -> case mStates of
                                          Nothing         -> Just [(1, nextState)]
                                          Just nextStates -> case List.filter (\(_, s) -> s == nextState) nextStates of
                                                               []        -> Just <| (1, nextState) ::  nextStates
                                                               [(p, s)]  -> Just <| (p+1, nextState) :: List.filter (\(_, s) -> s /= nextState) nextStates

updateState : State -> State -> MarkovChain -> MarkovChain
updateState state nextState mc = Dict.update state (updateNextState nextState) mc

normalizeProbabilities : MarkovChain -> MarkovChain
normalizeProbabilities mc = Dict.map (\state nextStates -> let totalP = List.sum <| List.map (\(p, _) -> p) nextStates in List.map (\(p, s) -> (p/totalP, s)) nextStates) mc

trainMarkovChain : List State -> MarkovChain
trainMarkovChain input = let (prev, markovChain) = List.foldl (\state (p, mc) -> (state, updateState p state mc)) ("", Dict.empty) input in markovChain |> normalizeProbabilities

tokenizeData data = List.filter (\token -> token /= "") <| String.words data

probability : Random.Generator Float
probability = Random.float 0 1

accumPValues : List (Float, State) -> List (Float, State)
accumPValues nextStates = let (_, states) = List.foldl (\(p, state) (accumP, past) -> (p + accumP, (p + accumP, state)::past)) (0, [(0, "")]) nextStates in List.reverse states
              
nextState : Random.Seed -> MarkovChain -> State -> (Maybe State, Random.Seed)
nextState seed mc state = case Dict.get state mc of
                       Nothing -> (Nothing, seed)
                       Just nextStates -> let (p, newSeed) = Random.generate probability seed
                                              nextState = List.head <| List.filter (\(cumulativeP, state) -> cumulativeP > p) <| accumPValues nextStates in
                                          case nextState of
                                            Nothing      -> (Nothing, newSeed)
                                            Just (_, ns) -> (Just ns, newSeed)
                                             

currentState model = List.head <| List.reverse <| tokenizeData model.data
                                                            
update action model =
  case action of
    Increment -> { model | markovChain <- trainMarkovChain (tokenizeData model.trainingData) }
    Decrement -> case currentState model of
                   Nothing       -> { model | data <- String.concat [model.data, "a"] }
                   Just curToken -> case nextState model.seed model.markovChain <| curToken of
                                      (Just token, newSeed) -> { model | data <- String.concat [ model.data, " ", token ]
                                                                       , seed <- newSeed }
                                      (Nothing, newSeed)    -> { model | seed <- newSeed
                                                                       , data <- String.concat [model.data, "b"]}


main = StartApp.start { model = model
                      , view = view
                      , update = update }
