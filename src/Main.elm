port module Main exposing (main)

import Browser
import Html exposing (Html)
import Graph exposing (Graph, NodeId)
import Graph.DOT exposing (Rankdir(..))
import Dict
import IntDict


port showCircuitPort : String -> Cmd msg


type alias Model =
  { circuit : Circuit }


type alias Circuit =
  { inputPins : List NodeId
  , outputPins : List NodeId
  , pins: Graph Bool ()
  }


type Gate
  = NandGate
    { hiddenWires : List Wire
    , outputWires : List Wire
    }
  | CompositeGate
    { inputWires : List Wire
    , gates : List Gate
    , outputPins : List NodeId
    }


type alias Wire = (NodeId, NodeId)


init : () -> (Model, Cmd Msg)
init _ =
  let
    setInputPins =
       setPins
        [ (0, False)
        , (1, True)
        ]
    circuit = eval <| setInputPins <| construct andGate
  in
  ({ circuit =
    circuit
  }
  , showCircuit circuit
  )


construct : Gate -> Circuit
construct gate =
  let
    inputs =
      case gate of
        NandGate _ ->
          [ 0, 1 ]
        CompositeGate { inputWires } ->
          List.map Tuple.first inputWires

    nodes =
      List.repeat (countNodes gate) False

    countNodes : Gate -> Int
    countNodes g =
      case g of
        NandGate _ ->
          3
        CompositeGate { inputWires, gates, outputPins } ->
          List.length inputWires + (List.sum <| List.map countNodes gates) + List.length outputPins
    
    gatherWires : Gate -> List Wire
    gatherWires g =
      case g of
        NandGate { hiddenWires, outputWires } ->
          hiddenWires ++ outputWires
        CompositeGate { inputWires, gates } ->
          inputWires ++ List.concat (List.map gatherWires gates)

    wires =
      gatherWires gate

    outputs =
      case gate of
        NandGate { outputWires } ->
          List.map Tuple.second outputWires
        CompositeGate { outputPins } ->
          outputPins

    pins =
      Graph.fromNodeLabelsAndEdgePairs nodes wires
  in
  { inputPins = inputs
  , outputPins = outputs
  , pins = pins
  }

showCircuit : Circuit -> Cmd Msg
showCircuit c =
  let
    styles =
      { rankdir = LR
      , graph = ""
      , node = ""
      , edge = ""
      }

    nodeLabelToString label =
      if label then "1" else "0"

    edgeLabelToString =
      always ""
    
    nodeLabelToAttributes label =
      Dict.fromList <|
        [ ("label", nodeLabelToString label)
        ]
        ++ if label then
          [ ("color", "green") ]
        else
          []
    
    edgeLabelToAttributes label =
      Dict.fromList
        [ ("label", edgeLabelToString label)
        ]
  in
  showCircuitPort <| Graph.DOT.outputWithStylesAndAttributes styles nodeLabelToAttributes edgeLabelToAttributes c.pins


andGate : Gate
andGate =
  CompositeGate
    { inputWires =
      [ (0, 2)
      , (1, 3)
      ]
    , outputPins =
      [ 8 ]
    , gates =
      [ NandGate
        { hiddenWires =
          [ (2, 4)
          , (3, 4)
          ]
        , outputWires =
          [ (4, 5)
          , (4, 6)
          ]
        }
      , NandGate
        { hiddenWires =
          [ (5, 7)
          , (6, 7)
          ]
        , outputWires =
          [ (7, 8)
          ]
        }
      ]
    }


type Msg
  = NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)


eval : Circuit -> Circuit
eval c =
  let
    startIds =
      List.map
      (\inputId ->
        let
          nextId =
            Graph.get inputId c.pins
              |> Maybe.andThen
                (\ctx -> Maybe.map Tuple.first <| List.head <| IntDict.toList ctx.outgoing)
        in
        Maybe.withDefault 0 nextId
      )
      c.inputPins
  in
  evalHelper startIds c


evalHelper : List NodeId -> Circuit -> Circuit
evalHelper ids c =
  if List.length ids == 0 then
    c
  else
    let
      (newValues, nextIdLists) =
        List.unzip <| List.map
          (\id ->
            case Graph.get id c.pins of
              Just ctx ->
                let
                  inputIds =
                    List.map Tuple.first <| IntDict.toList ctx.incoming
                  newValue =
                    case inputIds of
                      headId :: tailIds ->
                        let
                          firstValue =
                            getValue headId c
                        in
                        case tailIds of
                          [] ->
                            firstValue
                          secondId :: _ ->
                            nand firstValue (getValue secondId c)
                      [] ->
                        ctx.node.label
                  nextIdList =
                    List.map Tuple.first <| IntDict.toList ctx.outgoing
                in
                (newValue, nextIdList)
              Nothing ->
                (False, []) -- impossible
            )
            ids
      updatedCircuit =
        setPins (List.map2 Tuple.pair ids newValues) c
      nextIds =
        List.concat nextIdLists
    in
    evalHelper nextIds updatedCircuit


setPins : List (NodeId, Bool) -> Circuit -> Circuit
setPins newValues c =
  List.foldl
    (\(id, newValue) newCircuit ->
      { newCircuit
        | pins =
          Graph.update
            id
            (\currentCtx ->
              Maybe.map
                (\ctx ->
                  let
                    node = ctx.node
                  in
                  { ctx
                    | node = { node | label = newValue }
                  }
                )
                currentCtx
            )
            newCircuit.pins
      }
    )
    c
    newValues

nand : Bool -> Bool -> Bool
nand a b =
  not (a && b)


getValue : NodeId -> Circuit -> Bool
getValue id c =
  case Graph.get id c.pins of
    Just ctx ->
      ctx.node.label
    Nothing ->
      False


view : Model -> Html Msg
view _ =
  Html.div [] []


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
