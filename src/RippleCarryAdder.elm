module RippleCarryAdder exposing (..)

import Bitwise
import Array


andGate : Int -> Int -> Int
andGate a b =
    Bitwise.and a b


orGate : Int -> Int -> Int
orGate a b =
    Bitwise.or a b


inverter : Int -> Int
inverter a =
    case a of
        0 ->
            1

        1 ->
            0

        _ ->
            -1


halfAdder : Int -> Int -> { carry : Int, sum : Int }
halfAdder a b =
    let
        d =
            orGate a b

        e =
            andGate a b
                |> inverter

        sumDigit =
            andGate d e

        carryOut =
            andGate a b
    in
        { carry = carryOut
        , sum = sumDigit
        }


fullAdder : Int -> Int -> Int -> { sum : Int, carry : Int }
fullAdder a b carryIn =
    let
        firstResult =
            halfAdder b carryIn

        secondResult =
            halfAdder a firstResult.sum

        finalCarry =
            orGate firstResult.carry secondResult.carry
    in
        { carry = finalCarry
        , sum = secondResult.sum
        }



{-
   rippleCarryAdder0 :
       ( Int, Int, Int, Int )
       -> ( Int, Int, Int, Int )
       -> Int
       -> { carry : Int, sum0 : Int, sum1 : Int, sum2 : Int, sum3 : Int }
   rippleCarryAdder0 ( a3, a2, a1, a0 ) ( b3, b2, b1, b0 ) carryIn =
       let
           firstResult =
               fullAdder a0 b0 carryIn

           secondResult =
               fullAdder a1 b1 firstResult.carry

           thirdResult =
               fullAdder a2 b2 secondResult.carry

           finalResult =
               fullAdder a3 b3 thirdResult.carry
       in
           { carry = finalResult.carry
           , sum3 = finalResult.sum
           , sum2 = thirdResult.sum
           , sum1 = secondResult.sum
           , sum0 = firstResult.sum
           }
-}


digits : Int -> List Int
digits number =
    if number == 0 then
        []
    else
        digits (number // 10) ++ [ number % 10 ]


extractDigits : Int -> ( number, number1, number2, number3 )
extractDigits number =
    digits number
        |> padZeros 4
        |> Array.fromList
        |> arrayToTuple


arrayToTuple : Array.Array number -> ( number1, number2, number3, number4 )
arrayToTuple array =
    let
        firstElement =
            Array.get 0 array
                |> Maybe.withDefault -1

        secondElement =
            Array.get 1 array
                |> Maybe.withDefault -1

        thirdElement =
            Array.get 2 array
                |> Maybe.withDefault -1

        fourthElement =
            Array.get 3 array
                |> Maybe.withDefault -1
    in
        ( firstElement, secondElement, thirdElement, fourthElement )


padZeros : Int -> List number -> List number
padZeros total list =
    let
        numberOfZeros =
            total - (List.length list)
    in
        (List.repeat numberOfZeros 0) ++ list


rippleCarryAdder : Int -> Int -> Int -> Int
rippleCarryAdder a b carryIn =
    let
        -- Extract digits
        ( a3, a2, a1, a0 ) =
            extractDigits a

        ( b3, b2, b1, b0 ) =
            extractDigits b

        -- Compute sum
        firstResult =
            fullAdder a0 b0 carryIn

        secondResult =
            fullAdder a1 b1 firstResult.carry

        thirdResult =
            fullAdder a2 b2 secondResult.carry

        finalResult =
            fullAdder a3 b3 thirdResult.carry
    in
        [ finalResult, thirdResult, secondResult, firstResult ]
            |> List.map .sum
            |> (::) finalResult.carry
            |> numberFromDigits


numberFromDigits : List number -> number
numberFromDigits digitsList =
    List.foldl (\digit number -> digit + 10 * number) 0 digitsList
