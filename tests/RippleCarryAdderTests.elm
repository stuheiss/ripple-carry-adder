module RippleCarryAdderTests exposing (allTests)

import Test exposing (describe, test, fuzz3, Test)
import Fuzz exposing (..)
import Array
import Expect


-- import Test.Runner.Html exposing (run)

import RippleCarryAdder exposing (..)


{- }
   main =
       run <|
           describe "Addition"
               [ test "1 + 1 = 2" <|
                   \() ->
                       (1 + 1) |> Expect.equal 2
               ]
-}
{-
   main =
       run <| allTests
-}


allTests =
    describe "4-bit Ripple Carry Adder Components"
        [ inverterTests
        , andGateTests
        , orGateTests
        , halfAdderTests
        , fullAdderTests
        , rippleCarryAdderTests
        , rippleCarryAdderProperty1
        , rippleCarryAdderProperty2
        , rippleCarryAdderProperty3
        , additionTests
        ]


additionTests =
    describe "Addition"
        [ test "1 + 1 = 2" <|
            \() ->
                (1 + 1) |> Expect.equal 2
        ]

inverterTests =
    describe "Inverter"
        [ test "output is 0 when the input is 1" <|
            \() ->
                inverter 0
                    |> Expect.equal 1
        , test "output is 1 when the input is 0" <|
            \() ->
                inverter 1
                    |> Expect.equal 0
        ]


andGateTests =
    describe "AND gate"
        [ test "output is 0 when both inputs are 0" <|
            \() ->
                andGate 0 0
                    |> Expect.equal 0
        , test "output is 0 when the first input is 0" <|
            \() ->
                andGate 0 1
                    |> Expect.equal 0
        , test "output is 0 when the second input is 0" <|
            \() ->
                andGate 1 0
                    |> Expect.equal 0
        , test "output is 1 when both inputs are 1" <|
            \() ->
                andGate 1 1
                    |> Expect.equal 1
        ]


orGateTests =
    describe "OR gate"
        [ test "output is 0 when both inputs are 0" <|
            \() ->
                orGate 0 0
                    |> Expect.equal 0
        , test "output is 1 when the first input is 0" <|
            \() ->
                orGate 0 1
                    |> Expect.equal 1
        , test "output is 1 when the second input is 0" <|
            \() ->
                orGate 1 0
                    |> Expect.equal 1
        , test "output is 1 when both inputs are 1" <|
            \() ->
                orGate 1 1
                    |> Expect.equal 1
        ]


halfAdderTests =
    describe "Half adder"
        [ test "sum and carry-out are 0 when both inputs are 0" <|
            \() ->
                halfAdder 0 0
                    |> Expect.equal { carry = 0, sum = 0 }
        , test "sum is 1 and carry-out is 0 when the 1st input is 0 and the 2nd input is 1" <|
            \() ->
                halfAdder 0 1
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 1 and carry-out is 0 when the 1st input is 1 and the 2nd input is 0" <|
            \() ->
                halfAdder 1 0
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 0 and carry-out is 1 when both inputs are 1" <|
            \() ->
                halfAdder 1 1
                    |> Expect.equal { carry = 1, sum = 0 }
        ]


fullAdderTests =
    describe "Full adder"
        [ test "sum and carry-out are 0 when both inputs and carry-in are 0" <|
            \() ->
                fullAdder 0 0 0
                    |> Expect.equal { carry = 0, sum = 0 }
        , test "sum is 1 and carry-out is 0 when both inputs are 0, but carry-in is 1" <|
            \() ->
                fullAdder 0 0 1
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 1 and carry-out is 0 when the 1st input is 0, the 2nd input is 1, and carry-in is 0" <|
            \() ->
                fullAdder 0 1 0
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 0 and carry-out is 1 when the 1st input is 0, the 2nd input is 1, and the carry-in is 1" <|
            \() ->
                fullAdder 0 1 1
                    |> Expect.equal { carry = 1, sum = 0 }
        , test "sum is 1 and carry-out is 0 when the 1st input is 1, the 2nd input is 0, and the carry-in is 0" <|
            \() ->
                fullAdder 1 0 0
                    |> Expect.equal { carry = 0, sum = 1 }
        , test "sum is 0 and carry-out is 1 when the 1st input is 1, the 2nd input is 0, and the carry-in is 1" <|
            \() ->
                fullAdder 1 0 1
                    |> Expect.equal { carry = 1, sum = 0 }
        , test "sum is 0 and carry-out is 1 when the 1st input is 1, the 2nd input is 1, and the carry-in is 0" <|
            \() ->
                fullAdder 1 1 0
                    |> Expect.equal { carry = 1, sum = 0 }
        , test "sum is 1 and carry-out is 1 when the 1st input is 1, the 2nd input is 1, and the carry-in is 1" <|
            \() ->
                fullAdder 1 1 1
                    |> Expect.equal { carry = 1, sum = 1 }
        ]


-- four unit tests that covere the boundary cases and one more to communicate what the function actually does
rippleCarryAdderTests =
    describe "4-bit ripple carry adder"
        [ describe "given two binary numbers and a carry-in digit"
            [ test "returns the sum of those numbers and a carry-out digit" <|
                \() ->
                    rippleCarryAdder 1001 1101 1
                        |> Expect.equal 10111
            ]
        , describe "when the 1st input is 1111, and the 2nd input is 1111"
            [ test "and carry-in is 0, the output is 11110" <|
                \() ->
                    rippleCarryAdder 1111 1111 0
                        |> Expect.equal 11110
            , test "and carry-in is 1, the output is 11111" <|
                \() ->
                    rippleCarryAdder 1111 1111 1
                        |> Expect.equal 11111
            ]
        , describe "when the 1st input is 0000, and the 2nd input is 0000"
            [ test "and carry-in is 0, the output is 0000" <|
                \() ->
                    rippleCarryAdder 0 0 0
                        |> Expect.equal 0
            , test "and carry-in is 1, the output is 0001" <|
                \() ->
                    rippleCarryAdder 0 0 1
                        |> Expect.equal 1
            ]
        ]

-- fuzz tests to test the properties of rippleCarryAdder

-- convert an int to a decimal representation of a binary
intToBin : Int -> Int
intToBin x =
  let
    -- helper to convert an int to a list of binaries
    intToBinList x =
      if x == 0 then [0]
      else
        let toBin0 x =
          if x == 0 then []
          else (toBin0 <| x // 2) ++ [x % 2]
        in
          toBin0 x
  in
    intToBinList x |>
    -- add elements from the right while multiplying each elem by the next power of 10
    List.foldr (\x (result,mul) -> (result + x * mul, mul * 10)) (0,1) |>
    -- result is the first elem of the tuple
    Tuple.first

-- Property #1: If the most significant digits of both inputs are 0, the carry-out digit will always be 0
{-
Elm doesn’t provide a fuzzer for generating binary numbers.
Generate lists of binary digits of length <= 3 and convert to binary.
-}
rippleCarryAdderProperty1 : Test
rippleCarryAdderProperty1 =
    describe "carry-out's relationship with most significant digits"
        [ fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (intRange 0 1)
            "carry-out is 0 when most significant digits are both 0" <|
            \list1 list2 carryIn ->
                let
                    convertToBinary digitsList =
                        digitsList
                            |> List.take 3
                            |> numberFromDigits

                    firstInput =
                        convertToBinary list1

                    secondInput =
                        convertToBinary list2
                in
                    rippleCarryAdder firstInput secondInput carryIn
                        |> digits
                        |> List.length
                        |> Expect.lessThan 5
        ]

-- Property #2: If the most significant digits of both inputs are 1, the carry-out digit will always be 1
rippleCarryAdderProperty2 : Test
rippleCarryAdderProperty2 =
    describe "carry-out's relationship with most significant digits"
        [ fuzz3
            (intRange 8 15)
            (intRange 8 15)
            (intRange 0 1)
            "carry-out is 1 when most significant digits are both 1" <|
            \int1 int2 carryIn ->
                let
                    toBin x =
                        if x == 0 then "0"
                        else (toBin <| x // 2) ++ (toString <| x % 2)

                    firstInput =
                        Result.withDefault 0 (String.toInt <| toBin int1)

                    secondInput =
                        Result.withDefault 0 (String.toInt <| toBin int2)
                in
                    rippleCarryAdder firstInput secondInput carryIn
                        |> digits
                        |> List.length
                        |> Expect.greaterThan 4
        ]


-- Property #3: If the least significant digits of both inputs are 0 and the carry-in digit is also 0, the least significant digit of the output will always be 0
rippleCarryAdderProperty3 : Test
rippleCarryAdderProperty3 =
    describe "carry-in's relationship with least significant digits"
        [ fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (constant 0)
            """
            the least significant digit of the output is 0 when the
            carry-in is 0 and the least significant digits of both
            inputs are 0
            """ <|
            \list1 list2 carryIn ->
                let
                    firstInput =
                        convertToBinary list1

                    secondInput =
                        convertToBinary list2

                    convertToBinary digitsList =
                        digitsList
                            |> List.take 4
                            |> setLastDigitToZero
                            |> numberFromDigits

                    setLastDigitToZero digitsList =
                        Array.fromList digitsList
                            |> Array.set (lastIndex digitsList) 0
                            |> Array.toList

                    lastIndex digitsList =
                        (List.length digitsList) - 1

                    isLastDigitZero digitsList =
                        Array.fromList digitsList
                            |> Array.get (lastIndex digitsList)
                            |> Maybe.withDefault 0
                            |> (==) 0
                in
                    rippleCarryAdder firstInput secondInput carryIn
                        |> digits
                        |> isLastDigitZero
                        |> Expect.equal True
        ]

-- Property #4: If the least significant digits of both inputs are 1 and the carry-in digit is 0, the least significant digit will always be 0


