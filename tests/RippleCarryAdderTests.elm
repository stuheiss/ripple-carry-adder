module RippleCarryAdderTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, constant, intRange)
import Test exposing (..)
import Array
import RippleCarryAdder exposing (..)


suite : Test
suite =
    describe "4-bit Ripple Carry Adder Components rippleCarryAdderProperty only"
        [ rippleCarryAdderProperty1
        , rippleCarryAdderProperty2
        , rippleCarryAdderProperty3
        , rippleCarryAdderProperty4
        ]



{--
suite : Test
suite =
    describe "4-bit Ripple Carry Adder Components everything"
        [ inverterTests
        , andGateTests
        , orGateTests
        , halfAdderTests
        , fullAdderTests
        , rippleCarryAdderTests
        , rippleCarryAdderProperty1
        , rippleCarryAdderProperty2
        , rippleCarryAdderProperty3
        , rippleCarryAdderProperty4
        , additionTests
        ]
--}


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
-- helper: set the nth elem of list xs to v and return the modified list


setDigitToValue : Int -> a -> List a -> List a
setDigitToValue n v xs =
    Array.fromList xs
        |> Array.set n v
        |> Array.toList



-- Property #1: If the most significant digits of both inputs are 0, the carry-out digit will always be 0


rippleCarryAdderProperty1 : Test
rippleCarryAdderProperty1 =
    describe "#1 carry-out's relationship with most significant digits both 0"
        [ fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (intRange 0 1)
            "#1 carry-out is 0 when most significant digits are both 0"
          <|
            \list1 list2 carryIn ->
                let
                    firstInput =
                        list1 |> padZeros 4 |> List.take 4 |> setDigitToValue 0 0 |> numberFromDigits

                    secondInput =
                        list2 |> padZeros 4 |> List.take 4 |> setDigitToValue 0 0 |> numberFromDigits
                in
                    rippleCarryAdder firstInput secondInput carryIn
                        |> Expect.lessThan 10000
        ]



-- Property #2: If the most significant digits of both inputs are 1, the carry-out digit will always be 1


rippleCarryAdderProperty2 : Test
rippleCarryAdderProperty2 =
    describe "#2 carry-out's relationship with most significant digits both 1"
        [ fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (intRange 0 1)
            "#2 carry-out is 1 when most significant digits are both 1"
          <|
            \list1 list2 carryIn ->
                let
                    firstInput =
                        list1 |> padZeros 4 |> List.take 4 |> setDigitToValue 0 1 |> numberFromDigits

                    secondInput =
                        list2 |> padZeros 4 |> List.take 4 |> setDigitToValue 0 1 |> numberFromDigits
                in
                    rippleCarryAdder firstInput secondInput carryIn
                        |> Expect.greaterThan 1111
        ]



-- Property #3: If the least significant digits of both inputs are 0 and the carry-in digit is also 0, the least significant digit of the output will always be 0


rippleCarryAdderProperty3 : Test
rippleCarryAdderProperty3 =
    describe "#3 carry-in's relationship with least significant digits both 0 and carry-in 0"
        [ fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (constant 0)
            "#3 the least significant digit of the output is 0 when the carry-in is 0 and the least significant digits of both inputs are 0"
          <|
            \list1 list2 carryIn ->
                let
                    firstInput =
                        list1 |> padZeros 4 |> List.take 4 |> setDigitToValue 3 0 |> numberFromDigits

                    secondInput =
                        list2 |> padZeros 4 |> List.take 4 |> setDigitToValue 3 0 |> numberFromDigits
                in
                    rippleCarryAdder firstInput secondInput carryIn % 10 |> Expect.equal 0
        ]



-- Property #4: If the least significant digits of both inputs are 1 and the carry-in digit is 0, the least significant digit will always be 0


rippleCarryAdderProperty4 : Test
rippleCarryAdderProperty4 =
    describe "carry-in's relationship with least significant digits both 1 and carry-in 0"
        [ fuzz3
            (list (intRange 0 1))
            (list (intRange 0 1))
            (constant 0)
            "the least significant digit of the output is 0 when the carry-in is 0 and the least significant digits of both inputs are 1"
          <|
            \list1 list2 carryIn ->
                let
                    firstInput =
                        list1 |> padZeros 4 |> List.take 4 |> setDigitToValue 3 1 |> numberFromDigits

                    secondInput =
                        list2 |> padZeros 4 |> List.take 4 |> setDigitToValue 3 1 |> numberFromDigits
                in
                    rippleCarryAdder firstInput secondInput carryIn % 10 |> Expect.equal 0
        ]
