module BarcodeGenerator where

import Dict
import String
import Transform2D

main = flow down [
    plainText "Barcode Generator",
    asText upcBinToDigitsL,
    asText upcBinToDigitsR,
    asText upcBinToDigitsG,
    plainText <| generateEAN13 "1234567890123",
    flow right [
        spacer 100 1,
        showBarcode 640 240 <| generateEAN13 "1234567890123" ] ]

type Binary = String

upcBinToDigitsL : Dict.Dict Char Binary
upcBinToDigitsL = [
    ('0', "0001101"),
    ('1', "0011001"),
    ('2', "0010011"),
    ('3', "0111101"),
    ('4', "0100011"),
    ('5', "0110001"),
    ('6', "0101111"),
    ('7', "0111011"),
    ('8', "0110111"),
    ('9', "0001011") ] |> Dict.fromList

upcBinToDigitsR : Dict.Dict Char Binary
upcBinToDigitsR = Dict.map invertBinaryStr upcBinToDigitsL

upcBinToDigitsG : Dict.Dict Char Binary
upcBinToDigitsG = Dict.map String.reverse upcBinToDigitsR

upcCodeToBinToDigits : Dict.Dict Char (Dict.Dict Char Binary)
upcCodeToBinToDigits = [
    ('L', upcBinToDigitsL),
    ('R', upcBinToDigitsR),
    ('G', upcBinToDigitsG) ] |> Dict.fromList

invertBinaryStr : Binary -> Binary
invertBinaryStr = String.map invertBinaryChar

invertBinaryChar : Char -> Char
invertBinaryChar c = case c of
    '0' -> '1'
    '1' -> '0'
    otherwise -> '-'

generateEAN13 : String -> Binary
generateEAN13 str =
    let startGuard = "101"
        middleGuard = "01010"
        endGuard = "101"
        front = generateEAN13Front <| String.left 7 str
        back = generateEAN13Back <| String.right 6 str
    in startGuard ++ front ++ middleGuard ++ back ++ endGuard

-- todo
generateEAN13Front : String -> Binary
generateEAN13Front str = "0000000000"

-- todo
generateEAN13Back : String -> Binary
generateEAN13Back str = "0000000000"

showBarcode : Int -> Int -> String -> Element
showBarcode w h str =
    let bin = generateEAN13 str
        frms = showBinary bin
        fx = toFloat w / toFloat (length frms)
        tsx = Transform2D.scaleX fx
        tsy = Transform2D.scaleY <| toFloat h
        t = Transform2D.multiply tsx tsy
        frm = groupTransform t frms
        -- todo: Why not centerXOffset = toFloat w / -2?
        centerXOffset = toFloat w / (fx * (-2))
    in collage w h [ frm |> moveX centerXOffset]

showBinary : Binary -> [Form]
showBinary bin =
    let frms = map showBinChar <| String.toList bin
        w = String.length bin
        xs = [0 .. w] |> map toFloat
    in zipWith (\f x -> moveX x f) frms xs

showBinChar : Char -> Form
showBinChar c =
    let col = case c of
        '1' -> black
        otherwise -> white
    in square 1 |> filled col |> moveX 0.5