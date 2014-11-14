module BarcodeGenerator where

import Dict
import Regex
import String
import Transform2D
import Graphics.Input (Input, input)
import Graphics.Input.Field as Field

main : Signal Element
main = scene <~ baseContent.signal ~ addonContent.signal

scene : Field.Content -> Field.Content -> Element
scene baseContentSig addonContentSig =
    let base = baseContentSig.string
        addon = addonContentSig.string
        showEdit h = Field.field Field.defaultStyle h identity
    in  flow down [
            plainText "Barcode Generator",
            asText upcBinToDigitsL,
            asText upcBinToDigitsR,
            asText upcBinToDigitsG,
            showEdit baseContent.handle "123456789012" baseContentSig,
            showEdit addonContent.handle "12345" addonContentSig,
            plainText <| generateBarcode base addon, -- debug output
            flow right [
                spacer 100 1,
                showBarcode 640 240 <| generateBarcode base addon
            ],
            flow right [ plainText <| baseInputToBarcodeString base, spacer 10 1, plainText addon ]
        ]

baseContent : Input Field.Content
baseContent = input Field.noContent

addonContent : Input Field.Content
addonContent = input Field.noContent

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

baseInputToBarcodeString : String -> String
baseInputToBarcodeString base =
    let base' = String.padLeft 12 '0' base
        base'' = appendCheckDigit base'
    in  if baseOK base then appendCheckDigit base'' else ""

{- Input must have length 12. -}
appendCheckDigit : String -> String
appendCheckDigit str = str ++ calcCheckDigit str

baseOK : String -> Bool
baseOK = Regex.contains (Regex.regex "^\\d{11,12}$")

addonOK addon = addon == "" ||
    Regex.contains (Regex.regex "^\\d{2}$") addon ||
    Regex.contains (Regex.regex "^\\d{5}$") addon

generateBarcode : String -> String -> Binary
generateBarcode base addon =
    let base' = baseInputToBarcodeString base
    in  if baseOK base && addonOK addon then generateEAN13 base' else ""

{- Input must have length 13. -}
generateEAN13 : String -> Binary
generateEAN13 str =
    let startGuard = "101"
        middleGuard = "01010"
        endGuard = "101"
        front = String.left 7 str |> generateEAN13Front
        back = String.right 6 str |> generateEAN13Back
    in  startGuard ++ front ++ middleGuard ++ back ++ endGuard

{- Input must have length 12. -}
calcCheckDigit : String -> String
calcCheckDigit str =
    let vals = str |> String.reverse |> String.toList
            |> filterMap ((\x -> [x]) >> String.fromList >> String.toInt)
        f (a, b) = 3 * a + b
        s = vals |> nonOverlappingPairs |> map f |> sum
    in  if length vals == 12
        then 10 - s `rem` 10 |> show
        else ""

{-| nonOverlappingPairs [1,2,3,4,5] === [(1,2),(3,4)] -}
nonOverlappingPairs : [a] -> [(a,a)]
nonOverlappingPairs l = case l of
    (x1::x2::xs) -> (x1,x2) :: nonOverlappingPairs xs
    _ -> []

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
    in  collage w h [ frm |> moveX centerXOffset]

showBinary : Binary -> [Form]
showBinary bin =
    let frms = map showBinChar <| String.toList bin
        w = String.length bin
        xs = [0 .. w] |> map toFloat
    in  zipWith (\f x -> moveX x f) frms xs

showBinChar : Char -> Form
showBinChar c =
    let col = case c of
        '1' -> black
        otherwise -> white
    in  square 1 |> filled col |> moveX 0.5