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
            showEdit baseContent.handle "base code: 11 or 12 digits" baseContentSig,
            showEdit addonContent.handle "addon: 0, 2 or 5 digits" addonContentSig,
            flow right [
                spacer 100 1,
                displayBarcode 640 240 base addon
            ]
        ]

baseContent : Input Field.Content
baseContent = input Field.noContent

addonContent : Input Field.Content
addonContent = input Field.noContent

type Binary = String


upcDigitsToBinL : Dict.Dict Char Binary
upcDigitsToBinL = [
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

firstDigitToParities : Dict.Dict Char [Char]
firstDigitToParities = [
    ('0', "LLLLLL"),
    ('1', "LLGLGG"),
    ('2', "LLGGLG"),
    ('3', "LLGGGL"),
    ('4', "LGLLGG"),
    ('5', "LGGLLG"),
    ('6', "LGGGLL"),
    ('7', "LGLGLG"),
    ('8', "LGLGGL"),
    ('9', "LGGLGL") ] |> Dict.fromList |> Dict.map String.toList

upcDigitsToBinR : Dict.Dict Char Binary
upcDigitsToBinR = Dict.map invertBinaryStr upcDigitsToBinL

upcDigitsToBinG : Dict.Dict Char Binary
upcDigitsToBinG = Dict.map String.reverse upcDigitsToBinR

parityToDigitsToBin : Dict.Dict Char (Dict.Dict Char Binary)
parityToDigitsToBin = [
    ('L', upcDigitsToBinL),
    ('R', upcDigitsToBinR),
    ('G', upcDigitsToBinG) ] |> Dict.fromList

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

generateBarcode : String -> String -> (Binary, Binary)
generateBarcode base addon =
    let base' = baseInputToBarcodeString base
    in  if baseOK base && addonOK addon
        then (generateEAN13 base', generateAddon addon) else ("", "")

generateAddon : String -> Binary
generateAddon str = case String.length str of
    0 -> ""
    2 -> generateAddon2 str
    5 -> generateAddon5 str

fromMaybe : a -> Maybe a -> a
fromMaybe def m = case m of
    Just x -> x
    Nothing -> def

generateAddon2 : String -> Binary
generateAddon2 str =
    let value = fromMaybe 0 <| String.toInt str
        chars = String.toList str
        startGuard = "01011"
        middleGuard = "01"
        parities = addon2Parities value
        digitsToBins = map (flip Dict.getOrFail parityToDigitsToBin) parities
        binaries = zipWith Dict.getOrFail chars digitsToBins
        front = head binaries
        back = last binaries
    in  startGuard ++ front ++ middleGuard ++ back

addon2Parities : Int -> [Char]
addon2Parities value = case value `rem` 4 of
    0 -> ['L', 'L']
    1 -> ['L', 'G']
    2 -> ['G', 'L']
    3 -> ['G', 'G']

generateAddon5 : String -> Binary
generateAddon5 str = ""

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

generateEAN13Front : String -> Binary
generateEAN13Front str =
    let (first, rest) = case String.uncons str of
                            Just p -> p
                            Nothing -> ('0', "")
        parities = Dict.getOrFail first firstDigitToParities
        charDicts = map (flip Dict.getOrFail parityToDigitsToBin) parities
        chars = String.toList rest
        binaries = zipWith Dict.getOrFail chars charDicts
    in  String.concat binaries

generateEAN13Back : String -> Binary
generateEAN13Back str =
    let chars = String.toList str
        binaries = zipWith Dict.getOrFail chars <| repeat 6 upcDigitsToBinR
    in  String.concat binaries

displayBarcode : Int -> Int -> String -> String -> Element
displayBarcode w h base addon =
    let (baseBin, addonBin) = generateBarcode base addon
        baseElem = displayBinary w h baseBin
        humanReadable = flow right [
                            plainText <| baseInputToBarcodeString base,
                            spacer 10 1, plainText addon
                        ]
        addonElem = displayBinary w h addonBin -- todo size
        barcodeElem = flow right [ baseElem, spacer 100 1, addonElem]
    in  flow down [ barcodeElem, humanReadable ]

displayBinary : Int -> Int -> Binary -> Element
displayBinary w h bin =
    let frms = showBinary bin
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