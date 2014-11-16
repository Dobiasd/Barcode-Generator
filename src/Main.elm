-- todo docstrings, links, type safety, size scaling

module BarcodeGenerator where

import Dict
import Regex
import String
import Text
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
            spacer 640 1,
            plainText "Barcode Generator",
            showEdit baseContent.handle
                "base code: 11 or 12 digits"
                baseContentSig,
            showEdit addonContent.handle
                "addon: 0, 2 or 5 digits"
                addonContentSig,
            flow right [
                spacer 100 1,
                displayBarcode 1 base addon
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

baseInputToBarcodeString : Bool -> String -> String
baseInputToBarcodeString leadingZeroIfNeeded base =
    let base' = String.padLeft 12 '0' base
        checkDigit = calcCheckDigit base'
        baseOut = (if leadingZeroIfNeeded then base' else base) ++ checkDigit
    in  if baseOK base then baseOut else ""

baseOK : String -> Bool
baseOK = Regex.contains (Regex.regex "^\\d{11,12}$")

addonOK addon = addon == "" ||
    Regex.contains (Regex.regex "^\\d{2}$") addon ||
    Regex.contains (Regex.regex "^\\d{5}$") addon

generateBarcode : String -> String -> (Binary, Binary)
generateBarcode base addon =
    let base' = baseInputToBarcodeString True base
    in  (if baseOK base then generateEAN13 base' else "",
         if addonOK addon then generateAddon addon else "")

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
addon2Parities checksum = case checksum `rem` 4 of
    0 -> ['L', 'L']
    1 -> ['L', 'G']
    2 -> ['G', 'L']
    3 -> ['G', 'G']

addon5Parities : Int -> [Char]
addon5Parities checksum = case checksum `rem` 10 of
    0 -> ['G', 'G', 'L', 'L', 'L']
    1 -> ['G', 'L', 'G', 'L', 'L']
    2 -> ['G', 'L', 'L', 'G', 'L']
    3 -> ['G', 'L', 'L', 'L', 'G']
    4 -> ['L', 'G', 'G', 'L', 'L']
    5 -> ['L', 'L', 'G', 'G', 'L']
    6 -> ['L', 'L', 'L', 'G', 'G']
    7 -> ['L', 'G', 'L', 'G', 'L']
    8 -> ['L', 'G', 'L', 'L', 'G']
    9 -> ['L', 'L', 'G', 'L', 'G']

generateAddon5 : String -> Binary
generateAddon5 str =
    let startGuard = "01011"
        separator = "01"
        digitValues = stringToDigitValues str
        parities = addon5Parities <| calcCheckSum (3, 9) digitValues
        charDicts = map (flip Dict.getOrFail parityToDigitsToBin) parities
        chars = String.toList str
        binaries = zipWith Dict.getOrFail chars charDicts
    in startGuard ++ (intersperse separator binaries |> String.concat)

{-| Input must have length 13. -}
generateEAN13 : String -> Binary
generateEAN13 str =
    let startGuard = "101"
        middleGuard = "01010"
        endGuard = "101"
        front = String.left 7 str |> generateEAN13Front
        back = String.right 6 str |> generateEAN13Back
    in  startGuard ++ front ++ middleGuard ++ back ++ endGuard

{-| Input must have length 12. -}
calcCheckDigit : String -> String
calcCheckDigit str =
    let vals = str |> String.reverse |> stringToDigitValues
        s = calcCheckSum (3, 1) vals
    in  if length vals == 12
        then 10 - s `rem` 10 |> \x -> x `rem` 10 |> show
        else ""

stringToDigitValues = String.toList >>
    filterMap ((\x -> [x]) >>
        String.fromList >>
        String.toInt)

calcCheckSum : (Int, Int) -> [Int] -> Int
calcCheckSum (m1, m2) xs =
    let xs' = if length xs `rem` 2 == 0 then xs else xs ++ [0]
        f (a, b) = m1 * a + m2 * b
    in  xs' |> nonOverlappingPairs |> map f |> sum

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

-- todo all on one canvas for saving
displayBarcode : Int -> String -> String -> Element
displayBarcode xSizeFactor baseStr addonStr =
    let (baseBin, addonBin) = generateBarcode baseStr addonStr
        base = displayBinary xSizeFactor baseH baseBin
        addon = displayBinary xSizeFactor addonH addonBin
        textBaseDistY = 1
        addonDistX = 12
        addonTextDistX = 10

        textHeight = 10

        baseX1 = 10
        baseY1 = textHeight + textBaseDistY
        baseW = String.length baseBin |> toFloat
        baseX2 = baseX1 + baseW
        baseH = 66
        baseY2 = baseY1 + baseH

        guard1X1 = baseX1
        guard1Y1 = textHeight / 2
        guard1Y2 = baseY1
        guardH = guard1Y2 - guard1Y1

        textBaseY1 = 0
        textBaseY2 = textHeight

        textBaseSingleX1 = 0

        textBaseLeftX1 = guard2X2 + 3
        --textBaseLeftX2 = guard3X1 - 3

        textBaseRightX1 = guard4X2 + 3
        --textBaseRightX2 = guard5X1 - 3

        guard2X1 = guard1X1 + 2
        guard3X1 = guard1X1 + 46
        guard4X1 = guard3X1 + 2
        guard5X1 = guard3X1 + 46
        guard6X1 = guard5X1 + 2

        guard1X2 = guard1X1 + 1
        guard2X2 = guard2X1 + 1
        guard3X2 = guard3X1 + 1
        guard4X2 = guard4X1 + 1
        guard5X2 = guard5X1 + 1
        guard6X2 = guard6X1 + 1

        guard = rect 1 guardH
            |> filled black
            |> move (0.5, guardH / 2 |> ceiling |> toFloat)
            |> moveY guard1Y1
        guard1 = guard |> moveX guard1X1
        guard2 = guard |> moveX guard2X1
        guard3 = guard |> moveX guard3X1
        guard4 = guard |> moveX guard4X1
        guard5 = guard |> moveX guard5X1
        guard6 = guard |> moveX guard6X1
        guards = group [guard1, guard2, guard3, guard4, guard5, guard6]

        addonX1 = baseX2 + addonDistX
        addonY1 = baseY1
        addonW = String.length addonBin |> toFloat
        addonX2 = addonX1 + addonW
        addonY2 = baseY2 - (1 + textHeight + 3)
        addonH = addonY2 - addonY1

        textAddonX1 = addonX1 + addonTextDistX
        textAddonY1 = addonY2 + 3
        textAddonX2 = addonX2 - 1

        (strBaseSingle, strBaseLeft, strBaseRight) =
            baseInputToBarcodeString False baseStr |> splitBaseStr
        textBaseSingle = showText textHeight strBaseSingle
        textBaseLeft = showText textHeight strBaseLeft
        textBaseRight = showText textHeight strBaseRight
        textAddon = showText textHeight addonStr

        collageW = addonX2 |> ceiling
        collageH = baseY2 |> ceiling

        mainForm = group [ base |> move (baseX1, baseY1),
                           guards,
                           addon |> move (addonX1, addonY1),
                           textBaseSingle |> move (textBaseSingleX1, textBaseY1),
                           textBaseLeft |> move (textBaseLeftX1, textBaseY1),
                           textBaseRight |> move (textBaseRightX1, textBaseY1),
                           textAddon |> move (textAddonX1, textAddonY1)
                         ]

    in  if String.isEmpty baseBin || (not <| addonOK addonStr)
        then empty
        else collage (collageW + 4) (collageH + 5) [ mainForm
                    |> move (toFloat -collageW / 2,
                             toFloat -collageH / 2) ]


splitBaseStr : String -> (String, String, String)
splitBaseStr str = case String.length str of
    12 -> ("",                    String.slice 0 6 str, String.slice 6 12 str)
    13 -> (String.slice 0 1 str , String.slice 1 7 str, String.slice 7 13 str)
    otherwise -> ("", "", "")

showText : Float -> String -> Form
showText textHeight str =
    let elem = str |> toText
                   |> monospace
                   |> Text.height textHeight
                   |> leftAligned
        (w, h) = sizeOf elem
    in elem |> toForm |> move (toFloat w / 2 - 1, toFloat h / 4)

displayBinary : Int -> Int -> Binary -> Form
displayBinary xSizeFactor h bin =
    let frms = map (showBinChar xSizeFactor h) <| String.toList bin
        w = String.length bin
        xs = [0 .. w] |> map (\x -> xSizeFactor * x) |> map toFloat
    in  zipWith (\f x -> moveX x f) frms xs |> group

showBinChar : Int -> Int -> Char -> Form
showBinChar w h c =
    let col = case c of
        '1' -> black
        otherwise -> white
    in  rect (toFloat w) (toFloat h) |> filled col |>
            move (toFloat w / 2, toFloat h / 2)