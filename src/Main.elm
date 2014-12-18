-- todo = [ split into modules,
--          docstrings,
--          type safety,
--          single canvas for saving: https://groups.google.com/forum/?fromgroups#!topic/elm-discuss/U7KTUBSY904 ]

module BarcodeGenerator where

import Debug
import Dict
import Maybe(withDefault, Maybe)
import Maybe
import Regex
import Result
import String
import Text
import List
import List((::))
import Text(plainText)
import Transform2D
import Graphics.Input (dropDown, checkbox)
import Graphics.Element (Element, spacer, flow, down, right, container, midTop
  , color, middle, empty, image)
import Graphics.Collage (rect, filled, move, moveX, moveY, group, toForm
  , collage, Form)
import Color (lightGrey, black, white)
import Signal
import Signal(Signal, (<~), (~))
import Graphics.Input.Field as Field
import Window

main : Signal Element
main = scene <~ Window.width
              ~ Signal.subscribe baseContent
              ~ Signal.subscribe addonContent
              ~ Signal.subscribe sizeContent
              ~ Signal.subscribe fontContent
              ~ Signal.subscribe guardExtensionsCheck
              ~ Signal.subscribe addonFullCheck
              ~ Signal.subscribe lightMarginIndicatorCheck

scene : Int -> Field.Content -> Field.Content ->
        Int -> String -> Bool -> Bool -> Bool -> Element
scene winW baseContentSig addonContentSig
        sizeFactor font guardExtensions addonFull lightMarginIndicator =
    let w = 460
        base = baseContentSig.string
        addon = addonContentSig.string
        defSpacer = spacer w 20
        showEdit h = Field.field Field.defaultStyle h
    in  flow down [
            defSpacer,
            Text.fromString "EAN/UPC-A Barcode Generator (+ addon2/addon5)"
                |> Text.bold
                |> Text.leftAligned
                |> container winW 20 midTop,
            flow down [
                defSpacer,
                showEdit (Signal.send baseContent)
                    "base code: 11 or 12 digits"
                    baseContentSig,
                showEdit (Signal.send addonContent)
                    "addon: 0, 2 or 5 digits"
                    addonContentSig,
                defSpacer,
                flow right [
                    plainText "Size: ",
                    dropDown (Signal.send sizeContent) sizeOptions
                ],
                defSpacer,
                flow right [
                    plainText "Font: ",
                    dropDown (Signal.send fontContent) fontOptions
                ],
                defSpacer,
                showGuardExtensionsCheck guardExtensions,
                showAddonFullCheck addonFull,
                showLightMarginIndicatorCheck lightMarginIndicator
            ] |> container winW 250 midTop,
            defSpacer,
            flow down [
                defSpacer,
                flow right [
                    displayBarcode sizeFactor guardExtensions addonFull
                                       lightMarginIndicator font base addon
                ]
            ] |> container winW 650 midTop |> color lightGrey
        ]

showGuardExtensionsCheck : Bool -> Element
showGuardExtensionsCheck guardExtensions =
    flow right [
        checkbox (Signal.send guardExtensionsCheck) guardExtensions
            |> container 30 30 middle,
        plainText "guard extensions"
            |> container 153 30 middle
    ]

showAddonFullCheck : Bool -> Element
showAddonFullCheck addonFull = flow right [
        checkbox (Signal.send addonFullCheck) addonFull
            |> container 30 30 middle,
        plainText "full height addon"
            |> container 150 30 middle
    ]

showLightMarginIndicatorCheck : Bool -> Element
showLightMarginIndicatorCheck lightMarginIndicator = flow right [
        checkbox (Signal.send lightMarginIndicatorCheck) lightMarginIndicator
            |> container 30 30 middle,
        plainText "light margin indicator"
            |> container 180 30 middle
    ]



baseContent : Signal.Channel Field.Content
baseContent = Signal.channel Field.noContent

addonContent : Signal.Channel Field.Content
addonContent = Signal.channel Field.noContent

sizeContent : Signal.Channel (Int)
sizeContent = Signal.channel 4

fontContent : Signal.Channel (String)
fontContent = Signal.channel "OCR-B"

guardExtensionsCheck : Signal.Channel Bool
guardExtensionsCheck = Signal.channel True

addonFullCheck : Signal.Channel Bool
addonFullCheck = Signal.channel True

lightMarginIndicatorCheck : Signal.Channel Bool
lightMarginIndicatorCheck = Signal.channel True

sizeOptions : List (String, Int)
sizeOptions =
    [ ("normal", 4),
      ("smallest", 1),
      ("small", 2),
      ("large", 8)
    ]

fontOptions : List (String, String)
fontOptions =
    [ ("OCR-B", "OCR-B"),
      ("OCR-A", "OCR-A")
    ]

type alias Binary = String

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

firstDigitToParities : Dict.Dict Char (List Char)
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
    ('9', "LGGLGL") ] |> Dict.fromList |> dictMapValues String.toList

dictMapValues : (v -> w) -> Dict.Dict comparable v -> Dict.Dict comparable w
dictMapValues f d = Dict.map (\k v -> f v) d

upcDigitsToBinR : Dict.Dict Char Binary
upcDigitsToBinR = dictMapValues invertBinaryStr upcDigitsToBinL

upcDigitsToBinG : Dict.Dict Char Binary
upcDigitsToBinG = dictMapValues String.reverse upcDigitsToBinR

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

{-| Input must have length 13.
    http://en.wikipedia.org/wiki/International_Article_Number_%28EAN%29 -}
generateEAN13 : String -> Binary
generateEAN13 str =
    let startGuard = "101"
        middleGuard = "01010"
        endGuard = "101"
        front = String.left 7 str |> generateEAN13Front
        back = String.right 6 str |> generateEAN13Back
    in  startGuard ++ front ++ middleGuard ++ back ++ endGuard

getOrFail : comparable -> Dict.Dict comparable v -> v
getOrFail key dict =
  case Dict.get key dict of
    Maybe.Just res -> res

generateEAN13Front : String -> Binary
generateEAN13Front str =
    let (first, rest) = case String.uncons str of
                            Maybe.Just p -> p
                            Maybe.Nothing -> ('0', "")
        parities = getOrFail first firstDigitToParities
        charDicts = List.map (flip getOrFail parityToDigitsToBin) parities
        chars = String.toList rest
        binaries = List.map2 getOrFail chars charDicts
    in  String.concat binaries

generateEAN13Back : String -> Binary
generateEAN13Back str =
    let chars = String.toList str
        binaries = List.map2 getOrFail chars <| List.repeat 6 upcDigitsToBinR
    in  String.concat binaries

{-| Input must have length 12. -}
calcCheckDigit : String -> String
calcCheckDigit str =
    let vals = str |> String.reverse |> stringToDigitValues
        s = calcCheckSum (3, 1) vals
    in  if List.length vals == 12
        then 10 - s `rem` 10 |> \x -> x `rem` 10 |> toString
        else ""

stringToDigitValues = String.toList >>
    List.filterMap ((\x -> [x]) >>
        String.fromList >>
        String.toInt
        >> Result.toMaybe)

generateAddon : String -> Binary
generateAddon str = case String.length str of
    0 -> ""
    2 -> generateAddon2 str
    5 -> generateAddon5 str

fromMaybe : a -> Maybe a -> a
fromMaybe def m = case m of
    Maybe.Just x -> x
    Maybe.Nothing -> def

last : List a -> a
last = List.reverse >> List.head

{-| http://en.wikipedia.org/wiki/EAN_2 -}
generateAddon2 : String -> Binary
generateAddon2 str =
    let value = String.toInt str |> Result.toMaybe |> fromMaybe 0
        chars = String.toList str
        startGuard = "01011"
        middleGuard = "01"
        parities = addon2Parities value
        digitsToBins = List.map (flip getOrFail parityToDigitsToBin) parities
        binaries = List.map2 getOrFail chars digitsToBins
        front = List.head binaries
        back = last binaries
    in  startGuard ++ front ++ middleGuard ++ back

addon2Parities : Int -> List Char
addon2Parities checksum = case checksum `rem` 4 of
    0 -> ['L', 'L']
    1 -> ['L', 'G']
    2 -> ['G', 'L']
    3 -> ['G', 'G']

{-| http://en.wikipedia.org/wiki/EAN_2 -}
generateAddon5 : String -> Binary
generateAddon5 str =
    let startGuard = "01011"
        separator = "01"
        digitValues = stringToDigitValues str
        parities = addon5Parities <| calcCheckSum (3, 9) digitValues
        charDicts = List.map (flip getOrFail parityToDigitsToBin) parities
        chars = String.toList str
        binaries = List.map2 getOrFail chars charDicts
    in startGuard ++ (List.intersperse separator binaries |> String.concat)

addon5Parities : Int -> List Char
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

calcCheckSum : (Int, Int) -> List Int -> Int
calcCheckSum (m1, m2) xs =
    let xs' = if List.length xs `rem` 2 == 0 then xs else xs ++ [0]
        f (a, b) = m1 * a + m2 * b
    in  xs' |> nonOverlappingPairs |> List.map f |> List.sum

{-| nonOverlappingPairs [1,2,3,4,5] === [(1,2),(3,4)] -}
nonOverlappingPairs : List a -> List (a,a)
nonOverlappingPairs l = case l of
    (x1::x2::xs) -> (x1,x2) :: nonOverlappingPairs xs
    _ -> []

displayBarcode : Int -> Bool -> Bool -> Bool ->
                 String -> String -> String -> Element
displayBarcode xSizeFactor guardExtensions addonFull lightMarginIndicators
               font baseStr addonStr =
    let (baseBin, addonBin) = generateBarcode baseStr addonStr
        base = displayBinary xSizeFactor baseH baseBin
        addon = displayBinary xSizeFactor addonH addonBin
        textBaseDistY = 3 * xSizeFactor
        addonDistX = 14 * xSizeFactor
        addonTextDistX = if String.length addonStr == 2
            then 10 * xSizeFactor
            else 19 * xSizeFactor

        textHeight = 7 * xSizeFactor

        baseX1 = 10 * xSizeFactor
        baseY1 = textHeight + textBaseDistY
        baseW = String.length baseBin * xSizeFactor |> toFloat
        baseX2 = baseX1 + baseW
        baseH = 66 * xSizeFactor
        baseY2 = baseY1 + baseH

        guard1X1 = baseX1
        guard1Y1 = textHeight / 2
        guard1Y2 = baseY1
        guardH = guard1Y2 - guard1Y1

        textBaseY1 = 0 * xSizeFactor
        textBaseY2 = textHeight
        textBaseYC = (textBaseY2 - textBaseY1) / 2

        textBaseLeftSingleX1 = 0 * xSizeFactor
        textBaseRightSingleX1 = baseX2 + 3 * xSizeFactor

        textBaseLeftX1 = guard2X2 + 3 * xSizeFactor
        --textBaseLeftX2 = guard3X1 - 3

        textBaseRightX1 = guard4X2 + 3 * xSizeFactor
        --textBaseRightX2 = guard5X1 - 3

        guard2X1 = guard1X1 + 2 * xSizeFactor
        guard3X1 = guard1X1 + 46 * xSizeFactor
        guard4X1 = guard3X1 + 2 * xSizeFactor
        guard5X1 = guard3X1 + 46 * xSizeFactor
        guard6X1 = guard5X1 + 2 * xSizeFactor

        guard1X2 = guard1X1 + 1 * xSizeFactor
        guard2X2 = guard2X1 + 1 * xSizeFactor
        guard3X2 = guard3X1 + 1 * xSizeFactor
        guard4X2 = guard4X1 + 1 * xSizeFactor
        guard5X2 = guard5X1 + 1 * xSizeFactor
        guard6X2 = guard6X1 + 1 * xSizeFactor

        guard = rect xSizeFactor guardH
            |> filled black
            |> move (0.5 * xSizeFactor, guardH / 2 |> ceiling |> toFloat)
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
        addonW = String.length addonBin * xSizeFactor |> toFloat
        addonX2 = addonX1 + addonW
        addonY2 = if addonFull
            then baseY2
            else baseY2 - (1 * xSizeFactor + textHeight + 3 * xSizeFactor)
        addonH = addonY2 - addonY1

        textAddonX1 = addonX1 + addonTextDistX
        textAddonY1 = if addonFull
            then textBaseY1
            else addonY2 + 3 * xSizeFactor
        textAddonX2 = addonX2 - 1 * xSizeFactor
        textAddonYC = textAddonY1 + textHeight / 2

        addRightBaseLightMarginIndicator =
            lightMarginIndicators && String.isEmpty addonBin

        (strBaseLeftSingle, strBaseLeft, strBaseRight, strBaseRightSingle) =
            baseInputToBarcodeString False baseStr
            |> splitBaseStr addRightBaseLightMarginIndicator
        textBaseLeftSingle = showText font textHeight strBaseLeftSingle
        textBaseLeft = showText font textHeight strBaseLeft
        textBaseRight = showText font textHeight strBaseRight
        textBaseRightSingle = showText font textHeight strBaseRightSingle
        textAddon = (addonStr ++ if lightMarginIndicators && (not <| String.isEmpty addonBin) then " >" else "")
                        |> showText font textHeight

        border = 2 * xSizeFactor
        collageW = addonX2 + border + 13 * xSizeFactor |> ceiling
        collageH = baseY2 + border |> ceiling

        baseTextLeftOffsetX = if String.length strBaseLeft == 5
                              then 3 * xSizeFactor else 0
        baseTextRightOffsetX = if String.length strBaseRight == 5
                               then 3 * xSizeFactor else 0

        mainFormRaw = group
            [ base |> move (baseX1, baseY1),
              if guardExtensions then guards else empty |> toForm,
              addon |> move (addonX1, addonY1),
              textBaseLeftSingle |> move (textBaseLeftSingleX1, textBaseYC),
              textBaseLeft
                  |> move (textBaseLeftX1 + baseTextLeftOffsetX, textBaseYC),
              textBaseRight
                  |> move (textBaseRightX1 + baseTextRightOffsetX, textBaseYC),
              textBaseRightSingle |> move (textBaseRightSingleX1, textBaseYC),
              textAddon |> move (textAddonX1, textAddonYC)
            ]

        mainFormFinal = group [
            rect (toFloat collageW) (toFloat collageH) |> filled white,
                mainFormRaw |> move (toFloat -collageW / 2 + border / 2,
                                     toFloat -collageH / 2 + border / 2) ]

    in  if String.isEmpty baseBin || (not <| addonOK addonStr)
        then empty
        else collage collageW collageH [mainFormFinal]

splitBaseStr : Bool -> String -> (String, String, String, String)
splitBaseStr addRightLightMarginIndicator str =
    let slc start end = String.slice start end str
        right13 = if addRightLightMarginIndicator then ">" else ""
    in  case String.length str of
    12 -> (slc 0 1, slc 1 6, slc 6 11, slc 11 12)
    13 -> (slc 0 1 , slc 1 7, slc 7 13, right13)
    otherwise -> ("", "", "", "")

(digitImageWidth, digitImageHeight) = (140, 164)

showText : String -> Float -> String -> Form
showText font textHeight str =
    let frms = stringToListOfCharStrings str
               |> List.map (digitImage font scaleF)
        scaleF = textHeight / toFloat digitImageHeight
        distX = toFloat digitImageWidth * scaleF
    in  lineUp distX frms |> moveX (toFloat digitImageWidth * scaleF / 2)

lineUp : Float -> List Form -> Form
lineUp distX forms =
    let xs = [ 0 .. List.length forms ]
             |> List.map toFloat
             |> List.map (\x -> x * distX)
    in List.map2 (\frm x -> moveX x frm) forms xs |> group

stringToListOfCharStrings : String -> List String
stringToListOfCharStrings =
    String.toList >> List.map (\x -> String.cons x "")

digitImage : String -> Float -> String -> Form
digitImage font scaleFactor digit =
    let w = digitImageWidth |> toFloat |> \x -> x * scaleFactor |> round
        h = digitImageHeight |> toFloat |> \x -> x * scaleFactor |> round
    in  image w h (digitUrl font digit) |> toForm

digitUrl : String -> String -> String
digitUrl font digit =
    let fileName = case digit of
                       "<" -> "left"
                       ">" -> "right"
                       " " -> "space"
                       otherwise -> digit
    in  "fonts/" ++ font ++ "/" ++ fileName ++ ".png"

displayBinary : Int -> Int -> Binary -> Form
displayBinary xSizeFactor h bin =
    let frms = List.map (showBinChar xSizeFactor h) <| String.toList bin
        w = String.length bin
    in  lineUp (toFloat xSizeFactor) frms

showBinChar : Int -> Int -> Char -> Form
showBinChar w h c =
    let col = case c of
        '1' -> black
        otherwise -> white
    in  rect (toFloat w) (toFloat h) |> filled col |>
            move (toFloat w / 2, toFloat h / 2)