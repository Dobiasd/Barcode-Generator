module BarcodeGenerator where

import Dict
import String

main = flow down [
    plainText "Barcode Generator",
    asText upcBinToDigitsL,
    asText upcBinToDigitsR,
    asText upcBinToDigitsG ]

upcBinToDigitsL : Dict.Dict Char String
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

upcBinToDigitsR : Dict.Dict Char String
upcBinToDigitsR = Dict.map invertBinaryStr upcBinToDigitsL

upcBinToDigitsG : Dict.Dict Char String
upcBinToDigitsG = Dict.map String.reverse upcBinToDigitsL

upcCodeToBinToDigits : Dict.Dict Char (Dict.Dict Char String)
upcCodeToBinToDigits = [
    ('L', upcBinToDigitsL),
    ('R', upcBinToDigitsR),
    ('G', upcBinToDigitsG) ] |> Dict.fromList

invertBinaryStr : String -> String
invertBinaryStr = String.map invertBinaryChar

invertBinaryChar : Char -> Char
invertBinaryChar c = case c of
    '0' -> '1'
    '1' -> '0'
    otherwise -> '-'