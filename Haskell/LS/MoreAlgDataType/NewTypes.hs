-- ./MoreAlgDataType/NewTypes.hs
module MoreAlgDataType.NewTypes where

type String = [Char]

newtype Celsius    = Celsius    Float
newtype Fahrenheit = Fahrenheit Float
-- data Celsius    = Celsius    Float
-- data Fahrenheit = Fahrenheit Float