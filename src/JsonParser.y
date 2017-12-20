{
{-# LANGUAGE DeriveGeneric #-}
module JsonParser (
  parseConsole,
  parse,
  Jvalue(..),
  getNumber,
  getString,
  getBool,
  jStringToNumber) where

import JsonLexer (alexScanTokens, Token(..))

import qualified Data.HashMap.Strict as Map

import Control.DeepSeq
import GHC.Generics (Generic)
}

%name calc
%tokentype { Token }
%error { parseError }


 %token
      ','             { TokenComma }
      ':'             { TokenColon }

      '['             { TokenOpenBracket }
      ']'             { TokenCloseBracket }

      '{'             { TokenOpenBrace }
      '}'             { TokenCloseBrace }

      true            { TokenJTrue }
      false           { TokenJFalse }
      null            { TokenJNull }

      string          { TokenString _ }
      number          { TokenDouble _ }


%%

-- non-Terminals in ALL CAPS

VALUE       : OBJECT                { $1 }
            | ARRAY                 { $1 }
            | number                { Jnumber (getNumFromToken $1) }
            | string                { Jstring (getStrFromToken $1) }
            | true                  { Jbool True  }
            | false                 { Jbool False }
            | null                  { Jnull }

OBJECT      : '{' '}'               { Jobject (Map.empty) }
            | '{' MEMBERS '}'       { $2 }

MEMBERS     : PAIR                  { Jobject (Map.singleton (fst $1) (snd $1)) }
            | PAIR ',' MEMBERS      { Jobject (Map.insert (fst $1) (snd $1) (getMap $3)) -- FIX ME! this simply overwrites previous entries!}

PAIR        : string ':' VALUE      { (getStrFromToken $1 , $3) }

ARRAY       : '[' ']'               { Jarray [] }
            | '[' ELEMENTS ']'      { $2 }

ELEMENTS    : VALUE                 { Jarray [$1]  }
            | VALUE ',' ELEMENTS    { Jarray ($1 : getList $3) }


-------------------------------------------


{
-- Next we can declare the data type that represents the parsed expression:

data Jvalue = Jobject   (Map.HashMap String Jvalue)
            | Jarray    [Jvalue]
            | Jstring   String           -- terminals
            | Jnumber   Double
            | Jbool     Bool
            | Jnull
            deriving (Show, Generic)

instance NFData Jvalue

--------------helper functions------------------
-- help for external clients

getString :: Jvalue -> Maybe String
getString (Jstring str) = return str
getString  _ = Nothing

getNumber :: Jvalue -> Maybe Double
getNumber (Jnumber num) = return num
getNumber  _ = Nothing

getBool :: Jvalue -> Maybe Bool
getBool (Jbool flag) = return flag
getBool _ = Nothing

jStringToNumber :: Jvalue -> Maybe Double
jStringToNumber (Jstring s) = Just (read s)
jStringToNumber _           = Nothing
------------------------------------------------

-- data accessors
getNumFromToken :: Token -> Double
getNumFromToken (TokenDouble d) = d
getNumFromToken _ = error "Pattern Match failed on getting double from token that is not a TokenDouble"

getStrFromToken :: Token -> String
getStrFromToken (TokenString s) = init $ tail s   -- remove the sorrounding quotes.
getStrFromToken _ = error "Pattern Match failed on getting string from token that is not a TokenString"

getList :: Jvalue -> [Jvalue]
getList (Jarray xs) = xs
getList _ = error "Pattern Match failed on getting array from Jvalue that is not a Jarray"

getMap :: Jvalue -> Map.HashMap String Jvalue
getMap (Jobject themap) = themap
getMap _ = error "Pattern Match failed on getting 'hashmap' from Jvalue that is not a Jobject"


-- FIX ME! This error reporting is insufficient.
parseError :: [Token] -> a
parseError xs = error ("Parse error: " ++ show xs)

-- And finally a top-level function to take some input, parse it, and print out the result.
parseConsole = getContents >>= print . calc . alexScanTokens

parse :: String -> Jvalue
parse = calc . alexScanTokens

}
