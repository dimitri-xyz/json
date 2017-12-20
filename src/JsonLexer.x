{
module JsonLexer (lexConsole, alexScanTokens, Token(..)) where
}

%wrapper "basic"

-- What follows is coded exactly to the JSON spec as of Oct 2013

  ------------------------------------

$whites = [\t\n\r\ ]    -- according to JSON spec. This is more restrictive than
                        -- the $white class that comes predefined in Alex.

$digit    = 0-9         -- digits
$nonzero  = 1-9
$alpha    = [a-zA-Z]    -- alphabetic characters
$hexa     = [0-9A-Fa-f] -- hexadecimal digits

$control       =  [\x00-\x1F]       -- control characters
$noncontrol    =  [^\x00-\x1F]

$nonescaped    =  [\x20-\x10ffff] # [\\ \"] -- is not a backslash, quotes or "control char"
                                            -- Somehow Alex doesn't allow me to use negation here
                                            -- I get an error if I define this set as:
                                            -- = [^\x00-\x1F] # [\\\"] or other variants

  ------------------------------------

tokens :-

  $whites+              ;

  ","                   { \s -> TokenComma }
  ":"                   { \s -> TokenColon}

  "["                   { \s -> TokenOpenBracket}
  "]"                   { \s -> TokenCloseBracket}

  "{"                   { \s -> TokenOpenBrace}
  "}"                   { \s -> TokenCloseBrace}

  "true"                { \s -> TokenJTrue}
  "false"               { \s -> TokenJFalse}
  "null"                { \s -> TokenJNull}

  ------------------------------------
  -- a string is:

  \"                        --begins with a double quotes, followed by
  (                         -- any number of:
     $nonescaped        |   -- anything that is NOT: a backslash, quotes or "control char"
     \\ \"              |   -- or a backslash followed by quotes
     \\ \\              |   -- or a backslash followed by \
     \\ \/              |   -- or a backslash followed by /
     \\ \b              |   -- or a backslash followed by b (backspace)
     \\ \f              |   -- or a backslash followed by f (form feed)
     \\ \n              |   -- or a backslash followed by n (new line)
     \\ \r              |   -- or a backslash followed by r (carriage return)
     \\ \t              |   -- or a backslash followed by t (tab)
     \\ u ($hexa{4})        -- backslash u followed by 4 hexadecimal digits
  )*
  \"                --ending with another double quotes

                                    { \s -> TokenString s}

  ------------------------------------
  -- a double (number) is:
   \-?                            -- optional minus sign
   ( 0 | $nonzero $digit* )       -- (no leading zeros) either a zero or a non-zero followed by digits
   ( \. $digit+ )?                -- fractional part (optional dot plus digits)
   ( [eE] [\+\-]? $digit+ )?      -- exponent (optional)

                              { \s -> TokenDouble (read s) }  -- a double in exponential notation
                                                              -- FIX ME! This will fail to read large (e.g. 60 bit) Integers
                                                              -- and for all integers x: 2^53 < x < 2^64
                                                              -- because doubles only have 53 bits of mantissa.
                                                              -- for these it would be best to use Word64
  ------------------------------------

  -- anything else throw an error
  .                     { \s -> error ("Lexer Error:" ++ s) }  -- FIX ME!! I probably don't want to cause the program to abort here.


  ------------------------------------
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
      TokenComma         |
      TokenColon         |

      TokenOpenBracket   |
      TokenCloseBracket  |

      TokenOpenBrace     |
      TokenCloseBrace    |

      TokenJTrue         |
      TokenJFalse        |
      TokenJNull         |

      TokenString [Char] |
      TokenDouble Double
      deriving (Eq , Show)

  ------------------------------------

lexConsole = do
  s <- getContents
  print (alexScanTokens s)

}
