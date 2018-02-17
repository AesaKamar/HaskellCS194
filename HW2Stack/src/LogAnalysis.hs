module LogAnalysis where

import           Data.Char
import           Data.List
import           Data.String
import           Log

import           Control.Applicative                  (many, (*>), (<$), (<$>),
                                                       (<*), (<*>), (<|>))
import           Control.Monad                        (ap, mzero, void)
import           Data.Char                            (isDigit, isLetter)
import           Data.Functor
import           Text.Parsec                          (ParseError, parse,
                                                       runParser, try)

import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String                   (Parser)
import           Text.ParserCombinators.Parsec.Number




integer :: Parser Int
integer = read <$> many1 digit



parseMessage :: String -> LogMessage
parseMessage input =
  case parsedLog of
    Right r -> r
    Left _  -> Unknown input
  where
    parsedLog = parse logParser "" input


parseAll :: String -> [LogMessage]
parseAll inp =  parseMessage <$> lines inp


logParser:: Parser LogMessage
logParser = do
  msgTyp <- infoP <|> warningP <|> errorP
  space
  tmStmp <- integer
  space
  rest <- many anyChar
  return (LogMessage msgTyp tmStmp rest)

  where
    infoP = char 'I' $> Info
    warningP = char 'W'  $> Warning
    errorP = do
      char 'E' >> space
      nums <- integer
      return (Error nums)



instance Ord LogMessage where
  compare (LogMessage _ ts1 _) (LogMessage _ ts2 _) =
    compare ts1 ts2
  compare _ _ = EQ



insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message tree     = tree
  -- case tree of
  -- Leaf -> Node
  -- Node t1 logM t2  -> expression



















 -- THE END
