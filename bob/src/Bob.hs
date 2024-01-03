module Bob (responseFor) where
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char

isYelling :: String -> Bool
isYelling text 
  | filter isLetter text == "" = False
  | otherwise                  = all isUpper (filter isLetter text)

isQuestion :: Text -> Bool
isQuestion text = T.isSuffixOf (T.pack "?") (T.pack . removeSpaces $ T.unpack text)
  where removeSpaces = filter (\xs -> (xs /=' '))

responseFor :: Text -> Text
responseFor text
  | (isQuestion text) && (isYelling (T.unpack text)) = T.pack "Calm down, I know what I'm doing!" 
  | isQuestion text                                  = T.pack "Sure."
  | isYelling (T.unpack text)                        = T.pack "Whoa, chill out!"
  | all isSpace (T.unpack text)                      = T.pack "Fine. Be that way!"
  | otherwise                                        = T.pack "Whatever."
