module Internal.Parse (smoke) where

import Data.Maybe
import Data.Text (Text)

import Text.Regex.Bird
import Text.Regex.Bird.Text
import Text.Regex.Bird.Internal.Parse

smoke :: [(Text, Either String Regex, Regex)]
smoke = catMaybes $ run_parseSmoke <$> smokeTests_parse

run_parseSmoke :: (Text, Regex) -> Maybe (Text, Either String Regex, Regex)
run_parseSmoke (input, expected) = case parseRegex input of
    Left err -> Just (input, Left err, expected)
    Right out | out /= expected -> Just (input, Right out, expected)
              | otherwise -> Nothing

smokeTests_parse :: [(Text, Regex)]
smokeTests_parse =
    [ (".", Any)
    , ("", Empty)
    , ("a", Char 'a')
    , ("\\\\", Char '\\')
    , ("\\a", Char 'a')
    , ("\\.", Char '.')
    , ("\\&\\|", Str "&|")
    , ("\\^\\?\\*\\+", Str "^?*+")
    , ("\\(\\)\\[\\]\\{\\}", Str "()[]{}")

    , ("Hello, regex!", Str "Hello, regex!")
    , ("Hello, .!", Seq (Str "Hello, ") $ Seq Any (Str "!"))

    , ("(?0.zero)", Capture "0" (Str "zero"))
    , ("(?x.eks)", Capture "x" (Str "eks"))
    , ("(?00foo.Special Agent Double-O Foo)", Capture "00foo" (Str "Special Agent Double-O Foo"))
    , ("(=0)", Replay "0")
    , ("(=x)", Replay "x")
    , ("(=11)", Replay "11")
    , ("(=foo)", Replay "foo")

    , ("^a", Not (Char 'a'))
    , ("^(Goodbye)", Not (Str "Goodbye"))
    , ("^^a", Not (Not (Char 'a')))
    , ("^.", Not Any)
    , ("a*", Star (Char 'a'))
    , ("a?", Option (Char 'a'))
    , ("a+", Plus (Char 'a'))

    , ("a&b", And (Char 'a') (Char 'b'))
    , ("a.&.b\\\\", And (Seq (Char 'a') Any) (Seq Any (Str "b\\")))
    , ("a|b", Alt (Char 'a') (Char 'b'))
    , ("a.|.b\\\\", Alt (Seq (Char 'a') Any) (Seq Any (Str "b\\")))
    , ("a|b&c", Alt (Char 'a') (And (Char 'b') (Char 'c')))
    , ("a&b|c", Alt (And (Char 'a') (Char 'b')) (Char 'c'))

    , ("a(b)c", Str "abc")
    , ("a(hi)*", Seq (Char 'a') $ Star (Str "hi"))
    , ("(a|b)&c", And (Alt (Char 'a') (Char 'b')) (Char 'c'))
    ]
