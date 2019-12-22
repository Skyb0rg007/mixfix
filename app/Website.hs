{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Control.Lens                          ((&), (.~))
import           Control.Monad.Reader                  (runReader)
import           Data.Default                          (def)
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             (defaultLayoutOptions,
                                                        layoutPretty, pretty)
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Reflex.Dom
import           Text.Megaparsec                       (eof, errorBundlePretty,
                                                        runParserT)

import           Mixfix.Parse
import           Mixfix.Syntax

main :: IO ()
main = mainWidget app

app :: (PostBuild t m, DomBuilder t m, MonadHold t m) => m ()
app = do
    el "title" $ text "Mixfix Operator parser"
    let initial = "let assocr 0 if_then_else_ in\nif true then 1 else 0"
    txt <- textAreaElement $ def & initialAttributes .~ ("placeholder" =: "Enter code here")
                                 & textAreaElementConfig_initialValue .~ initial
    (btn, ()) <- elAttr' "button" mempty $ text "Submit"
    res <- holdDyn initial (current (_textAreaElement_value txt) `tag` domEvent Click btn)
    el "p" $ dynText (transform <$> res)
    instructions
    pure ()

transform :: Text -> Text
transform t =
    case runParserT (sc *> parseExp <* eof) "" t `runReader` mempty of
      Left err -> "ERROR: " <> Text.pack (errorBundlePretty err)
      Right ast -> renderStrict . layoutPretty defaultLayoutOptions $ pretty ast

instructions :: (DomBuilder t m) => m ()
instructions =
    el "p" $ elAttr "textarea" ("readonly" =: "true") $ do
        text "Syntax: \n"
        text "<expression> ::= <let> | <letassoc> | <expression> <expression> | <variable> | <literal>\n"
        text "<let> ::= let <variable> = <expression> in <expression>\n"
        text "<letassoc> ::= let <assoc> <num> <variable> in <expression>\n"
        text "<variable> ::= [a-zA-Z_][a-zA-Z0-9_]* | [=+*<>$%^&[]!|-:/_,]*\n"
        text "<literal> ::= [0-9]+ | \"...\" | 'c' | [0-9]+.[0-9]+\n"
        text "<assoc> ::= assocr | assocl | nonassoc\n"
