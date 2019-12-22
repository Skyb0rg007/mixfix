{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader
import qualified Data.IntMap                           as IntMap
import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as TIO
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Megaparsec

import           Mixfix.Parse
import           Mixfix.Syntax

usage :: IO ()
usage = do
    p <- getProgName
    hPutStrLn stderr $ "Usage: " ++ p ++ " <filename>"

main :: IO ()
main =
    getArgs >>= \case
        [] -> usage >> exitFailure
        filename:_ -> do
            contents <- if filename == "-" then TIO.getContents else TIO.readFile filename
            let name = if filename == "-" then "<stdin>" else filename
            case runParserT (sc *> parseExp <* eof) name contents `runReader` IntMap.empty of
              Left err  -> putStrLn $ errorBundlePretty err
              Right ast -> putDoc (pretty ast) >> putStrLn ""

