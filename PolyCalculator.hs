{-# LANGUAGE CPP #-}

module PolyCalculator (repl, evaluate) where

#ifdef GRADESCOPE
import           AutograderUtils (getChar, getLine, print, putStrLn, readLn)
import           Prelude         hiding (getChar, getLine, print, putStrLn, readLn)
#endif
import           Parser          (Polynomial (..), expand, parse, polynomial, simplify)

parseFailed, polynomialLoaded, noPolynomialLoaded, polynomialSaved, noPolynomialMemorised, unbearable :: String
parseFailed = "Could not parse the polynomial."
polynomialLoaded = "Polynomial loaded."
noPolynomialLoaded = "No polynomial loaded."
polynomialSaved = "Polynomial saved."
noPolynomialMemorised = "No polynomial memorised."
unbearable = "Unbearable action!"

repl :: IO ()
repl = replLoop Nothing Nothing

replLoop :: Maybe Polynomial -> Maybe Polynomial -> IO ()
replLoop current saved = do
    input <- getLine
    let command = words input
    case command of
        ("load":xs) -> do
            let polyStr = unwords xs
            case parse polynomial polyStr of
                Nothing -> putStrLn parseFailed >> replLoop current saved
                Just (poly, "") -> do
                    putStrLn polynomialLoaded
                    replLoop (Just poly) saved
                Just (_, _) -> putStrLn parseFailed >> replLoop current saved
        ("evaluate":n:[]) -> do
            let num = read n :: Integer
            case current of
                Nothing -> putStrLn noPolynomialLoaded
                Just poly -> print $ evaluate poly num
            replLoop current saved
        ("memorise":[]) -> do
            case current of
                Nothing -> putStrLn noPolynomialLoaded >> replLoop current saved
                Just poly -> do
                    putStrLn polynomialSaved
                    replLoop current (Just poly)
        ("recall":[]) -> do
            case saved of
                Nothing -> putStrLn noPolynomialMemorised
                Just poly -> print poly
            replLoop current saved
        ("clear":[]) -> replLoop current Nothing
        ("reset":[]) -> replLoop Nothing Nothing
        ("expand":[]) -> do
            case current of
                Nothing -> putStrLn noPolynomialLoaded
                Just poly -> print $ expand poly
            replLoop current saved
        ("simplify":[]) -> do
            case current of
                Nothing -> putStrLn noPolynomialLoaded
                Just poly -> print $ simplify poly
            replLoop current saved
        ("quit":[]) -> return ()
        _ -> do
            putStrLn unbearable
            replLoop current saved

evaluate :: Polynomial -> Integer -> Integer
evaluate (Mono c d) x = c * (x ^ d)
evaluate (Add p1 p2) x = evaluate p1 x + evaluate p2 x
evaluate (Mul p1 p2) x = evaluate p1 x * evaluate p2 x
