module System.Env.Load.Internal
    ( Variable
    , readVariable
    ) where

type Variable = (String, String)

readVariable :: String -> Maybe Variable
readVariable [] = Nothing
readVariable ('#':_) = Nothing
readVariable str = Just $
    let (x,y) = break (== '=') str
    in  (x, dequote $ drop 1 y)

dequote :: String -> String
dequote [] = []
dequote str@(x:xs)
    | x `elem` "'\"" = reverse $ drop 1 $ reverse xs
    | otherwise = str
