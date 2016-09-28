{-# LANGUAGE ViewPatterns #-}
import Data.List
import qualified Data.ByteString.Char8 as BS
import System.Environment
import System.IO
import Graph

run :: Handle -> IO ()
run handle = do
    input <- map (parseLine . BS.words) . BS.lines <$> BS.hGetContents handle
    print . sort . reachableFrom 1 $ readGraph input

parseLine :: [BS.ByteString] -> (Int, [Int])
parseLine [] = error "Empty line"
parseLine (x:xs) = (readEx x, readEx <$> xs)

readEx :: BS.ByteString -> Int
readEx (BS.readInt -> Just (v, rest)) | BS.null rest = v
readEx str = error $ "Illegal number: " ++ BS.unpack str

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> run stdin
        [filePath] -> withFile filePath ReadMode run
        _ -> fail "Invalid number of arguments"

