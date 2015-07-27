-- エントリーポイント
import Text.JSON
import Text.JSON.Generic
import System.IO

import GameState
import Move

main = do
  putStrLn "ヨテケル"
  idStr <- getLine
  iter

iter = do
  eof <- hIsEOF stdin
  if eof then
    return ()
  else do
    jsonStr <- getLine
    hPutStrLn stderr $ show (decodeJSON jsonStr :: GameState)
    putStrLn $ show $ Move Stay False ""
    iter
