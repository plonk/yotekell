-- エントリーポイント
import Text.JSON
import Text.JSON.Generic
import System.IO
import System.Random

import GameState
import Move
import AI

main =
  -- 標準入出力のブロックバッファリングを切る。
  do hSetBuffering stdin LineBuffering
     hSetBuffering stdout LineBuffering
     initGen <- newStdGen
   
     putStrLn "ヨテケル"
     idStr <- getLine
   
     iter (read idStr :: Int) initGen

iter myID gen =
  do eof <- hIsEOF stdin
     if eof then
       return ()
     else do
       jsonStr <- getLine
       let state = decodeJSON jsonStr :: GameState
       do hPutStrLn stderr $ show state
          (move, gen') <- decideMove myID state gen
          print move
          iter myID gen'
