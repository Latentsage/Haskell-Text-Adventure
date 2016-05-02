-- Main.hs, final code
module Main where
 
import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)
 
import Adventure
import Rooms
 
main :: IO ()
main = withSocketsDo $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  mainLoop sock
 
type Msg = (Int, String)
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn)
  mainLoop sock
 
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    
    -- do conn init here
 
    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ gameLoop (Game startingRoom []) hdl-- \loop -> \game -> do
        -- input <- liftM init (hGetLine hdl)
        -- hPutStrLn hdl (snd (handleInput game input))
        -- loop (fst (handleInput game input))
 
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Goodbye!"
             -- else, continue looping.
             _      -> loop
 
    killThread reader                      -- kill after the loop ends
    hClose hdl                             -- close the handle
    
gameLoop :: Game -> Handle -> IO()
gameLoop game hdl = do
    input <- liftM init (hGetLine hdl)
    hPutStrLn hdl (snd (handleInput game input))
    gameLoop (fst (handleInput game input)) hdl