module Lib
    ( runMDR,
      parseLine,
      getCommand,
    ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.UTF8
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO
import Data.List
import Control.Concurrent.MVar

talk :: MVar String -> Handle -> IO ()
talk mvar h = do
    line <- hGetLine h
    test <- takeMVar mvar 
    putStrLn line
    putStrLn test
    putMVar mvar "Sandun"
    hPutStr h (parseLine [] line)
    talk mvar h 

runMDR :: IO ()
runMDR = do
  mvar <- newMVar "test"
  runTCPServer Nothing "3000" talk mvar

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (MVar String -> Handle -> IO a) -> MVar String -> IO a
runTCPServer mhost port server mvar = 
  withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
	(conn, _peer) <- accept sock
	handle <- socketToHandle conn ReadWriteMode
	void $ forkFinally (server mvar handle) (const $ gracefulClose conn 5000)

parseLine :: [String] -> String -> String
parseLine participants line =
  let command = getCommand line in
  case command of 
    "KIKOO" -> handleKikoo participants line
    "TAVU" -> handleTavu line
    _ -> "ERR \"Talk my language u foreigner\""
      

getCommand :: String -> String
getCommand line = head (words line)

-- TODO: not hard coded address and port
handleKikoo :: [String] -> String -> String
handleKikoo participants line = 
  let participantIps = Data.List.foldl (\acc participant -> acc ++ " / " ++ participant) "" participants in 
  "OKLM \"SuckMyLambdaCalculus\" / 127.0.0.1:3000" ++ participantIps

handleTavu :: String -> String
handleTavu line = "ACK"
