module Lib
    ( runMDR,
      parseLine,
      getCommand,
    ) where

import Control.Concurrent (forkFinally, forkIO)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.UTF8
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO
import Data.List
import Control.Concurrent.MVar
import Network.SockAddr (showSockAddr)

data Interlocutor = Interlocutor Handle String

talk :: MVar [String] -> Handle -> IO ()
talk mvar h = do
    line <- hGetLine h
    participants <- takeMVar mvar 
    putStrLn line
    let result = parseLine participants line 
    let answer = snd result
    putMVar mvar (fst result)
    case answer of 
      Just value -> hPutStrLn h value
      Nothing -> return ()
    talk mvar h 

-- foutre les differents handle dans la mvar ? comme ca quand je speak j'ai plus qu'a envoyé un hPutStrLn à chacun
speak :: IO ()
speak = do
  x <- hGetLine stdin
  putStrLn ("speak" ++ x)
  speak


runMDR :: IO ()
runMDR = do
  mvar <- newMVar []
  runTCPServer Nothing "3000" talk mvar

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (MVar [String] -> Handle -> IO a) -> MVar [String] -> IO a
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
	forkIO speak 
	return sock
    loop sock = forever $ do
	(conn, _peer) <- accept sock
	handle <- socketToHandle conn ReadWriteMode
	hSetBuffering handle LineBuffering
	void $ forkFinally (server mvar handle) (const $ gracefulClose conn 5000)

parseLine :: [String] -> String -> ([String], Maybe String)
parseLine participants line =
  let command = getCommand line in
  case command of 
    "KIKOO" -> handleKikoo participants line
    "TAVU" -> handleTavu participants line
    "WTF" -> (participants, Nothing)
    "LOL" -> (participants, Nothing)
    _ -> (participants, Just "WTF \"Talk my language u foreigner\"")
      

getCommand :: String -> String
getCommand line = head (words line)

-- TODO: not hard coded address and port
handleKikoo :: [String] -> String -> ([String], Maybe String)
handleKikoo participants line =
  let 
      newParticipantIp = (words line)!!2
      participantIps = Data.List.foldl (\acc participant -> acc ++ " / " ++ participant) "" participants
  in 
   ((participants ++ [newParticipantIp]), Just $ "OKLM \"SuckMyLambdaCalculus\" 172.16.29.73:3000" ++ participantIps)


handleTavu :: [String] -> String -> ([String], Maybe String)
handleTavu participants line = (participants, Just "LOL")
