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

runMDR :: IO ()
runMDR = runTCPServer Nothing "3000" talk
  where
    talk h = do
	line <- hGetLine h
	putStrLn line
	hPutStr h (parseLine [] line)
	talk h 

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Handle -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
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
        void $ forkFinally (server handle) (const $ gracefulClose conn 5000)


parseLine :: [a] -> String -> String
parseLine participants line =
  let command = getCommand line in
  case command of 
    "KIKOO" -> handleKikoo participants line
    "TAVU" -> handleTavu line
    _ -> "ERR \"Talk my language u foreigner\""
      

getCommand :: String -> String
getCommand line = head (words line)

-- TODO: not hard coded address and port
handleKikoo :: [a] -> String -> String
handleKikoo participants line = "OKLM \"SuckMyLambdaCalculus\" / 127.0.0.1:3000"

handleTavu :: String -> String
handleTavu line = "ACK"
