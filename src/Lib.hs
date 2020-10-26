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
import Data.Map as M
import Control.Concurrent.MVar
import Network.SockAddr (showSockAddr)
import Debug.Trace

-- TODO: afficher nom devant les messages recu, permettre de se connecter à un autre server (partie client), gérer déconnexion d'un client (viré du state)

data Interlocutor = Interlocutor Handle String

getHandle (Interlocutor handle _) = handle
getServerAddress (Interlocutor _ serverAddress) = serverAddress

type State = Map String Interlocutor

getInterlocutors :: State -> [Interlocutor]
getInterlocutors state = Data.List.map snd (M.toList state)

getServerAddresses :: State -> [String]
getServerAddresses state = Data.List.map getServerAddress $ getInterlocutors state

getHandles :: State -> [Handle]
getHandles state = Data.List.map getHandle $ Data.List.map snd (M.toList state)

listenLoop :: MVar State -> String -> Handle -> IO ()
listenLoop mvar address h = do
    line <- hGetLine h
    state <- takeMVar mvar
    putStrLn line
    let result = parseLine state address line
    let newState = fst result
    -- je le laisse pour la posterité en commentaire pour l'instant, le traceShow
    -- let answer = traceShow result (snd result)
    let answer = snd result
    putMVar mvar newState
    case answer of
      Just value -> hPutStrLn h value
      Nothing -> return ()
    listenLoop mvar address h

speak :: MVar State -> IO ()
speak mvar = do
  msg <- hGetLine stdin
  state <- takeMVar mvar
  -- est-ce qu'on est sur de devoir le remettre dedans ?
  putMVar mvar state
  let handles = getHandles state
  mapM (\handle -> hPutStrLn handle msg) handles
  speak mvar


runMDR :: IO ()
runMDR = do
  state <- newMVar M.empty
  runTCPServer Nothing "3000" listenLoop state

runTCPServer :: Maybe HostName -> ServiceName -> (MVar State -> String -> Handle -> IO a) -> MVar State -> IO a
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
	forkIO $ speak mvar
	return sock
    loop sock = forever $ do
	(conn, _peer) <- accept sock
	handle <- socketToHandle conn ReadWriteMode
	hSetBuffering handle LineBuffering
	state <- takeMVar mvar
	let address = show _peer
	putMVar mvar $ M.insert address (Interlocutor handle "") state
	void $ forkFinally (server mvar address handle) (const $ gracefulClose conn 5000)

parseLine :: State -> String -> String -> (State, Maybe String)
parseLine state address line =
  let
    command = getCommand line
  in
  case command of
    "KIKOO" -> handleKikoo state address line
    "TAVU" -> handleTavu state line
    "WTF" -> (state, Nothing)
    "LOL" -> (state, Nothing)
    _ -> (state, Just "WTF \"Talk my language u foreigner\"")

getCommand :: String -> String
getCommand line = head (words line)

handleKikoo :: State -> String -> String -> (State, Maybe String)
handleKikoo state address line =
  let
      newParticipantIp = (words line)!!2
      participantIps = Data.List.foldl (\acc participant -> acc ++ " / " ++ participant) "" $ getServerAddresses state
      newState = M.update (\x -> Just (Interlocutor (getHandle (state ! address)) newParticipantIp)) address state
  in
   (newState, Just $ "OKLM \"SuckMyLambdaCalculus\" 172.16.29.73:3000" ++ participantIps)

handleTavu :: State -> String -> (State, Maybe String)
handleTavu state line = (state, Just "LOL")
