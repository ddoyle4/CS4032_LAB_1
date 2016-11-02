{-
Author: David Doyle
Description: A simple client that will send GET requests to a server. 
It will start a 'session' where the user can send multiple GET requests 
to a specified server until the quit command, 'q', is entered.
Date: 2/11/16
-}

module Main where
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Data.Char
import Data.String.Utils


-- Collects desire host and port
main :: IO () 
main = do
  putStrLn "Enter host (e.g. localhost):"
  host <- getLine
  putStrLn "Enter port:"
  port <- getLine 
  let portInt = read port :: Int                          -- cast port as Int
  client host portInt

-- Establishes server address and begins an interactive session
client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfo
  session serverAddr
  putStrLn "finished"


-- An interactive session which allows the user to send multiple GET requests to the server
session :: AddrInfo -> IO ()
session sa = do
  putStrLn "Enter text:"
  input <- getLine
  case input of
    "q" -> do 
      putStrLn "disconnected"
    _ -> do 
    putStrLn input
    getRequest sa (B8.pack (subStrRepl input " " "%20"))
    session sa

-- Creates a socket, sends the GET request, prints the response
getRequest :: AddrInfo -> B8.ByteString -> IO ()
getRequest addr input = do
  sock <- socket (addrFamily addr) Stream defaultProtocol
  let startGetRequest = B8.pack "GET /index.php?message="
  let endGetRequest = B8.pack " HTTP/1.1\r\n\r\n"
  let getRequest = B8.append startGetRequest (B8.append input endGetRequest)
  connect sock (addrAddress addr)
  send sock getRequest
  rMsg <- recv sock 8000
  B8.putStrLn rMsg
  sClose sock

-- Helper method used to clean strings to be passed in GET request, i.e. can replace " " with "%20" 
subStrRepl :: String -> String -> String -> String
subStrRepl str find repl = join repl (split find str)

