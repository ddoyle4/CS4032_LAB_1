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
  putStrLn "Enter host:"
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
  session host serverAddr
  putStrLn "finished"


-- An interactive session which allows the user to send multiple GET requests to the server
session :: String -> AddrInfo -> IO ()
session host sa = do
  putStrLn "Enter text:"
  input <- getLine
  case input of
    "q" -> do 
      putStrLn "disconnected"
    _ -> do 
    getRequest host sa (B8.pack (subStrRepl input " " "%20"))
    session host sa

-- Creates a socket, sends the GET request, prints the response
getRequest :: String -> AddrInfo -> B8.ByteString -> IO ()
getRequest host addr input = do
  sock <- socket (addrFamily addr) Stream defaultProtocol
  putStrLn host
  let startGetRequest = B8.pack "GET /index.php?message="
  let endGetRequest = B8.pack (" HTTP/1.1\r\nHost: " ++ host ++ "\r\n\r\n")
  let getRequest = B8.append startGetRequest (B8.append input endGetRequest)
  connect sock (addrAddress addr)
  send sock getRequest
  rMsg <- recv sock 8192                                  --simple get requests won't require more than 8KB 
  let response = B8.unpack $ rMsg
  let h = Fields (head (split "\r\n\r\n" response)) (last (split "\r\n\r\n" response))  
  putStrLn ("\n----------Header---------- \n\n" ++ (getHead h) ++ "\n--------------------------\n")
  putStrLn ("\n-----------Body----------- \n\n" ++ (getBody h) ++ "\n--------------------------\n")
  sClose sock

-- Helper method used to clean strings to be passed in GET request, i.e. can replace " " with "%20" 
subStrRepl :: String -> String -> String -> String
subStrRepl str find repl = join repl (split find str)

-- Implementation of GET response data structure
data GETResponse = Fields String String

getHead :: GETResponse -> String
getHead (Fields h b) = h

getBody :: GETResponse -> String
getBody (Fields h b) = b

