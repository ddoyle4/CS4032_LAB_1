module Main where
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Data.Char
import Data.String.Utils

main = do
  putStrLn "Enter host:"
  host <- getLine
  putStrLn "Enter port:"
  port <- getLine 
  let portInt = read port :: Int
  client host portInt

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfo
  session serverAddr
  putStrLn "finished"

session sa = do
  putStrLn "Enter text:"
  input <- getLine
  putStrLn input
  case input of
    "q" -> do 
      putStrLn "disconnected"
    _ -> do 
    putStrLn input
    getRequest sa (B8.pack (subStrRepl input " " "%20"))
    session sa

getRequest addr input = do
  sock <- socket (addrFamily addr) Stream defaultProtocol
  let startGetRequest = B8.pack "GET /index.php?message="
  let endGetRequest = B8.pack " HTTP/1.1\r\nHost: localhost:8002\r\n\r\n"
  let getRequest = B8.append startGetRequest (B8.append input endGetRequest)
  connect sock (addrAddress addr)
  send sock getRequest
  rMsg <- recv sock 8000
  B8.putStrLn rMsg
  sClose sock

subStrRepl :: String -> String -> String -> String
subStrRepl str find repl = join repl (split find str)
