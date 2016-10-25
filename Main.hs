module Main where
import Network.Socket hiding (sendTo, recv, recvFrom)
import Network.Socket.ByteString (recv)
import qualified Data.ByteString.Char8 as B8
import Data.Char

client' :: Int -> IO ()
client' = client "localhost"

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
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                msgSender sock serverAddr

--msgSender :: Socket -> IO ()
msgSender sock  addr = do
  connect sock (addrAddress addr)
  putStrLn "Enter text:"
  input <- getLine
  let msg = "GET /index.php?message=" ++ input ++ " HTTP/1.1\r\nHost: localhost:8002\r\n\r\n"
  putStrLn msg
  send sock msg
  rMsg <- recv sock 4096
  B8.putStrLn rMsg
  if input == "q" then do
    sClose sock
    putStrLn "Disconnected!" 
    else 
      msgSender sock addr
