module Hotest.Datagram (
    Datagram (cmd, msg),
    datagram,
    readDatagram,
    writeDatagram,
    completeMsg
)
where

import Data.ByteString
import Network.Socket hiding (send, sendTo, recv, recvFrom, Datagram)
import Network.Socket.ByteString
import Hotest.API
import Data.Word
import Prelude hiding (length, head, replicate)

data Datagram = Datagram {
    cmd :: Command,
    msg :: ByteString
} deriving (Show, Eq)

datagram :: Command -> ByteString -> Datagram
datagram c m = Datagram c m

readDatagram :: Socket -> IO Datagram
readDatagram sock = do
    cmd <- recv sock 1
    len <- recv sock 1
    msg <- recvExact sock (fromIntegral $ head len) empty
    return $ Datagram (toCmd $ head cmd) msg
    where
        recvExact s 0 m = return m
        recvExact s n m = do
            msg <- recv s n
            recvExact s (n - length msg) (m `append` msg)

writeDatagram :: Socket -> Datagram -> IO ()
writeDatagram sock dgram = do
    send sock $ singleton $ fromCmd $ cmd dgram
    send sock $ singleton $ fromIntegral $ length $ msg dgram
    sendAll sock $ msg dgram

completeMsg :: ByteString -> Int -> ByteString
completeMsg x n = x `append` (replicate (n - (length x)) 0)
