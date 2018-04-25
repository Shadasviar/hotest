module TestCases (
    sesTestCases,
    testCases,
    runTest,
    testSession
) 
where

import Hotest.Datagram
import Hotest.API
import qualified Data.ByteString.Char8 as BSC 
import Data.ByteString hiding (map, putStrLn, zipWith, length, putStr)
import Prelude hiding (concat, head, last)
import Network.Simple.TCP
import Text.Printf


sesTestCases :: [(String, Datagram, Datagram)]
sesTestCases = [
        ("OPEN_SESSION admin admin",
        datagram OPEN_SESSION $ opSesData ["admin", "admin"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd OPEN_SESSION]
        ),
        ("OPEN_SESSION lol lol",
        datagram OPEN_SESSION $ opSesData ["lol", "lol"],
        datagram ERROR_DATAGRAM $ pack [fromErrCode ACCESS_DENIED, fromCmd OPEN_SESSION]
        )
    ]

testCases :: [(String, Datagram, Datagram)]
testCases = [
        ("GET_TEST_LIST_SIZE 0",
        datagram GET_TEST_LIST_SIZE $ empty,
        datagram ERROR_DATAGRAM $ singleton 0
        ),
        ("CLOSE_SESSION",
        datagram CLOSE_SESSION $ empty,
        datagram ERROR_DATAGRAM $ pack [fromErrCode SUCCESS, fromCmd CLOSE_SESSION]
        )
    ]

runTest :: Socket -> (String, Datagram, Datagram) -> IO ()
runTest s (name, out, expected) = do
    printf "%-40s" $ name ++ " ... "
    writeDatagram s out
    res <- readDatagram s
    putStrLn $ testRes res $ res == expected
            
testSession :: (String, Datagram, Datagram) -> IO ()
testSession (name, out, expected) = do
    connect "127.0.0.1" "6666" $ \(sock, addr) -> do
        printf "%-40s" $ name ++ " ... "
        writeDatagram sock out
        res <- readDatagram sock
        putStrLn $ testRes res $ res == expected
        closeSock sock

opSesData :: [String] -> ByteString
opSesData x = concat $ zipWith completeMsg (map BSC.pack x) [20,30]

testRes :: Datagram -> Bool -> String
testRes _ True = "[  OK  ]"
testRes res _ = "[FAILED] " ++ show res
