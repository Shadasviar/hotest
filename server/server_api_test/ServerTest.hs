import TestCases
import Network.Simple.TCP
import Hotest.API
import Hotest.Datagram
import System.Exit

main = do
    mapM_ testSession sesTestCases
    connect "127.0.0.1" "6666" $ \(sock, addr) -> do
        putStrLn "Run tests set as admin admin ... "
        let (_, creds, expectedAnswer) = head sesTestCases
        writeDatagram sock creds
        res <- readDatagram sock
        if res == expectedAnswer
           then putStrLn "Connected ... "
           else die "Connection to server failed"
        mapM_ (runTest sock) testCases
