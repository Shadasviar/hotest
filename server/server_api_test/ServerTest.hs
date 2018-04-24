import TestCases
import Network.Simple.TCP

main = do
    mapM_ testSession sesTestCases
    connect "127.0.0.1" "6666" $ \(sock, addr) -> do
        mapM_ (runTest sock) testCases
