import Controller (withTest)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3001
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withBISocie $ run port . debug
