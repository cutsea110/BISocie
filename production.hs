import Application (withBISocie)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withBISocie $ run 3001
