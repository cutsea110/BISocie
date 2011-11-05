import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main (defaultMain)
import Application (withBISocie)

main :: IO ()
main = defaultMain fromArgs withBISocie
