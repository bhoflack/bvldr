import Test.Framework (defaultMain)
import qualified Json as J
import qualified Core as C

main = defaultMain $ J.tests ++ C.tests
