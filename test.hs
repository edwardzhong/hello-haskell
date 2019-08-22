import           Data.Char
import           Control.Monad -- MonadPlus 和 guard
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Control.Monad.Identity

isPassValid::String -> Bool
isPassValid s = length s >= 8 && check s
        where check s = and [f s | f <- any <$> [isUpper,isLower,isNumber]]

setPassword::MaybeT (WriterT String IO)()
setPassword = do
        liftIO $ putStrLn "Please set a password"
        pass <- liftIO $ getLine
        guard $ isPassValid pass
        tell pass

-- runWriterT $ runMaybeT  setPassword