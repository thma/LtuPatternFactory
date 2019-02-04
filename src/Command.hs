{-# LANGUAGE FlexibleContexts #-}
module Command where
import           Control.Monad.Writer
import           Control.Monad.Cont
import           Control.Monad.Identity

data Light = Light {
      turnOn  :: IO String
    , turnOff :: IO String     
}

simpleLamp = Light { 
      turnOn  = putStrLn "The Light is on"  >> return "on"
    , turnOff = putStrLn "The Light is off" >> return "off"
}

flipUpCommand :: Light -> IO String
flipUpCommand = turnOn

flipDownCommand :: Light -> IO String
flipDownCommand = turnOff

storeAndExecute :: IO String -> WriterT[String] IO ()
storeAndExecute command = do
    logEntry <- liftIO command
    tell [logEntry]

storeAndExecute' :: Light -> (Light -> IO String) -> WriterT[String] IO ()
storeAndExecute' light command = do
    logEntry <- liftIO (command light)
    tell [logEntry]   

storeAndExecute'' :: Light -> ContT (String) IO Light
storeAndExecute'' light = ContT $ \command -> do
    liftIO (command light)

funA :: String -> Int
funA = length

funB :: String -> Int
funB = const 5

applyCommand :: String -> (String -> Int) -> Int
applyCommand x f = f x

applyCommand' :: String -> Cont Int String
applyCommand' x = ContT $ \f -> f x

commandDemo :: IO ()
commandDemo = do
    let lamp = simpleLamp
    result <- execWriterT $ 
        storeAndExecute' lamp flipUpCommand   >>
        storeAndExecute' lamp flipDownCommand >>
        storeAndExecute' lamp flipUpCommand
    putStrLn $ "switch history: " ++ show result
    r <- runContT (storeAndExecute'' lamp) flipUpCommand
    print r
    
