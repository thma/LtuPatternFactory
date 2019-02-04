{-# LANGUAGE FlexibleContexts #-}
module Command where
import           Control.Monad.Writer

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

commandDemo :: IO ()
commandDemo = do
    putStrLn "Command -> higher order functions"
    let lamp = simpleLamp
    result <- execWriterT $ 
        storeAndExecute (flipUpCommand lamp)   >>
        storeAndExecute (flipDownCommand lamp) >>
        storeAndExecute (flipUpCommand lamp)
    putStrLn $ "switch history: " ++ show result 
