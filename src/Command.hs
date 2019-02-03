module Command where
import           Control.Monad.Writer

data Light = Light {
      turnOn  :: String
    , turnOff :: String     
}

simpleLamp = Light { 
      turnOn  = "The Light is on"
    , turnOff = "The Light is off"
}

flipUpCommand :: Light -> String
flipUpCommand = turnOn

flipDownCommand :: Light -> String
flipDownCommand = turnOff

storeAndExecute :: String -> Writer[String] ()
storeAndExecute command = do
    let logEntry = command
    tell [logEntry]
  
commandDemo :: IO ()
commandDemo = do
    let lamp = simpleLamp
    print $ execWriter $ 
        storeAndExecute (flipUpCommand lamp)   >>
        storeAndExecute (flipDownCommand lamp) >>
        storeAndExecute (flipUpCommand lamp)
    
