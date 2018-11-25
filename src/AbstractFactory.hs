module AbstractFactory where

-- | representation of a Button UI widget    
data Button = Button 
    { label  :: String -- the text label of the button
    , paint  :: IO ()  -- a platform dependent rendering action
    }

-- | rendering a Button for the WIN platform (we just simulate it by printing the label) 
winPaint :: String -> IO ()
winPaint lbl = putStrLn $ "winButton: " ++ lbl

-- | rendering a Button for the OSX platform
osxPaint :: String -> IO ()
osxPaint lbl = putStrLn $ "osxButton: " ++ lbl
 
-- | enumeration of supported operating system platforms
data OS = OSX | WIN deriving (Show, Eq, Enum)

-- | create a button for os platform with label lbl
createButtonFor :: OS -> String -> Button
createButtonFor os lbl = 
    case os of
        WIN -> Button lbl (winPaint lbl)
        OSX -> Button lbl (osxPaint lbl)

abstractFactoryDemo = do
    putStrLn "AbstractFactory -> functions as data type values"
    let os = WIN
    let createButton = createButtonFor os
    let ok = createButton "OK"
    let exit = createButton "Exit"    
    paint ok
    paint exit

    paint $ createButtonFor OSX "about"

    let linuxButton = Button "penguin" (putStrLn "linuxButton: penguin")    
    paint linuxButton