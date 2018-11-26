module AbstractFactory where
import System.Info (os) -- provide Platform information

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
 
data Platform = OSX | WIN | NIX | Other

platform :: Platform
platform = case os of
    "darwin"  -> OSX
    "mingw32" -> WIN
    "linux"   -> NIX
    _         -> Other

-- | create a button for os platform with label lbl
createButton :: String -> Button
createButton lbl = case platform of
    OSX    -> Button lbl (osxPaint lbl)
    WIN    -> Button lbl (winPaint lbl)
    NIX    -> Button lbl (osxPaint lbl)
    Other  -> Button lbl (osxPaint lbl)


abstractFactoryDemo = do
    putStrLn "AbstractFactory -> functions as data type values"    
    let exit = createButton "Exit"    
    let ok = createButton "OK"
    paint ok
    paint exit

    let osxButton = Button "apple" (putStrLn "osxButton: apple")    
    paint osxButton