module AbstractFactory where
import System.Info (os) -- provide Platform information

-- | representation of a Button UI widget    
data Button = Button 
    { label  :: String           -- the text label of the button
    , render :: Button -> IO ()  -- a platform specific rendering action
    }

-- | rendering a Button for the WIN platform (we just simulate it by printing the label) 
winPaint :: Button -> IO ()
winPaint btn = putStrLn $ "winButton: " ++ label btn

-- | rendering a Button for the OSX platform
osxPaint :: Button -> IO ()
osxPaint btn = putStrLn $ "osxButton: " ++ label btn

-- | paint a button by using the Buttons render function 
paint :: Button -> IO ()
paint btn@(Button _ render) = render btn 
 
-- | a representation of the operating system platform
data Platform = OSX | WIN | NIX | Other

-- | determine Platform by inspecting System.Info.os string
platform :: Platform
platform = case os of
    "darwin"  -> OSX
    "mingw32" -> WIN
    "linux"   -> NIX
    _         -> Other

-- | create a button for os platform with label lbl
createButton :: String -> Button
createButton lbl = case platform of
    OSX    -> Button lbl osxPaint
    WIN    -> Button lbl winPaint
    NIX    -> Button lbl (\btn -> putStrLn $ "nixButton: "   ++ label btn) 
    Other  -> Button lbl (\btn -> putStrLn $ "otherButton: " ++ label btn)

abstractFactoryDemo = do
    putStrLn "AbstractFactory -> functions as data type values"    
    let exit = createButton "Exit"            -- using the "abstract" API to create buttons
    let ok   = createButton "OK"
    paint ok                                  -- using the "abstract" API to paint buttons
    paint exit

    paint $ Button "Apple" osxPaint           -- paint a platform specific button
    paint $ Button "Pi"                       -- paint a user-defined button
        (\btn -> putStrLn $ "raspberryButton: " ++ label btn)