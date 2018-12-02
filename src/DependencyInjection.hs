module DependencyInjection where
import qualified Data.Text as T
import           Control.Arrow
import           CheapskateRenderer (HTML, MarkDown, textToMarkDown, markDownToHtml, htmlToText)
--import           CMarkGFMRenderer (HTML, MarkDown, textToMarkDown, markDownToHtml, htmlToText)


-- | a table of contents consists of a heading and a list of entries
data TableOfContents = Section Heading [TocEntry]

-- | a ToC entry can be a heading or a sub-table of contents
data TocEntry = Head Heading | Sub TableOfContents

-- | a heading can be just a title string or an url with a title and the actual link
data Heading = Title String | Url String String

-- | render a ToC entry as a Markdown String with the proper indentation
teToMd :: Int -> TocEntry -> String
teToMd depth (Head head) = headToMd depth head
teToMd depth (Sub toc)   = tocToMd  depth toc 

-- | render a heading as a Markdown String with the proper indentation
headToMd :: Int -> Heading -> String
headToMd depth (Title str)     = indent depth ++ "* " ++ str ++ "\n"
headToMd depth (Url title url) = indent depth ++ "* [" ++ title ++ "](" ++ url ++ ")\n"

-- | convert a ToC to Markdown String. The parameter depth is used for proper indentation.
tocToMd :: Int -> TableOfContents -> String
tocToMd depth (Section heading entries) = headToMd depth heading ++ concatMap (teToMd (depth+2)) entries

-- | produce a String of length n, consisting only of blanks
indent :: Int -> String
indent n = replicate n ' '    

-- | render a ToC as a Text (consisting of properly indented Markdown)
tocToMDText :: TableOfContents -> T.Text
tocToMDText = T.pack . tocToMd 0

tocToHtmlText :: (TableOfContents -> T.Text) -- a renderer function ToC to Text (with Mardown markups)
              -> (T.Text -> MarkDown)        -- a parser function from Text to a MarkDown document
              -> (MarkDown -> HTML)          -- a renderer function from MarkDown to a HTML document
              -> (HTML -> T.Text)            -- a renderer function HTML to Text
              -> TableOfContents             -- the actual ToC to be rendered
              -> T.Text                      -- the text output
tocToHtmlText tocToText textToMd mdToHtml htmlToText = tocToText >>> textToMd >>> mdToHtml >>> htmlToText


defaultTocToHtmlText :: TableOfContents -> T.Text
defaultTocToHtmlText = tocToHtmlText tocToMDText textToMarkDown markDownToHtml htmlToText

demoDI = do
    let toc = Section (Title "Chapter 1")
                [ Sub $ Section (Title "Section a") 
                    [Head $ Title "First Heading", 
                     Head $ Url "Second Heading" "http://the.url"]
                , Sub $ Section (Url "Section b" "http://the.section.b.url") 
                    [ Sub $ Section (Title "UnderSection b1") 
                        [Head $ Title "First", Head $ Title "Second"]]]
    putStrLn $ T.unpack $ tocToMDText toc    
                        
    putStrLn $ T.unpack $ defaultTocToHtmlText toc                                                                                                                  