module DependencyInjection where
import qualified Data.Text as T
import           Control.Arrow
import           CheapskateRenderer (HTML, MarkDown, textToMarkDown, markDownToHtml, htmlToText)
--import           CMarkGFMRenderer   (HTML, MarkDown, textToMarkDown, markDownToHtml, htmlToText)

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

-- | render a ToC as a Text with html markup. 
--   we specify this function as a chain of parse and rendering functions that must be provided externally
tocToHtmlText :: (TableOfContents -> T.Text) -- 1. a renderer function from ToC to Text with markdown markups
              -> (T.Text -> MarkDown)        -- 2. a parser function from Text to a MarkDown document
              -> (MarkDown -> HTML)          -- 3. a renderer function from MarkDown to an HTML document
              -> (HTML -> T.Text)            -- 4. a renderer function from HTML to Text
              -> TableOfContents             -- the actual ToC to be rendered
              -> T.Text                      -- the Text output (containing html markup)
tocToHtmlText tocToMdText textToMd mdToHtml htmlToText = 
    tocToMdText >>>    -- 1. render a ToC as a Text (consisting of properly indented Markdown)
    textToMd    >>>    -- 2. parse text with Markdown to a MarkDown data structure
    mdToHtml    >>>    -- 3. convert the MarkDown data to an HTML data structure
    htmlToText         -- 4. render the HTML data to a Text with hmtl markup


-- | a default implementation of a ToC to html Text renderer.
--   this function is constructed by partially applying `tocToHtmlText` to four functions matching the signature of `tocToHtmlText`.
defaultTocToHtmlText :: TableOfContents -> T.Text
defaultTocToHtmlText = 
    tocToHtmlText 
        tocToMDText         -- the ToC to markdown Text renderer as defined above
        textToMarkDown      -- a MarkDown parser, externally provided via import
        markDownToHtml      -- a MarkDown to HTML renderer, externally provided via import
        htmlToText          -- a HTML to Text with html markup, externally provided via import

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