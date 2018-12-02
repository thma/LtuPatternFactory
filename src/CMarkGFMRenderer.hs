module CMarkGFMRenderer where
import qualified CMarkGFM                        as CM                             
import qualified Data.Text                       as T

type MarkDown = T.Text
type HTML = T.Text

textToMarkDown :: T.Text -> MarkDown
textToMarkDown = CM.commonmarkToHtml [] []

markDownToHtml :: MarkDown -> HTML
markDownToHtml = id

htmlToText :: HTML -> T.Text
htmlToText = id