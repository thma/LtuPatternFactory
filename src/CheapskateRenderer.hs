module CheapskateRenderer where
import qualified Cheapskate                      as C
import qualified Data.Text                       as T
import qualified Text.Blaze.Html                 as H
import qualified Text.Blaze.Html.Renderer.Pretty as R

type MarkDown = C.Doc
type HTML = H.Html

textToMarkDown :: T.Text -> MarkDown
textToMarkDown = C.markdown C.def

markDownToHtml :: MarkDown -> HTML
markDownToHtml = H.toHtml

htmlToText :: HTML -> T.Text
htmlToText = T.pack . R.renderHtml