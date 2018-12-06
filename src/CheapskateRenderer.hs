module CheapskateRenderer
    ( MarkDown
    , HTML
    , textToMarkDown
    , markDownToHtml
    , htmlToText
    ) where

import qualified Cheapskate                      as C
import qualified Data.Text                       as T
import qualified Text.Blaze.Html                 as H
import qualified Text.Blaze.Html.Renderer.Pretty as R

-- | a type synonym that hides the Cheapskate internal Doc type
type MarkDown = C.Doc

-- | a type synonym the hides the Blaze.Html internal Html type
type HTML = H.Html

-- | parse Markdown from a Text (with markdown markup). Using the Cheapskate library.
textToMarkDown :: T.Text -> MarkDown
textToMarkDown = C.markdown C.def

-- | convert MarkDown to HTML by using the Blaze.Html library
markDownToHtml :: MarkDown -> HTML
markDownToHtml = H.toHtml

-- | rendering a Text with html markup from HTML. Using Blaze again.
htmlToText :: HTML -> T.Text
htmlToText = T.pack . R.renderHtml
