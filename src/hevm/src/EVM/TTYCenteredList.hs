module EVM.TTYCenteredList where

-- Hard fork of brick's List that centers the currently highlighted line.

import Control.Lens
import Data.Maybe (fromMaybe)

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List

import qualified Data.Vector as V

-- | Turn a list state value into a widget given an item drawing
-- function.
renderList :: (Ord n, Show n)
           => (Bool -> e -> Widget n)
           -- ^ Rendering function, True for the selected element
           -> Bool
           -- ^ Whether the list has focus
           -> List n e
           -- ^ The List to be rendered
           -> Widget n
           -- ^ rendered widget
renderList drawElem foc l =
    withDefAttr listAttr $
    drawListElements foc l drawElem

drawListElements :: (Ord n, Show n) => Bool -> List n e -> (Bool -> e -> Widget n) -> Widget n
drawListElements foc l drawElem =
    Widget Greedy Greedy $ do
        c <- getContext

        let es = V.slice start num (l^.listElementsL)
            idx = fromMaybe 0 (l^.listSelectedL)

            start = max 0 $ idx - (initialNumPerHeight `div` 2)
            num = min (numPerHeight * 2) (V.length (l^.listElementsL) - start)

            -- The number of items to show is the available height divided by
            -- the item height...
            initialNumPerHeight = (c^.availHeightL) `div` (l^.listItemHeightL)
            -- ... but if the available height leaves a remainder of
            -- an item height then we need to ensure that we render an
            -- extra item to show a partial item at the top or bottom to
            -- give the expected result when an item is more than one
            -- row high. (Example: 5 rows available with item height
            -- of 3 yields two items: one fully rendered, the other
            -- rendered with only its top 2 or bottom 2 rows visible,
            -- depending on how the viewport state changes.)
            numPerHeight = initialNumPerHeight +
                           if initialNumPerHeight * (l^.listItemHeightL) == c^.availHeightL
                           then 0
                           else 1

            -- off = start * (l^.listItemHeightL)

            drawnElements = flip V.imap es $ \i e ->
                let isSelected = i == (if start == 0 then idx else div initialNumPerHeight 2)
                    elemWidget = drawElem isSelected e
                    selItemAttr = if foc
                                  then withDefAttr listSelectedFocusedAttr
                                  else withDefAttr listSelectedAttr
                    makeVisible = if isSelected
                                  then visible . selItemAttr
                                  else id
                in makeVisible elemWidget

        render $ viewport (l^.listNameL) Vertical $
                 -- translateBy (Location (0, off)) $
                 vBox $ V.toList drawnElements
