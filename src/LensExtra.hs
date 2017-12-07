module LensExtra where

import Control.Lens


assoced :: Traversable t => IndexedTraversal' k (t (k,v)) v
assoced = traverse . itraversed
