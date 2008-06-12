module Slides where
import Text.PrettyPrint.HughesPJ
import Frame (Frame)

data Presentation = Presentation { title    :: String
                                 , author   :: String
                                 , sections :: [Section]
                                 }
data Section  = Section { name :: String, frames :: [Frame], subsections :: [Section]}
