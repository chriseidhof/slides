module Latex where

import Slides
import Frame hiding (title, text)
import qualified Frame as F
import Text.PrettyPrint.HughesPJ

lhs = vcat . map (lhsSec 0) . sections

lhsSec :: Int -> Section -> Doc
lhsSec n sec = c ((concat $ replicate n "sub")  ++ "section") <> braces (text $ name sec)
            $+$ vcat (
              (map lhsFrame (frames sec))
              ++
              (map (lhsSec (n+1)) $ subsections sec)
              )

lhsFrame f = c "frame{" $+$ (
               c "frametitle" <> braces(text $ F.title f)
               $+$ lhsFrameMarkup (markup f)
             ) $+$ rbrace

lhsFrameMarkup (TitlePage)  = c "titlepage"
lhsFrameMarkup (Image i sz) = with "figure" $ c1 "includegraphics" i
lhsFrameMarkup (a :&: b )   = lhsFrameMarkup a $+$ lhsFrameMarkup b
lhsFrameMarkup (Code _ c)   = vcat $ [space] ++ (map (\x -> text $ ">" ++ x) (lines c)) ++ [space]
lhsFrameMarkup (Text t)     = text t
lhsFrameMarkup (Bullet bs)  = with "itemize" $ vcat $ map item bs
 where item i = c "item" <> space <> text i

-- command
c x = text "\\" <> text x
with x body = c1 "begin" x $+$ body $+$ c1 "end" x
c1 x y = c x <> braces (text y)

class Outline t where
  outline :: t -> Doc

instance Outline Presentation where
  outline = nameAndSections title sections

instance Outline Section where
  outline = nameAndSections name subsections

nameAndSections :: (Outline a) => (t -> String) -> (t -> [a]) -> t -> Doc
nameAndSections f s p = (text . f) p  $+$ indent (map outline $ s p)

indent = nest 2 . vcat
