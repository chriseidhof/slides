module Frame where

data Frame = Frame {title :: String, markup :: FrameMarkup}
data FrameMarkup = Bullet [String]
                 | Image String Int
                 | TitlePage
                 | FrameMarkup :&: FrameMarkup
                 | Code Language String
                 | Text String

type Language = String

frame :: String -> FrameMarkup -> Frame
frame = Frame
bullets :: [String] -> FrameMarkup
bullets = Bullet
(<&>) :: FrameMarkup -> FrameMarkup -> FrameMarkup
(<&>) = (:&:)
titlePage :: FrameMarkup
titlePage = TitlePage
image :: String -> Int -> FrameMarkup
image = Image
code :: String -> Language -> FrameMarkup
code = Code

ruby    = code "ruby"
haskell = code "ruby"
text = Text
