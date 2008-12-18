module DepTypes where

import Slides
import Frame hiding (ruby)
import qualified Frame
import Latex
import Text.PrettyPrint.HughesPJ (render)

main = putStrLn $ render $ renderLhs agda

agda = Presentation "Silver Bullet: Dependent Types" "Chris Eidhof" asections

asections = [ introduction
           ,  Section "The Action" []
              [ Section "Typing the shell" [] []
              , Section "Relational databases" [] []
              , Section "Other examples" []
                [  Section "Printf" [] []
                ,  Section "XML" [] []
                ]
              ]
           ,Section "Conclusion"    []
             [ Section "The Crisis" [] []
             , Section "The Resolution" [] []
             , Section "The Solution" 
               [photoCredits] 
               []
             ]
           ]

introduction = Section "Introduction" 
               [ frame "" (titlePage {- <&> image "images/silver-bullet.jpg" 4 -})
               , frame "Curl in Ruby" rubyCode
               ]
               [ Section "Curl" [] []
               , Section "The solution: dependent types" [] []
               ]


rubyCode = Frame.ruby "def curl(url, port)\nreturn \"Hello, world.\"\nend"

photoCredits = frame "Photo credits" (text "All photos are from flickr: " <&> bullets
                                           [ "Silver bullet: monkeyc" , "Abacus: ansik"
                                           , "Tape: mattblaze" , "Databases: (nz)dave"
                                           , "Ammo: rwr" ]
                                     )
