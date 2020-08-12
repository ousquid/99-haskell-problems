import Data.List

fullWords :: Int -> String
fullWords num = concat $ intersperse "-" $ [numLUT !! (read [nChar]) | nChar <- show num]
    where numLUT = ["zero","one","two","three","four","five","six","seven","eight","nine"]