import Data.List



test a=
  case a of
    1 -> "pouik"
    2 -> "test"
    _ -> "haha"

alpha = sort(nub (zip "y qeeejp mysljylc kd kxveddknmc re jsicpdrysirbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcdde kr kd eoya kw aej tysr re ujdr lkgc jv" "a zooour language is impossible to understandthere are twenty six factorial possibilitiesso it is okay if you want to just give up" ))

myGetChar x ((a,b):ls) = case a of
  x -> b
  _ -> myGetChar x ls
myGetChar _ [] = ' '

translate xs = map translateChar xs
               where translateChar x=myGetChar x alpha
