import Data.List



test a=
  case a of
    1 -> "pouik"
    2 -> "test"
    _ -> "haha"

alpha = sort(nub (zip "zy qeeejp mysljylc kd kxveddknmc re jsicpdrysirbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcdde kr kd eoya kw aej tysr re ujdr lkgc jv" "qa zooour language is impossible to understandthere are twenty six factorial possibilitiesso it is okay if you want to just give up" ))

myGetChar _ [] = '\0'
myGetChar x ((a,b):ls) 
  |x==a = b
  |x/=a = myGetChar x ls

translate xs = map translateChar xs
               where translateChar x = myGetChar x alpha
