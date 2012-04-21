import Data.List
import System.IO


test a=
  case a of
    1 -> "pouik"
    2 -> "test"
    _ -> "haha"

alpha = sort(nub (zip "zy qeeejp mysljylc kd kxveddknmc re jsicpdrysirbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcdde kr kd eoya kw aej tysr re ujdr lkgc jv" "qa zooour language is impossible to understandthere are twenty six factorial possibilitiesso it is okay if you want to just give up" ))

myGetChar x [] = x
myGetChar x ((a,b):ls) 
  |x==a = b
  |x/=a = myGetChar x ls

translate xs = map translateChar xs
               where translateChar x = myGetChar x alpha

main::IO ()
main=interact translate
  
titi::IO ()  
titi=
  do
  file<-openFile "input.txt" ReadMode
  n<-hGetLine file
  a<-hGetContents file
  let b=translate a
  putStrLn b
  hClose file

--main=do
--  inh=
--openFile "input.txt" ReadMode >>= ( \h -> hGetContents h >>= putStrLn >> hClose h)
