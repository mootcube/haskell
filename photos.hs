module Mathieu.Photos where
import System.IO
import Text.Regex.Posix ((=~))
import System.FilePath (replaceExtension)
import System.Directory (doesFileExist, renameFile)

getListPhotos::String->IO [String]
getListPhotos path = return []

isNum:: Char -> Bool
isNum c=[c]=~"[0-9]"::Bool
{-	c=='0'||
	c=='1'||
	c=='2'||
	c=='3'||
	c=='4'||
	c=='5'||
	c=='6'||
	c=='7'||
	c=='8'||
	c=='9'
-}

num::String -> Integer
num=read . filter isNum

matchingPhotos::Integer->Integer->[String]->[String]
matchingPhotos min max=filter (\a -> (num a)>=min && (num a)<=max)

renameZippedPhotos::String->[(String,Integer)]->[(String,String)]
renameZippedPhotos name=map (renameZippedPhoto name)
--  where f (a,b)=(a,name++(show b))
                          
renameZippedPhoto::String->(String,Integer)->(String,String)
renameZippedPhoto name (a,b)=(a,name++(show b))

zipPhoto::String->Integer->(String,Integer)
zipPhoto a b=(a,b)

renamePhotos::String->Integer->[String]->[(String,String)]
renamePhotos a n (l:ls)=(renameZippedPhoto a (zipPhoto l n)):(renamePhotos a (n+1) ls)
renamePhotos _ _ []=[]

--zipWith (++) l [1..]