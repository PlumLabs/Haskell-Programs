--dice si una palabra es un anagrama de otra palabra
anagramas :: String -> String -> Bool
anagramas "" "" = True
anagramas "" (ys) = False
anagramas (xs) "" = False
anagramas (xs) (ys) = find xs (permutar(ys))

--busca una palabra dentro de una lista de palabras
find::String->[String]->Bool
find "" _ = False
find _ [] = False
find (xs) (y:ys) | xs==y = True
                 | otherwise = find (xs) (ys)

--dada una palabra genera una lista con todas las
--permutacionde de dicha palabra
permutar:: String->[String]
permutar "" = [""]
permutar (xs) = concat[map((++)[y]) (permutar(cut y xs )) | y<-xs ]

--funcion auxiliar usada por permutar
cut:: Char->String->String
cut x "" = ""
cut x (y:ys) | x==y = ys
			 | otherwise = [y]++(cut x ys)