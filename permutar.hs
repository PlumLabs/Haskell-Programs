permutar:: Eq a=>[a]->[[a]]
permutar [] = [[]]
permutar (xs) = concat[map((++)[y]) (permutar(cut y xs )) | y<-xs ]

cut:: Eq a=> a->[a]->[a]
cut x [] = []
cut x (y:ys) | x==y = ys
			 | otherwise = [y]++(cut x ys)