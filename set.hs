set::Eq a=>[a]->[[a]]
set (xs) = concat[map((++)[y]) (set(droop y xs)) | y<-xs]++[[]]

droop::Eq a=> a->[a]->[a]
droop a [] = []
droop x (y:ys) | x==y = ys
			   | otherwise = droop x ys