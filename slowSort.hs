slow::Ord a=>[a]->[a]
slow [] = []
slow (xs) = head (filter order (permutar xs))

order::Ord a=>[a]->Bool
order [] = True
order [x] = True
order (x:y:xs) = (x<=y)&&(order (y:xs))