module ESE171611 where


-- see t.e. E20170901, E20170720

inL :: (Eq a) => a -> [a] -> Bool
inL _ [] = False
inL x (y:ys)
    | x == y = True
    | otherwise = (inL x ys)

