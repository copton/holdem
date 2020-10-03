module EnumExtra where

-- | enumFromToLeftRel x n = [x-n .. x]
enumFromToLeftRel :: Enum a => a -> Int -> [a]
enumFromToLeftRel x n =
    let x' = fromEnum x
    in map toEnum [x'-n .. x']
