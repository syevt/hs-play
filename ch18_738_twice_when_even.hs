twiceWhenEven :: [Integer] -> [Integer]
  -- x <- xs
  -- if even x
    -- then [x * x, x * x]
    -- else []
-- twiceWhenEven xs = do
twiceWhenEven xs =
  xs >>=
  (\x ->
     if even x
       then [x * x, x * x]
       else [])
