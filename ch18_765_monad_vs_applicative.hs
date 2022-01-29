import Control.Applicative

cowFromString3 :: String -> Int -> Int -> Maybe Cow
cowFromString3 name age weight = do
  nammy <- noEmpty name
  agey <- noNegative age
  weighty <- noNegative weight
  Just $ Cow nammy agey weighty

-- vs Applicative
cowFromString' name' age' weight' =
  Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'
