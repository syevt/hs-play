import Control.Applicative

data Cow =
  Cow
    { name :: String
    , age :: Int
    , weight :: Int
    }
  deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
   in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Maybe Cow
mkSphericalCow name' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative weight' of
        Nothing -> Nothing
        Just weighty -> weightCheck (Cow nammy weighty)

cowFromString3 :: String -> Int -> Int -> Maybe Cow
cowFromString3 name age weight = do
  nammy <- noEmpty name
  agey <- noNegative age
  weighty <- noNegative weight
  Just $ Cow nammy agey weighty

-- vs Applicative
cowFromString' name' age' weight' =
  Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'
