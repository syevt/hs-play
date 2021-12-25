import           Test.QuickCheck
import           Test.QuickCheck.Function

data IgnoreOne f g a b = IgnoreSomething (f a) (g b)
  deriving Show

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa ga) = IgnoreSomething fa (fmap f ga)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == id x

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = fmap (g . f) x == (fmap g . fmap f $ x)

type IntToString = Fun Int String
type StringToInt = Fun String Int
type IntFI = [String] -> Bool
type IntFC = [String] -> StringToInt -> IntToString -> Bool

main :: IO ()
main = do
  quickCheck (functorIdentity :: IntFI)
  quickCheck (functorCompose :: IntFC)
