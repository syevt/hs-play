import           Test.QuickCheck
import           Test.QuickCheck.Function

data List a = Nil | Cons a (List a) deriving Show

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

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
