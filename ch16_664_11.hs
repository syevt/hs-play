import           Test.QuickCheck
import           Test.QuickCheck.Function

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read sa  ) = Read (f . sa)
  -- fmap f (OneGoat a         ) = OneGoat (f a)
  -- fmap f (MoreGoats as bs cs) = MoreGoats (fmap f as) (fmap f bs) (fmap f cs)

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
