import Control.Applicative
import qualified Data.Monoid as M
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhEither b a
  = Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhEither e) where
  fmap _ (Main.Right e) = Main.Right e
  fmap f (Main.Left a) = Main.Left (f a)

instance Monoid e => Applicative (PhEither e) where
  pure a = Main.Left a
  (Main.Right e) <*> (Main.Right _) = Main.Right e
  (Main.Right e) <*> (Main.Left _) = Main.Right e
  (Main.Left _) <*> (Main.Right e) = Main.Right e
  (Main.Left f) <*> (Main.Left a) = Main.Left $ f a

instance Monoid a => Monad (PhEither a) where
  return = pure
  (Main.Right e) >>= _ = Main.Right e
  (Main.Left a) >>= f = f a

instance (Eq e, Eq a) => EqProp (PhEither e a) where
  (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (PhEither e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    frequency [(1, return $ Main.Right e), (3, return $ Main.Left a)]

typeToCheck ::
     PhEither ([M.Product Int], [M.Sum Int], [String]) ( M.Sum Int
                                                       , String
                                                       , M.Product Int)
typeToCheck = undefined

main :: IO ()
main = do
  quickBatch $ functor typeToCheck
  quickBatch $ applicative typeToCheck
  quickBatch $ monad typeToCheck
