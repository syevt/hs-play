import Control.Applicative
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (Failure e) <*> (Success _) = Failure e
  (Success _) <*> (Failure e) = Failure e
  (Failure e) <*> (Failure e') = Failure $ e <> e'
  (Success f) <*> (Success a) = Success $ f a

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance (Q.Arbitrary e, Q.Arbitrary a) => Q.Arbitrary (Validation e a) where
  arbitrary = do
    a <- Q.arbitrary
    e <- Q.arbitrary
    Q.frequency [(1, return $ Failure e), (3, return $ Success a)]

main :: IO ()
main = do
  quickBatch $
    functor
      (undefined :: Validation ([String], [String], [String]) ( String
                                                              , String
                                                              , String))
  quickBatch $
    applicative
      (undefined :: Validation ([String], [String], [String]) ( String
                                                              , String
                                                              , String))
