
module Data.Product where

data Product xs where
  Empty :: Product '[]
  (:*)  :: x -> Product xs -> Product (x : xs)

infixr 1 :*

class HasElem a s where
  getElem :: s -> a
  modElem :: (a -> a) -> s -> s

instance HasElem x (Product (x : xs)) where
  getElem   (x :* _) = x
  modElem f (x :* xs) = f x :* xs

instance {-# OVERLAPPABLE #-} HasElem x (Product xs) => HasElem x (Product (y : xs)) where
  getElem   (_ :* xs) = getElem xs
  modElem f (x :* xs) = x :* modElem f xs

instance Show (Product '[]) where
  show _ = "Empty"

instance (Show (Product xs), Show x) => Show (Product (x : xs)) where
  show (x :* xs) = show x ++ " :* " ++ show xs
