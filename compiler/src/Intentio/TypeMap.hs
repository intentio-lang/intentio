{-# LANGUAGE AllowAmbiguousTypes #-}

module Intentio.TypeMap
  ( -- * TypeMap type
    TypeMap

    -- * Construction
  , empty
  , singleton

    -- * Size
  , null
  , size

    -- * Query
  , hasType
  , lookup
  , findWithDefault

    -- * Insertion
  , insert
  , insertWith

    -- * Update/delete
  , delete
  , adjust
  , update
  , alter

    -- * Union
  , union
  , unions

    -- * Lenses
  , at
  )
where

import           Intentio.Prelude        hiding ( empty
                                                , null
                                                , at
                                                )

import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                , fromDynamic
                                                )
import qualified Data.Map                      as M
import           Data.Typeable                  ( typeOf
                                                , typeRep
                                                )

newtype TypeMap = TypeMap (M.Map TypeRep Dynamic)
  deriving (Show)

instance Semigroup TypeMap where
  (<>) = union
  {-# INLINABLE (<>) #-}

instance Monoid TypeMap where
  mempty = empty
  {-# INLINABLE mempty #-}
  mconcat = unions
  {-# INLINABLE mconcat #-}

-- | Construct an empty type map.
empty :: TypeMap
empty = TypeMap M.empty
{-# INLINABLE empty #-}

-- | Construct type map with single item.
singleton :: Typeable a => a -> TypeMap
singleton a = TypeMap $ M.singleton (typeOf a) (toDyn a)
{-# INLINABLE singleton #-}

-- | Return 'True' if this type map is empty, 'False' otherwise.
null :: TypeMap -> Bool
null (TypeMap m) = M.null m
{-# INLINABLE null #-}

-- | Return the number of values in this set.
size :: TypeMap -> Int
size (TypeMap m) = M.size m
{-# INLINABLE size #-}

-- | Return 'True' if map has value of given type, otherwise 'False'.
hasType :: forall a . Typeable a => TypeMap -> Bool
hasType (TypeMap m) = M.member (typeRep $ Proxy @a) m
{-# INLINABLE hasType #-}

-- | Return the value of given type, or 'Nothing' if this set does not
-- contain one.
lookup :: forall a . Typeable a => TypeMap -> Maybe a
lookup (TypeMap m) = fromDynamic' <$> M.lookup (typeRep $ Proxy @a) m
{-# INLINABLE lookup #-}

-- | Return the value of given type, or the default if this set does not
-- contain one.
findWithDefault :: Typeable a => a -> TypeMap -> a
findWithDefault a (TypeMap m) = fromDynamic' x
  where x = M.findWithDefault (toDyn a) (typeOf a) m
{-# INLINABLE findWithDefault #-}

-- | Add specified value to this set. If it already contains an value
-- of such type, it is replaced by the.
insert :: Typeable a => a -> TypeMap -> TypeMap
insert a (TypeMap m) = TypeMap $ M.insert (typeOf a) (toDyn a) m
{-# INLINABLE insert #-}

-- | Add specified value to this set. If it already contains an value
-- of such type, it is replaced by the result of applying the given function to
-- the new and old value.
insertWith :: Typeable a => (a -> a -> a) -> a -> TypeMap -> TypeMap
insertWith f a (TypeMap m) = TypeMap $ M.insertWith f' (typeOf a) (toDyn a) m
  where f' x y = toDyn $ f (fromDynamic' x) (fromDynamic' y)
{-# INLINABLE insertWith #-}

-- | Remove value from this set if present.
delete :: forall a . Typeable a => TypeMap -> TypeMap
delete (TypeMap m) = TypeMap $ M.delete (typeRep $ Proxy @a) m
{-# INLINABLE delete #-}

-- | Adjust the value of given type in this map only if it is present.
-- Otherwise, leave the map alone.
adjust :: forall a . Typeable a => (a -> a) -> TypeMap -> TypeMap
adjust f (TypeMap m) = TypeMap $ M.adjust f' (typeRep $ Proxy @a) m
  where f' = toDyn . f . fromDynamic'
{-# INLINABLE adjust #-}

-- | The expression @update f map@ updates the value @x@ of type accepted by
-- function @f@, (if it is in the map). If @f x@ is 'Nothing',
-- the element is deleted. If it is @Just y@, the key @k@ is bound to the
-- new value @y@.
update :: forall a . Typeable a => (a -> Maybe a) -> TypeMap -> TypeMap
update f (TypeMap m) = TypeMap $ M.update f' (typeRep $ Proxy @a) m
  where f' x = toDyn <$> f (fromDynamic' x)
{-# INLINABLE update #-}

-- | The expression @alter f map@ alters the value @x@ of type accepted by
-- function @f@, or absence thereof. 'alter' can be used to insert, delete,
-- or update a value in a map.
alter :: forall a . Typeable a => (Maybe a -> Maybe a) -> TypeMap -> TypeMap
alter f (TypeMap m) = TypeMap $ M.alter f' (typeRep $ Proxy @a) m
  where f' x = toDyn <$> f (fromDynamic' <$> x)
{-# INLINABLE alter #-}

-- | The expression @union t1 t2@ takes the left-biased union of @t1@ and @t2@.
-- It prefers t1 when duplicate keys are encountered.
union :: TypeMap -> TypeMap -> TypeMap
union (TypeMap t1) (TypeMap t2) = TypeMap $ M.union t1 t2
{-# INLINABLE union #-}

-- | The union of a list of maps.
unions :: Foldable f => f TypeMap -> TypeMap
unions = foldl' union empty
{-# INLINABLE unions #-}

at :: forall a . Typeable a => Lens' TypeMap (Maybe a)
at f m = f mv <&> \case
  Nothing -> maybe m (const $ delete @a m) mv
  Just v' -> insert v' m
  where mv = lookup @a m
{-# INLINABLE at #-}

fromDynamic' :: Typeable a => Dynamic -> a
fromDynamic' x = fromDynamic x ^?! _Just
