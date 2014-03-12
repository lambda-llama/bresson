{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Data.Bson.Types
    ( RegexOption(..)
    , RegexOptions
    , Value(..)
    , Binary(..)
    , ObjectId(..)
    , Document
    , Array
    , Label
    , Field
    , parse
    , parseMaybe
    , parseEither
    ) where

import Control.Applicative(Applicative(..), Alternative(..))
import Control.DeepSeq (NFData(..))
import Control.Monad (MonadPlus(..), ap)
import Data.Int (Int32, Int64)
import Data.Monoid (Monoid(..))
import Data.Time.Clock (UTCTime)
import Data.Time.Format ()
import Data.Typeable (Typeable)
import Data.Word (Word32, Word16)
import qualified Data.ByteString as S

import Data.BitSet.Word (BitSet)
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.Word.Word24 (Word24)
import Data.Text (Text)
import Data.UUID (UUID)

-- | Options for 'ValueRegex', constructors order is important because
-- it's binary representation should be encoded in alphabetical order.
data RegexOption = RegexOptionCaseInsensitive -- i
                 | RegexOptionLocaleDependent -- l
                 | RegexOptionMultiline       -- m
                 | RegexOptionDotall          -- s
                 | RegexOptionUnicode         -- u
                 | RegexOptionVerbose         -- x
    deriving (Eq, Show, Typeable, Enum)

type RegexOptions = BitSet RegexOption

-- | A  value is one of the following types of values
data Value = ValueDouble {-# UNPACK #-} !Double
           | ValueString {-# UNPACK #-} !Text
           | ValueDocument !Document
           | ValueArray {-# UNPACK #-} !Array
           | ValueBinary !Binary
           | ValueObjectId {-# UNPACK #-} !ObjectId
           | ValueBool !Bool
           | ValueUtcTime {-# UNPACK #-} !UTCTime
           | ValueNull
           | ValueRegex {-# UNPACK #-} !Text !RegexOptions
           | ValueJavascript {-# UNPACK #-} !Text
           | ValueJavascriptWithScope {-# UNPACK #-} !Text !Document
           | ValueInt32 {-# UNPACK #-} !Int32
           | ValueInt64 {-# UNPACK #-} !Int64
           | ValueTimestamp {-# UNPACK #-} !Int64
           | ValueMin
           | ValueMax
    deriving (Eq, Show, Typeable)

type Label = Text
type Document = HashMap Label Value
type Array = Vector Value
type Field = (Label, Value)

data ObjectId = ObjectId
    { objectIdTime    :: {-# UNPACK #-} !Word32
    , objectIdMachine :: {-# UNPACK #-} !Word24
    , objectIdPid     :: {-# UNPACK #-} !Word16
    , objectIdInc     :: {-# UNPACK #-} !Word24
    } deriving (Eq, Show, Typeable)

data Binary = BinaryGeneric     {-# UNPACK #-} !S.ByteString
            | BinaryFunction    {-# UNPACK #-} !S.ByteString
            | BinaryUuid        {-# UNPACK #-} !UUID
            | BinaryMd5         {-# UNPACK #-} !S.ByteString
            | BinaryUserDefined {-# UNPACK #-} !S.ByteString
    deriving (Eq, Show, Typeable)


---- Parser stuff

-- | The result of running a 'Parser'.
data Result a = Error String
              | Success a
                deriving (Eq, Show, Typeable)

instance (NFData a) => NFData (Result a) where
    rnf (Success a) = rnf a
    rnf (Error err) = rnf err

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error err) = Error err
    {-# INLINE fmap #-}

instance Monad Result where
    return = Success
    {-# INLINE return #-}
    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Alternative Result where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

-- | Failure continuation.
type Failure f r   = String -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | A continuation-based parser type.
newtype Parser a = Parser {
      runParser :: forall f r.
                   Failure f r
                -> Success a f r
                -> f r
    }

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                 in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks -> ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in runParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks -> let kf' _ = runParser b kf ks
                                   in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

-- | Run a 'Parser'.
parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) Error Success
{-# INLINE parse #-}

-- | Run a 'Parser' with a 'Maybe' result type.
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v = runParser (m v) (const Nothing) Just
{-# INLINE parseMaybe #-}

-- | Run a 'Parser' with an 'Either' result type.
parseEither :: (a -> Parser b) -> a -> Either String b
parseEither m v = runParser (m v) Left Right
