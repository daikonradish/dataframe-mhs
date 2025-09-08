{-# LANGUAGE FlexibleInstances #-}
module DataFrame.Functions where

import DataFrame.Core
import DataFrame.Column
import DataFrame.Expression
import DataFrame.Eval

-- | Each function must have a type class that specialies it to the
-- | right type.

class (Eq a) => ExprEq a where
    eq :: Expr a -> Expr a -> Expr Bool

instance ExprEq Int where
    eq l r = BinaryIntToIntOp (==) l r

instance ExprEq Double where
    eq l r = BinaryDoubleToDoubleOp (==) l r

instance ExprEq [Char] where
    eq l r = BinaryStringToStringOp (==) l r

instance ExprEq Bool where
    eq l r = BinaryBoolToBoolOp (==) l r


class (Ord a) => ExprGeq a where
    geq :: Expr a -> Expr a -> Expr Bool

instance ExprGeq Int where
    geq l r = BinaryIntToIntOp (>=) l r

instance ExprGeq Double where
    geq l r = BinaryDoubleToDoubleOp (>=) l r

instance ExprGeq [Char] where
    geq l r = BinaryStringToStringOp (>=) l r

instance ExprGeq Bool where
    geq l r = BinaryBoolToBoolOp (>=) l r

-- For addition
class (Num a) => ExprAdd a where
    add :: Expr a -> Expr a -> Expr a

instance ExprAdd Int where
    add l r = BinaryIntToIntOp (+) l r

instance ExprAdd Double where
    add l r = BinaryDoubleToDoubleOp (+) l r

-- | Multiplication
class (Num a) => ExprMult a where
    mult :: Expr a -> Expr a -> Expr a

instance ExprMult Int where
    mult l r = BinaryIntToIntOp (*) l r

instance ExprMult Double where
    mult l r = BinaryDoubleToDoubleOp (*) l r

-- | fromInteger
class (Num a) => ExprFromInteger a where
    fInteger :: Integer -> Expr a

instance ExprFromInteger Int where
    fInteger = Lit . fromInteger

instance ExprFromInteger Double where
    fInteger = Lit . fromInteger

-- | fromInteger
class (Num a) => ExprNegate a where
    neg :: Expr a -> Expr a

instance ExprNegate Int where
    neg = UnaryIntOp negate

instance ExprNegate Double where
    neg = UnaryDoubleOp negate

-- | abs
class (Num a) => ExprAbsolute a where
    absolute :: Expr a -> Expr a

instance ExprAbsolute Int where
    absolute = UnaryIntOp abs

instance ExprAbsolute Double where
    absolute = UnaryDoubleOp abs

-- | signum
class ExprSigNum a where
    sig :: Expr a -> Expr a

instance ExprSigNum Int where
    sig = UnaryIntOp signum

instance ExprSigNum Double where
    sig = UnaryDoubleOp signum

instance (ToColumn a,
          Num a,
          ExprAdd a,
          ExprFromInteger a,
          ExprMult a,
          ExprNegate a,
          ExprAbsolute a,
          ExprSigNum a) => Num (Expr a) where
    (+) = add
    (*) = mult
    fromInteger = fInteger
    negate = neg
    abs = absolute
    signum = sig

-- | fromRational
class (Fractional a) => ExprFromRational a where
    fromRational' :: (Fractional a) => Rational -> Expr a

instance ExprFromRational Double where
    fromRational' = Lit . fromRational

-- | (/)
class (Fractional a) => ExpDivide a where
    divide :: Expr a -> Expr a -> Expr a

instance ExpDivide Double where
    divide = BinaryDoubleToDoubleOp (/)

instance (Fractional a,
          ToColumn a,
          ExprFromRational a,
          ExpDivide a,
          Num a,
          ExprAdd a,
          ExprFromInteger a,
          ExprMult a,
          ExprNegate a,
          ExprAbsolute a,
          ExprSigNum a) => Fractional (Expr a) where
    fromRational = fromRational'
    (/) = divide

filterWhere :: Expr Bool -> DataFrame -> DataFrame
filterWhere expr df = case interpret expr df of
    (CBool xs) -> let
            ixs = map fst (filter snd xs)
        in fromNamedColumns $ map (\(i, v) -> (i, atIndicies ixs v)) (columns df)
    _          -> error "Should not be possible"

derive :: ToColumn a => String -> Expr a -> DataFrame -> DataFrame
derive name expr df = DataFrame ((columns df) ++ [(name, interpret expr df)])
