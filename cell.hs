{-# LANGUAGE DeriveGeneric #-}

module Cell (
    Cell (..),
    GCell (..),

    Operation (..),
    Expression (..),
    Key
    ) where

import GHC.Generics

type Key = (Char, Char)

data GCell a =
    GCell {
        value :: a
    }
    deriving (Read, Show, Generic)

data Operation
    = Addition
    | Multiplication
    | Average
    deriving (Read, Show, Generic)

data Expression = 
    Expression {
        references :: [Key],
        op :: Operation
    }
    deriving (Read, Show, Generic)

data Cell
    = CellDouble (GCell Double)
    | CellString (GCell String)
    | CellExp (GCell Expression)
    deriving (Read, Show, Generic)