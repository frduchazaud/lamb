{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import           Data.Text
import qualified Data.Text.IO                  as T

import           Flow

someFunc :: IO ()
someFunc = "someFunc" |> T.putStrLn

-- FIXME: add specialized types for : type
data Id 
    = Rdr String 
    | Qual String 
    | Idx Int

data Expr ctx
    = LitInt Int ctx
    | LitStr String ctx
    | Val Id ctx
    | App { fun :: Id, args :: [Expr ctx], ctx :: ctx }

data Syntax ctx 
    = Module { name :: Id, exposing :: [Id] }
    | Import { modu :: Id, as :: Maybe Id, incl :: [Id], excl :: [Id] }
    | Bind { name :: Id, expr :: Expr ctx }


data Ty ctx
    = Alias { alias :: Id, ty :: Ty ctx }
    | SumTy [(Id, Ty ctx, ctx)] ctx
    | TupleTy [(Ty ctx, ctx)] ctx
    | RecordTy [(Id, Ty ctx, ctx)] ctx




