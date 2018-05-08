module MyTest where

import Prelude hiding (Num)
import Data.List

-- Define the data types and constructors for the semantics

type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Down
          | Up
          deriving(Eq, Show)

data Expr = Name Var
          | Lit Num
          | Add Expr Expr
          deriving(Eq, Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving(Eq, Show)

----------------------------------------------------
-- Define a miniLogo macro line(x1,y1,x2,y2)
-- Concrete Syntax:
-- define line (x1, y1, x2, y2) {
--  pen up; move(x1, y1);
--  pen down; move(x2, y2);
--  }
----------------------------------------------------


line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"]
       [Pen Up, Move (Name "x1") (Name "y1"),
       Pen Down, Move (Name "x2") (Name "y2")]

--------------------------------------------------
-- Define a macro nix (x,y,w,h) that draws an x
-- Concrete Syntax
-- define nix (x, y, w, h) {
--  call line (x, y, x + w, y + h);
--  call line (x + w, y, x, y + h);
--  }
---------------------------------------------------

nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
      [Call "line" [Name "x", Name "y", Add (Name "x") (Name "w"), Add (Name "y") (Name "h")],
      Call "line" [Add (Name "x") (Name "w"), Name "y", Name "x", Add (Name "y") (Name "h")]]

------------------------------------------------------

steps :: Int -> Prog
steps 0 = [Pen Up, Move (Lit 0) (Lit 0), Pen Down]
steps n = steps (n-1) ++ [Move (Lit (n - 1)) (Lit n), Move (Lit n) (Lit n)]


macros :: Prog -> [Macro]
macros []     = []
macros (c:cs) = case c of
                Define m _ _ -> m : macros cs
                _ -> macros cs

pretty :: Prog -> String
pretty []     = ""
pretty (c:cs) = case c of
                Pen m        -> "Pen " ++ show(m) ++ ";\n" ++ pretty cs
                Move x y     -> "Move (" ++ prettyExpr x ++ "," ++ prettyExpr y ++ ");\n" ++ pretty cs
                Define m v p -> "Define " ++ m ++ " (" ++ intercalate "," v ++ ") {\n" ++ pretty p ++ "}\n" ++ pretty cs
                Call m es    -> "Call " ++ m ++ " (" ++ intercalate "," (map prettyExpr es) ++ ");\n" ++ pretty cs

prettyExpr :: Expr -> String
prettyExpr e = case e of
               Name v -> v
               Lit n -> show(n)
               Add x y -> prettyExpr x ++ " + " ++ prettyExpr y
