--------------------------------------------------------------------------------
--- jonesjon 932709446 ---------------------------------------------------------
--------------------------------------------------------------------------------

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test t w r = case t of
             Not t    -> not (test t w r)
             Facing c -> c == (getFacing r)
             Clear d  -> isClear (relativePos d r) w
             Beeper   -> hasBeeper (getPos r) w
             Empty    -> isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r    = Done r
stmt PickBeeper _ w r    = let p = getPos r
                           in if hasBeeper p w
                                 then OK (decBeeper p w) (incBag r)
                                 else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper _ w r     = let p = getPos r
                           in if not (isEmpty r)
                                 then OK (incBeeper p w) (decBag r)
                                 else Error ("No beeper to put.")
stmt Move _ w r          = let p = relativePos Front r
                           in if isClear p w
                                 then OK w (setPos p r)
                                 else Error ("Blocked at: " ++ show p)
stmt (Turn d) _ w r      = OK w (updateFacing (cardTurn d) r)
stmt (Block s) d w r     = case s of
                           (m:[]) -> stmt m d w r
                           (m:sl) -> let result = stmt m d w r
                                     in onOK (stmt (Block sl) d) result
stmt (If t s1 s2) d w r  = if test t w r
                               then stmt s1 d w r
                               else stmt s2 d w r
stmt (Call m) d w r      = case lookup m d of
                           Just s -> stmt s d w r
                           Nothing -> Error ("Undefined macro: " ++ show m)
stmt (Iterate n s) d w r = case n of
                           1 -> stmt s d w r
                           _ -> let result = stmt s d w r
                                in onOK (stmt (Iterate (n - 1) s) d) result
stmt (While t s) d w r   = if test t w r
                             then onOK (stmt (While t s) d) (stmt s d w r)
                             else (OK w r)

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
