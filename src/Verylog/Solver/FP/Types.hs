{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Verylog.Solver.FP.Types where

import           Control.Monad.Reader
import           Control.Lens
import           Text.PrettyPrint
import           Text.Printf
import qualified Data.HashSet               as S
import qualified Data.HashMap.Strict        as M

import           Verylog.Language.Types hiding (St, ufs)
import           Verylog.Solver.Common

type SMTVar = String

data FQBind = FQBind { bindId   :: Int
                     , bindName :: Id
                     , bindType :: String
                     , bindRef  :: String
                     }

data InvFun = InvFun { invFunName  :: Id
                     , invFunArity :: Int
                     }

data UFConst = UFConst { ufConstName  :: Id
                       , ufConstArity :: Int
                       }

data FPSt = FPSt { _constraints :: [Inv]
                 , _invs        :: [InvFun]
                 , _ufs         :: [UFConst]
                 , _binds       :: M.HashMap Id FQBind
                 }

makeLenses ''FPSt

type R    = Reader FPSt
type RDs  = Reader FPSt [Doc]
type Pr a = a -> R Doc

instance PPrint FPSt where
  toDoc st = runReader comp st
    where
      comp :: R Doc
      comp = do let qs = qualifiers
                bs <- printBinds
                cs <- makeConstraints
                wf <- makeWFConstraints
                return $ vcat $ punctuate nl $ qs : (vcat <$> [bs, cs, wf])

nl = text "\n"

qualifiers :: Doc
qualifiers = vcat $ text <$> [ "qualif Eq(x:int, y:int) : (x = y)"
                             ]
printBinds :: RDs
printBinds = do
  bs <- views binds M.elems
  mapM printBind bs

printBind :: Pr FQBind
printBind (FQBind{..}) = 
  return $
  text "bind" <+> int bindId <+> text bindName <+> colon <+>
  typeDef (text bindType) (text bindRef)

printExpr :: Pr Expr
printExpr _ = return empty

makeConstraints :: RDs
makeConstraints = return []

makeWFConstraints :: RDs
makeWFConstraints = return []

-- instance PPrint Inv where
--   toDoc (Prop{..}) = vcat [ text "constraint:"
--                           , nest 2 body
--                           ]
--     where
--       body = vcat [ text "env" <+> brackets empty
--                   , text "lhs" <+> typeDef constraintLhs
--                   , text "rhs" <+> typeDef constraintRhs
--                   , text "id" <+> int constraintId <+> text "tag []"
--                   ]
--     -- parens $ text "assert" <+> chk
--     -- where
--     --   chk     = forallArgs (BinOp IMPLIES invBody (Structure invName invArgs))
--     --   invName = printf "%s%d" invPred invId

--   toDoc (Inv{..}) = parens $ text "assert" <+> chk
--     where
--       chk = forallArgs (BinOp IMPLIES propL propR)

-- instance PPrint Expr where
--   toDoc (Boolean True)   = text "true"
--   toDoc (Boolean False)  = text "false"
--   toDoc (Number n)       = text $ show n
--   toDoc (Var x)          = text x
--   toDoc (Ands [])        = text "true"
--   toDoc (Ands as)        = psep (text "and" : (toDoc <$> as))
--   toDoc (Ite{..})        = parens $ text "ite" <+> cat [ ptoDoc cnd
--                                                        , ptoDoc expThen
--                                                        , ptoDoc expElse
--                                                        ]
--   toDoc (Structure f as) = parens $ hsep (text <$> (f:as))
--   toDoc (BinOp{..})      = let op = case bOp of
--                                       IMPLIES -> "=>"
--                                       EQU     -> "="
--                                       LE      -> "<="
--                                       GE      -> ">="
--                                       PLUS    -> "+"
--                                       AND     -> "and"
--                                       OR      -> "or"
--                            in psep [ text op
--                                    , toDoc expL
--                                    , toDoc expR
--                                    ]
--   toDoc (UFCheck{..}) = psep [ text "and"
--                              , (sel (fst ufNames) (fst <$> ufArgs))
--                              , (sel (snd ufNames) (snd <$> ufArgs))
--                              ]
--     where
--       sel f args = psep [ text "="
--                         , toDoc f
--                         , psep (text "select" : text ufFunc : (toDoc <$> args))
--                         ]

-- instance PPrint InvFun where
--   toDoc (InvFun{..}) = parens $ hsep [ text "declare-fun"
--                                      , text invFunName
--                                      , parens $ hsep (replicate invFunArity (text "Int"))
--                                      , text "Bool"
--                                      ]

-- instance PPrint UFConst where
--   toDoc (UFConst{..}) = parens $ hsep [ text "declare-const"
--                                       , text ufConstName
--                                       , parens $ hsep $
--                                         text "Array" : (replicate (ufConstArity+1) (text "Int"))
--                                       ]

-- ptoDoc :: PPrint a => a -> Doc
-- ptoDoc = parens . toDoc

-- psep :: [Doc] -> Doc
-- psep = parens . sep

-- pcat :: [Doc] -> Doc
-- pcat = parens . cat

-- type S = State (S.HashSet Id)

-- -- TODO : this is probably not quite right
-- --------------------------------------------------------------------------------
-- allVars :: Expr -> [Id]
-- --------------------------------------------------------------------------------
-- allVars s = evalState comp S.empty
--   where
--     comp = f s >> get >>= return . S.toList

--     f                  :: Expr -> S ()
--     f (BinOp{..})      = sequence_ (f <$> [expL, expR])
--     f (Ands es)        = sequence_ (f <$> es)
--     f (Ite{..})        = sequence_ (f <$> [cnd, expThen, expElse])
--     f (Structure _ vs) = modify (S.union (S.fromList vs))
--     f (Var v)          = modify (S.insert v)
--     f (Boolean _)      = return ()
--     f (Number _)       = return ()
--     f (UFCheck{..})    = sequence_ (f <$> as ++ t2l ufNames)
--       where
--         as        = concatMap t2l ufArgs
--         t2l (x,y) = [x,y]

-- instance Show Inv where
--   show = pprint
-- instance Show InvFun where
--   show = pprint
-- instance Show UFConst where
--   show = pprint

typeDef :: Doc -> Doc -> Doc
typeDef ty ref = 
    braces (text "v" <> colon <+> ty <+> text "|" <+> ref)

instance Show FPSt where
  show = pprint
