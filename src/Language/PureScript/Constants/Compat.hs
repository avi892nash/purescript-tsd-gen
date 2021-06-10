{-# LANGUAGE CPP #-}
module Language.PureScript.Constants.Compat
  ( module C
  ) where
#if defined(MIN_VERSION_purescript_ast) || defined(MIN_VERSION_purescript_cst)
import           Language.PureScript.Constants.Prim as C
#else
import           Language.PureScript.Constants as C
#endif
