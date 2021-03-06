{-# LANGUAGE OverloadedStrings #-}

module Compiler.Translator
  ( translate'
  , getDependencies
  ) where

import           AST
import           Compiler.Translator.Type
import           Control.Exception
import           Control.Monad                                 (filterM, join)
import           Control.Monad.Identity                        (Identity)
import           Control.Monad.Reader                          (asks)
import           Control.Monad.State                           (get, gets, put)
import           Control.Monad.Writer                          (WriterT,
                                                                mapWriterT,
                                                                tell)
import           Data.List                                     (intercalate)
import qualified Data.Map                                      as Map
import           Data.Maybe                                    as M

import           Compiler.Translator.AExpr
import           Compiler.Translator.BExpr
import           Compiler.Translator.Statement
import           Compiler.Translator.Statement.ClassTranslator

getDependencies =
  Dependencies
    { aExprTranslatorGetter = aExprTranslator
    , bExprTranslatorGetter = bExprTranslator
    , stmtTranslatorGetter = translateStatement
    , fnStmtTranslatorGetter = functionStmtTranslator
    , classStmtTranslatorGetter = classStmtTranslator
    }

translate' :: [Imported] -> Translator
translate' files = do
  files' <- filter (not . null) . concat <$> mapM translateFile files
  imports <- gets toImport
  return $ imports ++ files'

translateFile :: Imported -> Translator
translateFile (IFile fName _ (AST stmts)) = do
  declarations <- declareFunctions stmts
  imports <- concat <$> mapM importTranslator (filterImports stmts)
  restOfCode <- mapM translateStatement stmts
  return . wrapInsideNamespace fName . concat $ imports : declarations : restOfCode

declareFunctions :: [Stmt] -> Translator
declareFunctions = return . map trans . filter cond
  where
    trans (Function _ n _ (VFn t cm) args _) =
      typeToString (VFnNamed (n ++ "(" ++ argumentsTranslator args ++ ")") t cm) ++ ";\n"
    trans (Function _ n _ t args _) = typeToString t ++ " " ++ n ++ "(" ++ argumentsTranslator args ++ ");\n"
    trans (ClassExpr _ n _ _ _) = "class " ++ n ++ ";\n"
    cond Function {}  = True
    cond ClassExpr {} = True
    cond _            = False

filterImports = filter cond
  where
    cond Import {} = True
    cond _         = False

wrapInsideNamespace "main" res = res
wrapInsideNamespace n res      = ("namespace " ++ n ++ "{\n") : res ++ ["}\n"]

-- TRANSLATORS
translateStatement :: Stmt -> Translator
translateStatement s =
  case s of
    t@Import {}                  -> return [] --importTranslator t
    t@LinkPath {}                -> linkPathTranslator t
    t@Function {}                -> functionTranslator t
    t@Assign {}                  -> assignTranslator t
    t@ClassExpr {}               -> classTranslator t
    t@Skip {}                    -> return []
    t@NativeFunction {}          -> nativeTranslator t
    t@NativeClass {}             -> return []
    t@NativeAssignDeclaration {} -> return []

functionStmtTranslator :: FunctionStmt -> Translator
functionStmtTranslator s =
  case s of
    AssignFn o a b c -> assignTranslator $ Assign o a b c
    WhileFn _ a b    -> whileTranslator a b functionStmtTranslator
    ForFn _ a b c    -> forTranslator a b c functionStmtTranslator
    a@IfFn {}        -> ifTranslator a
    a@ReturnFn {}    -> returnExprTranslator a
    a@OtherFn {}     -> casualExprTranslator a
    a@Break {}       -> breakTranslator a
    Pass             -> return [""]

classStmtTranslator :: ClassStmt -> Translator
classStmtTranslator c =
  case c of
    t@ClassAssign {}  -> classAssignTranslator t
    t@Method {}       -> methodTranslator t
    t@Constructor {}  -> constTranslator t
    t@NativeMethod {} -> abstractFunctionTranslator t

-- (right_in, before, after)
aExprTranslator :: AExpr -> Translator
aExprTranslator expr =
  case expr of
    e@TypedVar {}        -> varTranslator e
    e@ScopeMark {}       -> scopeMarkTranslator e
    Var _ a _ b c        -> varTranslator (TypedVar (VName a) VAuto b c)
    e@Optional {}        -> optionalTranslator e
    e@ABracket {}        -> bracketTranslator e
    e@ABracketApply {}   -> bracketApplyTranslator e
    IntConst _ i         -> return . return $ show i
    FloatConst _ f       -> return . return $ show f
    StringVal _ s        -> return . return $ show s
    e@NativePtrInput {}  -> nativePtrInputWrapper e
    e@NativePtrRes {}    -> nativePtrResWrapper e
    e@TypedListVar {}    -> listVarTranslator e
    e@Range {}           -> return ["\"TODO\""]
    e@LambdaFn {}        -> lambdaTranslator e
    e@Neg {}             -> aExprNegTranslator e
    e@ABinary {}         -> binaryTranslator e
    TypedABinary _ a b c -> binaryTranslator $ ABinary a b c
    ABool bExpr          -> bExprTranslator bExpr
    e@Null {}            -> return ["nullptr"]
    a                    -> throw $ UnsupportedTypeException (show a)

--    Nop           -> return ["nullptr"]
bExprTranslator :: BExpr -> Translator
bExprTranslator expr =
  case expr of
    BoolConst a ->
      return $
      if a
        then ["true"]
        else ["false"]
    BoolVar a -> aExprTranslator a
    e@Not {} -> negationTranslator e
    e@BBinary {} -> bBinaryTranslator e
    e@RBinary {} -> rBinaryTranslator e
