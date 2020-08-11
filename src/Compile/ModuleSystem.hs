module Compile.ModuleSystem (moduleSystem) where

import Control.Monad.Except
import Types
import Data.Value
import Parse

moduleSystem (List _ (Atom _ "executable":loadExprs):exprs)  = (:[]) <$> handleLoads loadExprs (func "do" exprs)
moduleSystem exprs  = moduleSystem $ func "executable" []:exprs

data ModuleInfo = ModuleInfo {
  prefix:: String,
  name:: String,
  exports:: [(String, Maybe String)],
  value:: LispVal
}

handleLoads :: [LispVal] -> LispVal -> IOThrowsError LispVal
handleLoads [] inner = return inner
handleLoads loadExprs inner = do
  modules <- mapM getModuleInfo loadExprs
  let moduleBindings = moduleBinding <$> modules
  let innerBindings = list (concat [[atom "let"], concat $ moduleExportsBindings <$> modules, [inner]])
  return $ list (concat [[atom "let"], moduleBindings, [innerBindings]]) 
  where
    getModuleInfo :: LispVal -> IOThrowsError ModuleInfo
    getModuleInfo (List _ [Atom _ "load", String loadPath, Atom _ "unqualified"]) = do
      info <- loadModule loadPath
      return $ info {prefix = ""}
    getModuleInfo (List _ [Atom _ "load", String loadPath]) = loadModule loadPath
    getModuleInfo (List _ [Atom _ "load", String loadPath, Atom _ "as", Atom _ prefix]) = do 
      info <- loadModule loadPath
      return $ info {prefix = prefix}
    moduleBinding (ModuleInfo _ name _ value) = list [atom $ "##module." ++ name, value]
    moduleExportsBindings (ModuleInfo prefix name exports _) = bindExports prefix name exports
      where
        bindExports :: String -> String -> [(String, Maybe String)] -> [LispVal]
        bindExports prefix name ((export, Nothing):exports) = bindExports prefix name ((export, Just export):exports)
        bindExports prefix name ((export, Just alias):exports) = let
          bindName = atom (prefix ++ "." ++ alias)
          bindValue = func "get" [String export, atom ("##module." ++ name)]
          in list [bindName, bindValue]:bindExports prefix name exports
        bindExports _ _ [] = []
        

loadModule :: String -> IOThrowsError ModuleInfo
loadModule path = do
  loadedModule <- load path
  getModuleInfo loadedModule
  where
    getModuleInfo (List _ (Atom _ "module" : String name : List _ exports : loadExprs): exprs) = do
      value <- handleLoads loadExprs (list (concat [[atom "let"], exprs, [list $ exportsList name (parseExports exports)]]))
      return ModuleInfo {prefix = name, name = name, exports = parseExports exports, value = value}
    parseExports ((Atom _ export):exports) = (export, Nothing):parseExports exports
    parseExports ((List _ [Atom _ export, Atom _ "as", Atom _ alias]):exports) = (export, Just alias):parseExports exports
    parseExports [] = []
    exportsList name ((export, _):exports) = [String export, atom export] ++ exportsList name exports
    