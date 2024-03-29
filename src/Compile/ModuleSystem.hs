module Compile.ModuleSystem (moduleSystem) where

import Data.Value
import Parse
import Types

moduleSystem :: [Value] -> IOThrowsError Value
moduleSystem (Call (Atom _ "executable" : loadExprs) : [expr]) = handleLoads loadExprs expr
moduleSystem (Call (Atom _ "executable" : loadExprs) : exprs) = handleLoads loadExprs (func "do" exprs)
moduleSystem exprs = moduleSystem $ func "executable" [] : exprs

data ModuleInfo = ModuleInfo
  { prefix :: String,
    name :: String,
    exports :: [(String, Maybe String)],
    value :: Value
  }

handleLoads :: [Value] -> Value -> IOThrowsError Value
handleLoads [] inner = return inner
handleLoads loadExprs inner = do
  modules <- mapM getModuleInfo loadExprs
  let moduleBindings = moduleBinding <$> modules
  let innerBindings = Call (concat [[atom "let"], concat $ moduleExportsBindings <$> modules, [inner]])
  return $ Call (concat [[atom "let"], moduleBindings, [innerBindings]])
  where
    getModuleInfo :: Value -> IOThrowsError ModuleInfo
    getModuleInfo (List _ [Atom _ "load", String loadPath, Atom _ "unqualified"]) = do
      info <- loadModule loadPath
      return $ info {prefix = ""}
    getModuleInfo (List _ [Atom _ "load", String loadPath]) = loadModule loadPath
    getModuleInfo (List _ [Atom _ "load", String loadPath, Atom _ "as", Atom _ prefix]) = do
      info <- loadModule loadPath
      return $ info {prefix = prefix}
    moduleBinding (ModuleInfo _ name _ value) = list [atom $ "$$module." ++ name, value]
    moduleExportsBindings (ModuleInfo prefix name exports _) = bindExports prefix name exports
      where
        bindExports :: String -> String -> [(String, Maybe String)] -> [Value]
        bindExports prefix name ((export, Nothing) : exports) = bindExports prefix name ((export, Just export) : exports)
        bindExports prefix name ((export, Just alias) : exports) =
          let bindName =
                if prefix /= ""
                  then atom (prefix ++ "." ++ alias)
                  else atom (prefix ++ alias)
              bindValue = func "get" [String export, atom ("$$module." ++ name)]
           in list [bindName, bindValue] : bindExports prefix name exports
        bindExports _ _ [] = []

loadModule :: String -> IOThrowsError ModuleInfo
loadModule path = do
  loadedModule <- load path
  getModuleInfo loadedModule
  where
    getModuleInfo (Call (Atom _ "module" : String name : List _ exports : loadExprs) : exprs) = do
      value <- handleLoads loadExprs (Call (concat [[atom "let"], exprs, [Map $ exportsList name (parseExports exports)]]))
      return ModuleInfo {prefix = name, name = name, exports = parseExports exports, value = value}
    parseExports ((Atom _ export) : exports) = (export, Nothing) : parseExports exports
    parseExports ((List _ [Atom _ export, Atom _ "as", Atom _ alias]) : exports) = (export, Just alias) : parseExports exports
    parseExports [] = []
    exportsList name ((export, _) : exports) = (String export, func "unquote" [atom export]) : exportsList name exports
    exportsList _ [] = []
