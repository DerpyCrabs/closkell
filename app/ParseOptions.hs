module ParseOptions (parseOptions, Options (..), CommonOptions (..), Input (..), EvalOptions (..), CompileOptions (..), ServeOptions (..)) where

import Options.Applicative

data Options = Eval EvalOptions | Compile CompileOptions | Serve ServeOptions

data CommonOptions = CommonOptions {optWithoutTypecheck :: Bool, optInput :: Input}

data Input = FileInput String | StdInput

data EvalOptions = EvalOptions {eoCommon :: CommonOptions, eoUsingNode :: Bool, eoArgs :: [String]}

data CompileOptions = CompileOptions {coCommon :: CommonOptions, coJSOutput :: Maybe String, coCLSKOutput :: Maybe String}

newtype ServeOptions = ServeOptions {sPort :: Int}

withoutTypecheck :: Parser Bool
withoutTypecheck = switch (long "without-typecheck" <> help "Disables typechecking")

usingNode :: Parser Bool
usingNode = switch (long "using-node" <> help "Evaluate generated from source JS using Node.JS")

fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILENAME"
          <> help "Input file"
      )

jsOutput :: Parser String
jsOutput =
  strOption
    ( long "emit-js"
        <> metavar "FILENAME"
        <> help "Output generated JS source to file"
    )

clskOutput :: Parser String
clskOutput =
  strOption
    ( long "emit-clsk"
        <> metavar "FILENAME"
        <> help "Output compiled clsk source to file"
    )

stdInput :: Parser Input
stdInput =
  flag'
    StdInput
    ( long "stdin"
        <> help "Read from stdin"
    )

input :: Parser Input
input = fileInput <|> stdInput

port :: Parser Int
port = option auto (long "port" <> short 'p' <> help "Where to host backend server" <> showDefault <> value 8081 <> metavar "PORT")

commonOptions :: Parser CommonOptions
commonOptions = CommonOptions <$> withoutTypecheck <*> input

arg :: Parser String
arg = argument str (metavar "...ARGS")

evalOptions :: Parser EvalOptions
evalOptions = EvalOptions <$> commonOptions <*> usingNode <*> many arg

compileOptions :: Parser CompileOptions
compileOptions = CompileOptions <$> commonOptions <*> optional jsOutput <*> optional clskOutput

serveOptions :: Parser ServeOptions
serveOptions = ServeOptions <$> port

options :: Parser Options
options =
  subparser
    ( command "eval" (info ((Eval <$> evalOptions) <**> helper) (progDesc "Evaluate input source"))
        <> command "compile" (info ((Compile <$> compileOptions) <**> helper) (progDesc "Compile input source"))
        <> command "serve" (info ((Serve <$> serveOptions) <**> helper) (progDesc "Serve debugger backend"))
    )

optionsInfo :: ParserInfo Options
optionsInfo = info (options <**> helper) (fullDesc <> header "Compiler of Closkell language")

parseOptions :: IO Options
parseOptions = execParser optionsInfo
