module Compiler.Translator.Type where

import Control.Monad.Writer (WriterT)
import Control.Monad.Identity (Identity)

type Translator = WriterT [String] Identity [String]

