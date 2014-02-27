module Scryptic (
  module M,
  defaultScryptOpts,
) where

import Scryptic.Monad as M
import Scryptic.Types as M
import Scryptic.Parser as M
import Scryptic.Runtime as M
import Scryptic.RuntimeOptions

{-
runScrypt :: Scryptic -> Text -> IO ()
runScrypt scryptic input = do
    case parseScript scryptic input of
        Left err -> error $ "scryptic: invalid input, " ++ show err
        Right akt -> void $ async akt
        -}
