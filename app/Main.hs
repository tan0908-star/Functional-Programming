{-# OPTIONS_GHC -Wno-unused-imports #-}
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Other
import SimpleJson
import File 
import System.FilePath (takeExtension)
-- import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  getRecursiveContents path >>= mapM_ putStrLn . filter (\file -> takeExtension file == fileLastName) -- 这东西最好只有两个参数
    where
      path = ".."
      fileLastName = ".cabal"