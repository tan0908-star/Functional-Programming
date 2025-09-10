{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module File (getRecursiveContents
            ) where

-- file: ch09/RecursiveContents.hs
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents, Permissions)
import System.FilePath ((</>))

-- | 读取 path 路径下的文件后缀名为 fileLastName 并列举出其文件路径
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names  -- 注意这里的缩进：必须与do块内其他语句对齐
    paths <- forM properNames $ \name -> do
        let path = topdir </> name  -- 这里的缩进：必须在forM的do块内缩进
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]  -- else子句必须与if的then对齐
    return (concat paths)  -- 与do块内其他语句对齐



type InfoP a = FilePath -> Permissions -> Maybe Integer -> a
sizeP :: InfoP Integer
sizeP _ _ (Just size) = size
sizeP _ _ Nothing = -1
-- 这种方法不知道比 If else 优雅多少
