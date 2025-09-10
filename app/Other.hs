module Other where

import Data.Char (digitToInt)

-- 定义一个类型类
class BasicEq a where
    isEq :: a -> a -> Bool
    isEq x y = not (isNotEq x y)
    
    isNotEq :: a -> a -> Bool
    isNotEq x y = not (isEq x y)

data TestType = ValueA | ValueB deriving (Eq, Ord)

instance BasicEq TestType where
    isEq ValueA ValueA = True
    isEq ValueB ValueB = True
    isEq _ _ = False
        
-- 接收一个输入文件 inputFile 并进行 function 处理，将结果保存在 outputFile
processFile :: (String -> String) -> FilePath -> FilePath -> IO ()
processFile function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

-- 将字符串转为整数
asInt :: String -> Int
asInt = loop 0

loop :: Int -> String -> Int
loop = foldl (\ acc x -> acc * 10 + digitToInt x)
