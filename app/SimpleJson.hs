{-# LANGUAGE LambdaCase #-}
module SimpleJson(
    JValue(..)
    ,renderJValue
    ,putJValue) where
import Data.List (intercalate)

-- Json 解析库
data JValue = JString String
            | JNumber Int
            | JBool Bool
            | JNull -- 这是个什么处理方法？
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

renderJValue :: JValue -> String
renderJValue = \case
  JString s   -> show s
  JNumber n   -> show n
  JBool b     -> if b then "true" else "false"
  JNull       -> "null"
  JObject o   -> "{" ++ renderPairs o ++ "}"
  JArray a    -> "[" ++ renderValues a ++ "]"
  where
    renderPairs = intercalate ", " . map renderPair
    renderPair (k, v) = show k ++ ": " ++ renderJValue v
    renderValues = intercalate ", " . map renderJValue

-- 在终端以对应类型显示渲染成为字符串的 Json 值
putJValue :: JValue -> IO ()
putJValue s = putStrLn $ renderJValue s

-- 剩下的好像就是写打印和生成文档相关的内容了，很有趣，但是我先放一下。