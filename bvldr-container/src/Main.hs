{-# LANGUAGE OverloadedStrings #-}
import Container.Docker (create)

main :: IO ()
main = create "ubuntu" "echo hello" >> putStrLn "hello world"