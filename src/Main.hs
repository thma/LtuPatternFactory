module Main where
import Strategy  
import Singleton
import Pipeline
import Visitor

main :: IO ()
main = do
  putStrLn "have fun with Lambda the ultimate Pattern Factory\n"
  strategyDemo
  singletonDemo
  pipelineDemo
  visitorDemo

