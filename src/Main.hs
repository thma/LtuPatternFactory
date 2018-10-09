module Main where
import Strategy  
import Singleton
import Pipeline
import Visitor
import Adapter
import Builder

main :: IO ()
main = do
  putStrLn "have fun with Lambda the ultimate Pattern Factory\n"
  strategyDemo
  singletonDemo
  pipelineDemo
  visitorDemo
  adapterDemo
  builderDemo

