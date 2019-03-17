module Main where

import           AbstractFactory
import           Adapter
import           Builder
import           Coerce
import           Composite
import           DependencyInjection
import           HigherOrder
import           Infinity
import           Interpreter
import           Iterator
import           JsonPersistence
import           NullObject
import           Pipeline
import           Singleton
import           Strategy
import           TemplateMethod
import           Visitor
import           MapReduce
import           MiniPascal
import           AspectPascal

main :: IO ()
main = do
  putStrLn "have fun with Lambda the ultimate Pattern Factory\n"
  strategyDemo
  singletonDemo
  pipelineDemo
  compositeDemo
  visitorDemo
  adapterDemo
  builderDemo
  templateMethodDemo
  nullObjectDemo
  iteratorDemo
  abstractFactoryDemo
  jsonPersistenceDemo
  demoDI
  interpreterDemo
  infinityDemo
  mapReduceDemo
  miniPascalDemo
  aspectPascalDemo
  higherOrderDemo