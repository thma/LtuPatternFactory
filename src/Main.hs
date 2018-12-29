module Main where
import           AbstractFactory
import           Adapter
import           Builder
import           Coerce
import           Composite
import           DependencyInjection
import           Iterator
import           JsonPersistence
import           NullObject
import           Pipeline
import           Singleton
import           Strategy
import           TemplateMethod
import           Visitor
import           Interpreter

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
