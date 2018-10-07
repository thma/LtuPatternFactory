# Lambda the Ultimate Pattern Factory

In the Early Nineties my first programming languages were Lisp, Scheme, and ML. When I later started to work in OO languages like C++ and Java I noticed that idioms that were standard vocabulary in functional programming (fp) were not so easy to achieve and required sophisticated structures. Books like [Design Patterns: Elements of Reusable Object-Oriented Software](https://en.wikipedia.org/wiki/Design_Patterns) were a great starting point to reason about those structures. One of my earliest findings was that several GoF-Patterns had a stark resemblance of structures that are built into in functional languages: for instance the strategy pattern corresponds to higher order functions in fp languages (more details see [below](#strategy)).

In recent years my interest in functional languages (and particularly [Haskell](https://www.haskell.org/)) was reignited when I sensed a more widespread acceptance of fp concepts and languages in the software industry. 

While re-reading through the [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia) (or the original article: https://wiki.haskell.org/wikiupload/8/85/TMR-Issue13.pdf) I had the impression that the Haskell typeclass library is a rich source of powerful and elegant concepts, tools and patterns. I thought it would be a good exercise to study the structure of software design-patterns (https://en.wikipedia.org/wiki/Software_design_pattern#Classification_and_list) and to find out how they map to concepts in functional programming and Haskell and its typeclass library in particular.

By searching the web I found some blog entries studying specific patterns, but I did not come across any text covering larger sets of patterns. So as it seems that nobody did this kind of comparative study yet I found it worthy to spend some time on it and write down all my findings on the subject.


# The Patternopedia
A walk through the Typeclassopedia from a design pattern perspective.
(There is even a [Scala Typeclassopedia](https://github.com/tel/scala-typeclassopedia) available).

## Strategy
> "The strategy pattern [...] is a behavioral software design pattern that enables selecting an algorithm at runtime. Instead of implementing a single algorithm directly, code receives run-time instructions as to which in a family of algorithms to use"

![strategy pattern](https://upload.wikimedia.org/wikipedia/commons/4/45/W3sDesign_Strategy_Design_Pattern_UML.jpg)

> "In the above UML class diagram, the `Context` class doesn't implement an algorithm directly. Instead, `Context` refers to the  `Strategy` interface for performing an algorithm (`strategy.algorithm()`), which makes `Context` independent of how an algorithm is implemented. The `Strategy1` and `Strategy2` classes implement the `Strategy` interface, that is, implement (encapsulate) an algorithm." 
>(quoted from https://en.wikipedia.org/wiki/Strategy_pattern)

* in C a strategy would be modelled as a function pointer that can be used to dispatch calls to different functions.
* In an OO language like Java a strategy would be modelled as a single strategy-method interface that would be implemented by different strategy classes that provide implementations of the strategy method.
* in functional programming a strategy is just a higher order function, that is a parameter of a function that has a function type.

```haskell
-- first we define two simple strategies that map numbers to numbers:
strategyId :: Num a => a -> a
strategyId n = n

strategyDouble :: Num a => a -> a
strategyDouble n = 2*n

-- now we define a context that applies a function of type Num a => a -> a to a list of a's:
context :: Num a => (a -> a) -> [a] -> [a]
context f l = map f l
-- according to the rules of currying this can be abbreviated to:
context = map
``` 
The `context` function uses higher order `map` function  (`map :: (a -> b) -> [a] -> [b]`) to apply the strategies to lists of numbers:
```haskell
ghci> context strategyId [1..10]
[1,2,3,4,5,6,7,8,9,10]
ghci> context strategyDouble [1..10]
[2,4,6,8,10,12,14,16,18,20]
```
Instead of map we could use just any other function that accepts a function of type `Num a => a -> a` and applies it in a given context.
In Haskell the application of a function in a computational context is generalized with the typeclass `Functor`:
```haskell
class  Functor f  where
    fmap :: (a -> b) -> f a -> f b
```
Actually `map` is the fmap implementation for the List Functor instance:
```haskell
instance Functor [] where
    fmap = map
```
Although it would be fair to say that the typeclass `Functor` captures the essential idea of the strategy pattern - namely the lifting into and the execution in a computational context of a function - the usage of higher order functions (or strategies) is of course not limited to `Functors` - we could use just any higher order function fitting our purpose. Other typeclasses like `Foldable` or `Traversable` can serve as helpful abstractions when dealing with typical use cases of applying variable strategies within a computational context. 



## Singleton
> "The singleton pattern is a software design pattern that restricts the instantiation of a class to one object. This is useful when exactly one object is needed to coordinate actions across the system." 
> (quoted from https://en.wikipedia.org/wiki/Singleton_pattern)

The singleton pattern ensures that multiple requests to a given object always return one and the same singleton instance.
In functional programming this semantics can be achieved by ```let```. 

```haskell
let singleton = someExpensiveComputation
in  mainComputation

--or in lambda notation:
(\singleton -> mainComputation) someExpensiveComputation
```

Via the `let`-Binding we can thread the singleton through arbitrary code in the `in` block. All occurences of `singleton` in the `mainComputation`will point to the same instance.

Typeclasses provide several tools to make this kind of threading more convenient or even to avoid explicit threading of instances.

### Using Pointed to create singletons

> "Given a Functor, the Pointed class represents the additional ability to put a value into a “default context.” Often, this corresponds to creating a container with exactly one element, but it is more general than that." 
> (quoted from the Typeclassopedia)

```haskell
class Functor f => Pointed f where
    pure :: a -> f a
```


### Using Applicative Functor for threading of singletons

The following code defines a simple expression evaluator:

```haskell
data Exp e = Var String
           | Val e
           | Add (Exp e) (Exp e)
           | Mul (Exp e) (Exp e)

-- the environment is a list of tupels mapping variable names to values of type e
type Env e = [(String, e)] 

-- a simple evaluator reducing expression to numbers
eval :: Num e => Exp e -> Env e -> e
eval (Var x)   env = fetch x env
eval (Val i)   env = i
eval (Add p q) env = eval p env + eval q env  
eval (Mul p q) env = eval p env * eval q env 
```
`eval` is a classic evaluator function that recursively evaluates sub-expression before applying `+` or `*`.
Note how the explicit `env`parameter is threaded through the recursive eval calls. This is needed to have the 
environment avalailable for variable lookup at any recursive call depth.

If we now bind `env` to a value as in the following snippet it is used as an imutable singleton within the recursive evaluation of `eval exp env`.

```haskell
main = do
  let exp = Mul (Add (Val 3) (Val 1)) 
                (Mul (Val 2) (Var "pi"))
      env = [("pi", pi)]
  print $ eval exp env
```
Experienced Haskellers will notice the ["eta-reduction smell"](https://wiki.haskell.org/Eta_conversion) in `eval (Var x) env = fetch x env` which hints at the possibilty to remove `env` as an explicit parameter. We can not do this right away as the other equations for `eval` do not allow eta-reduction. In order to do so we have to apply the combinators of the `Applicative Functor`:
```haskell
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative ((->) a) where
    pure        = const
    (<*>) f g x = f x (g x)
```
This `Applicative` allows us to rewrite `eval` as follows:
```haskell
eval :: Num e => Exp e -> Env e -> e
eval (Var x)   = fetch x
eval (Val i)   = pure i
eval (Add p q) = pure (+) <*> eval p  <*> eval q  
eval (Mul p q) = pure (*) <*> eval p  <*> eval q 
```
Any explicit handling of the variable `env` is now removed.
(I took this example from the classic paper [Applicative programming with effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.pdf) which details how `pure` and `<*>` correspond to the combinatory logic combinators `K` and `S`.)

## Pipeline (Pipes and Filters)
> In software engineering, a pipeline consists of a chain of processing elements (processes, threads, coroutines, functions, etc.), arranged so that the output of each element is the input of the next; the name is by analogy to a physical pipeline.
> (Quoted from: https://en.wikipedia.org/wiki/Pipeline_(software))

The concept of pipes and filters in Unix shell scripts is a typical example of the pipeline architecture pattern.
```bash
$ echo "hello world" | wc -w | xargs printf "%d*3\n" | bc -l
6
``` 
This works exactly as stated in the wikipedia definition of the pattern: the output of `echo "hello world"` is used as input for the next command `wc -w`. The ouptput of this command is then piped as input into `xargs printf "%d*3\n"` and so on.
On the first glance this might look like ordinary function composition. We could for instance come up with the following approximation in Haskell:
```haskell
((3 *) . length . words) "hello world"
6
```
But with this design we missed an important feature of the chain of shell commands: The commands do not work on elementary types like Strings or numbers but on input and output streams that are used to propagate the actual elementary data around. So we can't just send a String into the `wc` command as in `"hello world" | wc -w`. Instead we have to use `echo` to place the string into a stream that we can then use as input to the `wc` command:
```bash
$ echo "hello world" | wc -w
```
So we might say that `echo` *lifts* the String `"hello world"` into the stream context.
We can capture this behaviour in a functional program like this:
```haskell
-- The Stream type is a wrapper around an arbitrary payload type 'a'
newtype Stream a = Stream a deriving (Show)

-- echo lifts an item of type 'a' into the Stream context
echo :: a -> Stream a
echo = Stream

-- the 'andThen' operator used for chaining commands
infixl 7 |>
(|>) :: Stream a -> (a -> Stream b) -> Stream b
Stream x |> f = f x


-- echo and |> are used to create the actual pipeline
pipeline :: String -> Stream Int
pipeline str = 
  echo str |> echo . length . words |> echo . (3 *)
-- now executing the program in ghci repl:
ghci> pipeline "hello world"
Stream 6  
```

The `echo` function lifts any input into the `Stream` context:
```haskell
ghci> echo "hello world"
Stream "hello world"
```
The `|>` (pronounced as "andThen") does the function chaining:
```haskell
ghci> echo "hello world" |> echo . words
Stream ["hello","world"]
```
The result of `|>` is of type `Stream b` that's why we cannot just write `echo "hello world" |> words`. We have to use echo  to create a `Stream` output that can be digested by a subsequent `|>`.

The interplay of a Context type `Stream a` and the functions `echo` and `|>` is a well known pattern from functional languages: it's the legendary *Monad*. As the [Wikipedia article on the pipeline pattern](https://en.wikipedia.org/wiki/Pipeline_(software)) states:

> Pipes and filters can be viewed as a form of functional programming, using byte streams as data objects; more specifically, they can be seen as a particular form of monad for I/O. 

There is an interesting paper available elaborating on the monadic nature of Unix pipes: http://okmij.org/ftp/Computation/monadic-shell.html.

Here is the definition of the Monad typeclass in Haskell:
```Haskell
class Applicative m => Monad m where
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)  :: m a -> (a -> m b) -> m b

    -- | Inject a value into the monadic type.
    return :: a -> m a
    return = pure
```
By looking at the types of `>>=` and `return` it's easy to see the direct correspondence to `|>` and `echo` in the pipeline example above:
```haskell
    (|>)   :: Stream a -> (a -> Stream b) -> Stream b
    echo   :: a -> Stream a
```

Mhh, this is nice, but still looks a lot like ordinary composition of functions, just with the addition of a wrapper. 
In this simplified example that's true, because we have designed the `|>` operator to simply unwrap a value from the Stream and bind it to the formal parameter of the subsequent function:
```haskell
Stream x |> f = f x
```
But we are free to implement the `andThen` operator in any way that we seem fit as long we maintain the type signature and the [monad laws](https://en.wikipedia.org/wiki/Monad_%28functional_programming%29#Monad_laws).
So we could for instance change the semantic of `>>=` to keep a log along the execution pipeline.
In the following snippet I have extended `>>=` to increment a counter so that at the and of the pipeline we are informed about the number of invocations of `>>=`.

```haskell
-- The DeriveFunctor Language Pragma provides automatic derivation of Functor instances
{-# LANGUAGE DeriveFunctor #-}
-- the Stream type is extened by an Int that keeps the counter state
newtype Stream a = Stream (a, Int) deriving (Show, Functor)

-- as any Monad must be an Applicative we also have to instantiate Applicative
instance Applicative Stream where
  pure = return
  Stream (f, _) <*> r = fmap f r

-- our definition of the Stream Monad
instance Monad Stream where
  -- returns a Stream wrapping a tuple of the actual payload and an initial counter state of 0
  return a = Stream (a, 0)
  -- we define (>>=) to reach an incremented counter to the subsequent action
  m >>= k = let Stream(a, c1) = m
                next          = k a
                Stream(b, c2) = next
            in Stream (b, c1 + 1 + c2)

-- instead of echo and |> we now use the "official" monadic versions return and >>=
pipeline :: String -> Stream Int
pipeline str =
  return str >>= return . length . words >>= return . (3 *)

-- when using this in GHCI we receive a Stream wrapping a tuple of the result of the 
-- actual pipeline plus the result of the counter:
ghci> pipeline "hello world"
Stream (6,2)
```
What's noteworthy here is that Monads allow to make the mechanism of chaining functions explicit. We can define what `andThen` should mean in our pipeline by choosing a different Monad implementation.
So in a sense Monads could been called [programmable semicolons](http://book.realworldhaskell.org/read/monads.html#id642960)

There are several predefined Monads available in the Haskell curated libraries and it's also possible to combine their effects by making use of `MonadTransformers`.

## Monoid -- find Pattern

- 

## Visitor

> [...] the visitor design pattern is a way of separating an algorithm from an object structure on which it operates. A practical result of this separation is the ability to add new operations to existent object structures without modifying the structures. It is one way to follow the open/closed principle.
> (Quoted from [Wikipedia](https://en.wikipedia.org/wiki/Visitor_pattern))


In functional languages - and Haskell in particular - we have a whole armada of tools serving this purpose:
* higher order functions like map, fold, filter and all their variants allow to "visit" lists
* The Haskell typeclasses `Functor`, `Foldable`, `Traversable`, etc. provide a generic framework to allow visiting any algebraic datatype by just deriving one of these typeclasses.
### Using Foldable
```haskell
-- we are re-using the Exp data type from the Singleton example 
-- and transform it into a Foldable type:
instance Foldable Exp where
    foldMap f (Val x)   = f x
    foldMap f (Add x y) = foldMap f x `mappend` foldMap f y
    foldMap f (Mul x y) = foldMap f x `mappend` foldMap f y

filterF :: Foldable f => (a -> Bool) -> f a -> [a]
filterF p = foldMap (\a -> if p a then [a] else [])     

visitorDemo = do
    let exp = Mul (Add (Val 3) (Val 2)) 
                  (Mul (Val 4) (Val 6))
    putStr "size of exp: "
    print $ length exp
    putStrLn "filter even numbers from tree"
    print $ filterF even exp
```
By virtue of the instance declaration Exp becomes a Foldable instance an can be used with arbitrary functions defined on Foldable like `length` in the example.

`foldMap` can for example be used to write a filtering function `filterF`that collects all elements matching a predicate into a list. 

### Using Traversable

tbd.: Traversable Demo

# Beyond Typeclass patterns

- Chain of Responsibility: ADT + fumction pattern matching the ADT (at least the distpatch variant)

- Composite: ADT

- Template Method, type class

- Currying / Partial application

## Dependency Injection
> [...] Dependency injection is a technique whereby one object (or static method) supplies the dependencies of another object. A dependency is an object that can be used (a service). An injection is the passing of a dependency to a dependent object (a client) that would use it. The service is made part of the client's state. Passing the service to the client, rather than allowing a client to build or find the service, is the fundamental requirement of the pattern.
>
> This fundamental requirement means that using values (services) produced within the class from new or static methods is prohibited. The client should accept values passed in from outside. This allows the client to make acquiring dependencies someone else's problem. 
> (Quoted from [Wikipedia](https://en.wikipedia.org/wiki/Dependency_injection))

In functional languages this is simply achieved by binding a functions formal parameters to values.
See the following example where the function `generatePage :: (String -> Html) -> String -> Html` does not only require a String input but also a rendering function that does the actual conversion from text to Html.
```haskell
data Html = ...

generatePage :: (String -> Html) -> String -> Html
generatePage renderer text = renderer text

htmlRenderer :: String -> Html
htmlRenderer = ...
```
With partial application its even possible to form a closure that incorporates the rendering function:
```haskell
ghci> closure = generatePage htmlRenderer
:type closure
closure :: String -> Html
```



## Adapter
> "The adapter pattern is a software design pattern (also known as wrapper, an alternative naming shared with the decorator pattern) that allows the interface of an existing class to be used as another interface. It is often used to make existing classes work with others without modifying their source code." 
> (Quoted from https://en.wikipedia.org/wiki/Adapter_pattern)

An example is an adapter that converts the interface of a Document Object Model of an XML document into a tree structure that can be displayed. 

What does an adapter do? It translates a call to the adapter into a call of the adapted backend code. Which may also involve translation of the argument data.

Say we have some `backend` function that we want to provide with an adapter. we assume that `backend` has type `c -> d`:
```haskell
backend :: c -> d
```
Our adapter should be of type `a -> b`:
```haskell
adapter :: a -> b
```

In order to write this adapter we have to write two function. The first is:
```haskell
marshal :: a -> c
```
which translated the input argument of `adapter` into the correct type `c` that can be digested by the backend.
And the second function is:
```haskell
unmarshal :: d -> b
```
which translates the result of the `backend`function into the correct return type of `adapter`.
`adapter` will then look like follows:
```haskell
adapter :: a -> b
adapter = unmarshal . backend . marshal
```

# Table of patterns
|Name|Description|GOF|Functional Pendant|
|---|---|---|---|
|Abstract factory|Provide an interface for creating families of related or dependent objects without specifying their concrete classes.|Yes|x|
|Builder|Separate the construction of a complex object from its representation, allowing the same construction process to create various representations.|Yes|x|
|Dependency Injection|A class accepts the objects it requires from an injector instead of creating the objects directly.|No|x|
|Factory method|Define an interface for creating a single object, but let subclasses decide which class to instantiate. Factory Method lets a class defer instantiation to subclasses.|Yes|x|
|Lazy initialization|"Tactic of delaying the creation of an object, the calculation of a value, or some other expensive process until the first time it is needed. This pattern appears in the GoF catalog as ""virtual proxy"", an implementation strategy for the Proxy pattern."|Yes|x|
|Multiton|Ensure a class has only named instances, and provide a global point of access to them.|No|x|
|Object pool|Avoid expensive acquisition and release of resources by recycling objects that are no longer in use. Can be considered a generalisation of connection pool and thread pool patterns.|No|x|
|Prototype|Specify the kinds of objects to create using a prototypical instance, and create new objects from the 'skeleton' of an existing object, thus boosting performance and keeping memory footprints to a minimum.|Yes|x|
|Resource acquisition is initialization (RAII)|Ensure that resources are properly released by tying them to the lifespan of suitable objects.|No|x|
|Singleton|Ensure a class has only one instance, and provide a global point of access to it.|Yes|x|
|Adapter, Wrapper, or Translator|Convert the interface of a class into another interface clients expect. An adapter lets classes work together that could not otherwise because of incompatible interfaces. The enterprise integration pattern equivalent is the translator.|Yes|x|
|Bridge|Decouple an abstraction from its implementation allowing the two to vary independently.|Yes|x|
|Composite|Compose objects into tree structures to represent part-whole hierarchies. Composite lets clients treat individual objects and compositions of objects uniformly.|Yes|x|
|Decorator|Attach additional responsibilities to an object dynamically keeping the same interface. Decorators provide a flexible alternative to subclassing for extending functionality.|Yes|x|
|Extension object|Adding functionality to a hierarchy without changing the hierarchy.|No|x|
|Facade|Provide a unified interface to a set of interfaces in a subsystem. Facade defines a higher-level interface that makes the subsystem easier to use.|Yes|x|
|Flyweight|Use sharing to support large numbers of similar objects efficiently.|Yes|x|
|Front controller|The pattern relates to the design of Web applications. It provides a centralized entry point for handling requests.|No|x|
|Marker|Empty interface to associate metadata with a class.|No|x|
|Module|Group several related elements, such as classes, singletons, methods, globally used, into a single conceptual entity.|No|x|
|Proxy|Provide a surrogate or placeholder for another object to control access to it.|Yes|x|
|Twin [19]|Twin allows modeling of multiple inheritance in programming languages that do not support this feature.|No|x|
|Blackboard|Artificial intelligence pattern for combining disparate sources of data (see blackboard system)|No|x|
|Chain of responsibility|Avoid coupling the sender of a request to its receiver by giving more than one object a chance to handle the request. Chain the receiving objects and pass the request along the chain until an object handles it.|Yes|x|
|Command|Encapsulate a request as an object, thereby allowing for the parameterization of clients with different requests, and the queuing or logging of requests. It also allows for the support of undoable operations.|Yes|x|
|Interpreter|Given a language, define a representation for its grammar along with an interpreter that uses the representation to interpret sentences in the language.|Yes|x|
|Iterator|Provide a way to access the elements of an aggregate object sequentially without exposing its underlying representation.|Yes|x|
|Mediator|Define an object that encapsulates how a set of objects interact. Mediator promotes loose coupling by keeping objects from referring to each other explicitly, and it allows their interaction to vary independently.|Yes|x|
|Memento|Without violating encapsulation, capture and externalize an object's internal state allowing the object to be restored to this state later.|Yes|x|
|Null object|Avoid null references by providing a default object.|No|x|
|Observer or Publish/subscribe|Define a one-to-many dependency between objects where a state change in one object results in all its dependents being notified and updated automatically.|Yes|x|
|Servant|Define common functionality for a group of classes. The servant pattern is also frequently called helper class or utility class implementation for a given set of classes. The helper classes generally have no objects hence they have all static methods that act upon different kinds of class objects.|No|x|
|Specification|Recombinable business logic in a Boolean fashion.|No|x|
|State|Allow an object to alter its behavior when its internal state changes. The object will appear to change its class.|Yes|x|
|Strategy|Define a family of algorithms, encapsulate each one, and make them interchangeable. Strategy lets the algorithm vary independently from clients that use it.|Yes|x|
|Template method|Define the skeleton of an algorithm in an operation, deferring some steps to subclasses. Template method lets subclasses redefine certain steps of an algorithm without changing the algorithm's structure.|Yes|x|
|Visitor|Represent an operation to be performed on the elements of an object structure. Visitor lets a new operation be defined without changing the classes of the elements on which it operates.|Yes|x|



# some interesting links
https://www.ibm.com/developerworks/library/j-ft10/index.html

http://blog.ezyang.com/2010/05/design-patterns-in-haskel/

https://staticallytyped.wordpress.com/2013/03/09/gang-of-four-patterns-with-type-classes-and-implicits-in-scala/
