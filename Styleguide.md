Haskell Style Guide
---

In order to keep code in this repository as readable and aproachable as possible, we ask that you follow some basic formatting guidelines.
As with all style guides, functionality comes first, and if for some reason, you absolutely must choose between writing code that works
and code that follows this guide, write code that works. However, please don't be offended if we criticize sections of your code that
violate this style guide, or refactor your code during pull requests to make it more compliant with this style guide. If something isn't
explicitly covered in this style guide, try your best to follow code examples in this guide and other code within the project.

Formatting
---

## Line length

Soft max line length is 80 characters (Only exceed if a line break will result in a line with >10 chars, use judgement)
Hard max is 100 chars (Lines > 100 characters are not allowed at all)

## Indentation

Tabs are illegal. Use spaces for indenting. Use 2 spaces to indent code, except where this would result in invalid code (e.g. where statements)

```haskell
cat :: IO ()
cat = do
  var <- getLine
  putStrLn var

sayHello :: IO ()
sayHello = do
    name <- getLine
    putStr $ greet name
  where
    greet "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
```

## Blank lines
Maintain one blank line between top level definitions. Do not add a blank line between a type declaration and an implementation.
Avoid blank lines in data declarations. Do not put a blank line between an instance declaration and the first instance function.
Blank lines between functions within instance declarations are optional if the function body is large. Use your best judgement

```haskell
-- Good
doThing1 :: IO ()
doThing1 = putStrLn "did thing"

doThing2 :: IO ()
doThing2 = putStrLn "did other thing"

-- Bad
doThing3 :: IO ()
doThing3 = putStrLn "I have bad formatting"
doThing4 :: IO
doThing4 = putStrLn "The programmer who wrote this is a bad person"

-- Good
data MultiLineThing = Line1
                    | Line2
                    | Line3

data BadDataFormat  = Why
                    | Would

                    | You
                    
                    | Ever

-- Good
instance Has3Functions MultiLineThing where
  function1 = return "Hello"
  function2 = return "World"
  function3 = return "!"

-- Also good
instance HasLargeFunction MultiLineThing where
  functionLarge = do
    lots
    of
    things
    with
    a
    large
    body
    return "Done!"

  function2 = return "I have a blank line before me!"

-- Bad
instance MadeByABadPerson BadDataFormat where

  function = "This makes no sense. Why would you ever"
```

## Data Declarations

Data constructors should be properly aligned.
```haskell
-- Good
data Tree = Branch !a !(Tree a) !(Tree a)
          | Leaf

-- Bad
data JustNo = AConstructor
  | NaughtyConstructor

-- It doesn't matter where constructors are aligned, as long as they're aligned
data HasASuperLongName 
  = OneConstructor
  | AnotherConstructor

-- Records must also be aligned
data RecordSyntas = Record {
    oneRecord     :: String
  , anotherRecord :: String
  , truthishVal   :: Bool
  } deriving (Eq, Show)
```

## List Constructors

Align elements in lists

```haskell
exceptions = [ 
    InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
```

## Infix operators during line breaks

Trailing infix operators should be avoided. Instead, lead the next line with the operator

```haskell
-- Good
myList = [
    1
  , 2
  , 3
  , 4
  ]

-- Bad
badList = [
  1 ,
  2 ,
  3 ,
  4
  ]

-- This also applies to things other than lists

-- Good
withFmap = a
       <$> b
       <*> c

-- Bad
badFmap = a <$>
          b <*>
          c
```

## Pragmas

Pragmas should immediately follow the function they apply to

```haskell
id :: a -> a
id x = x
{-# INLINE id -#}
```

In the case of datatype definitions, pragmas must immediately precede the type it applies to. Indent all other constructors to align.

```haskell
data Array e = Array
  {-# UNPACK #-} !Int
                 !ByteArray
```

## Export lists and modules

When a module both re-exports modules and contains its own code, the module name should immediately follow the opening parentheses

```haskell
module Data.Set 
  ( module Data.Set
  , module Data.Set.Operators
  ) where

  helloWorld :: IO ()
  helloWorld = putStrLn "Hello, World!"
```

Code in a module should be indented once.

```haskell
module MyModule where
  
  -- Good
  apply :: (a -> b) -> a -> b
  apply f a = f a

-- Bad
whyDoThis :: Bool
whyDoThis = False
```

## If-then-else clauses

In general, guards and pattern matches should be used over if-then-else clauses. However,
where guards and pattern matches cannot be used, `then` and `else` statements should be indented.

```haskell
foo = if condition
        then code
        else moreCode
```

## Case expressions

Case expressions should be as follows:

```haskell
foobar = case something of
  Just j  -> foo
  Nothing -> bar
```

When the statement in question is too long to place on a single line, the `->` should be
deferred to the next line

```haskell
foobar' = case something of
  VeryLongPatternToBeMatched var
    -> foo
  otherwise -> bar
```

## Imports

Imports should be grouped in the following order:

1. Standard library
2. Related third party
3. Local application/library specific imports

Seperate import groups with a blank line. Imports should be sorted alphabetically, by module name.

Comments and Commits
---

In order to maintain basic sanity, pull requests will be rejected if the following guidelines
are not followed.

## Punctuation and spelling

Write proper sentences, start with a capital letter and use proper punctuation. 
Spell words properly.

## Top level definitions

All top level definitions must have a type signature. Top level definitions should have a
Haddock syntax comment. If a top level definition is exported, it must have a Haddock syntax
comment.

```haskell
-- | Send a message on a socket.  The socket must be in a connected
--   state.  Returns the number of bytes sent.  Applications are
--   responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

Record datatypes should also be commented.

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

## Multi line comments

Block comments should be avoided. Haddock-style comments should be aligned with the first letter of the first line

```haskell
-- | This is a haddock comment
--   that spans multiple lines
```

Other multi-line comments should be indented after the first line

```haskell
-- This is a non-haddock
--   comment that also spans
--   multiple lines
```

Code patterns
---

## Naming

Use camelCase for functions and PascalCase for data types. For readability, don't capitalize
all the letters of of abbreviations when there are 3 or more letters. E.g. `HttpServer`
instead of `HTTPServer`

## Modules

Use singulars for modulare names, e.g. `Data.Map` over `Data.Maps`.

## Lambda functions

`where` clauses should generally be preferred over lambda functions. This is because `where`
clauses support type signatures, and generally give more readable errors.
EXCEPTION: Lambdas are acceptable as arguments to higher-order functions when the function
body is short.

## Point-Free style and `(>>=)`

Point-free style should generally be avoided, as it tends to lead to unreadable code.
However, in most cases, pointfree code should be annotated with its pointful equivalent.
`(>>=)` should be treated similar Unix's `|` operator. If you would save the output of a
command to a file, you should save the output of the equivalent function to a variable.
