Title: Binary Tree in Haskell
Date: 2018-08-20
Category: Programming
Tags: computers, programming, haskell
Status: published
Summary: Binary Tree in Haskell

# Introduction

Binary trees have internal structure; for any given node, all elements to the
left are less than the current value, and all elements to the right are greater
than the current value.

For instance:
```
     5
  /--+--\
  3     6
/-+-\
1   2
```

Lets build a binary tree in Haskell.

# Type Definition

A lot of the power of Haskell comes from it's type system. New types can be
defined in terms of existing types (a type constructor), as aliases for
existing types (AuthorName :: String), or as original items (EmptyTree).

Lets define a tree using a type constructor (a type that works on another
type). It doesn't make much sense to just have a 'tree', it's a tree of
'something', like a tree of `Int` or `String`. The only constraint is a way to
compare the elements that our tree will be made of. Without some way to compare
elements, we won't be able to decide whether to place an element in the left or
right side when we're inserting elements.

Our `BinaryTree` may either be an `EmptyTree` or a `Node`. A `Node` is composed
of a value, the 'something' we're making the tree of, and a left and right
side, which are also trees.

```haskell
module BinaryTree where

data Tree a = EmptyTree
            | Node a (Tree a) (Tree a) deriving (Read, Eq)
```

The snippet `deriving (Read, Eq)` tells Haskell that our `Tree` can be compared
to other trees and can be parsed from a string representation. We could write
out a tree to a file, and Haskell would know how to read it back into a tree.

# Tree operations

What can we do with our new tree? Add elements, remove elements, check if an
element is a member, and get height. Each of these functions can be defined
recursively with pattern matching using our new types. We'll use `EmptyTree` as
our base case.

The type definitions for insert and elem require that the elements of our tree
are orderable. To tell if the element we're looking for is what we've found, we
have to have a way to compare items. This may seem strange compared to other
languages where most types are comparable - you might draw an analogy with a
complex class in Python and Java. Is this object less than another? What does
that mean?

- Insert takes an 'orderable something', an existing tree, and produces a new
  tree. We also require that the element we're inserting and the tree's
  elements have the same type. This makes sense, and keeps us from trying to
  insert a `String` into a tree of `Int`, etc.

```haskell
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x <  a = Node a (treeInsert x left) right
    | x >  a = Node a left (treeInsert x right)
```

- Elem also takes an 'orderable something' to look for, an existing tree and
  produces a boolean.

```haskell
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x <  a = treeElem x left
    | x >  a = treeElem x right
```

- Height only requires a tree and produces an `Int`. If you could manually
  construct a tree of elements that aren't orderable (meaning you couldn't use
  insert), we can still determine the height of the tree.

```haskell
treeHeight :: Tree a -> Int
treeHeight EmptyTree = 0
treeHeight (Node a left right) = maximum [1, lh, rh]
    where lh = 1 + treeHeight left
          rh = 1 + treeHeight right
```

# Printing

Haskell can print our trees using the structure we defined in the type, but it
doesn't look great. We can do the work of `deriving (Show)` ourselves and
produce prettier output.

The type definition states that for a tree to be converted to a string, the
elements of the tree must also be convertable to string. We could show the
structure of the tree without this requirement, but we wouldn't be able to
represent the values.

```
haskell> Node 5 (Node 3 EmptyTree (Node 4 EmptyTree EmptyTree)) EmptyTree

  3
    4
5
```

```haskell
instance (Show a) => Show (Tree a) where
    show EmptyTree = ""
    show tree = show' tree 0 (widestElement tree + 1)
```

`show'` is a helper function that has the extra context we need to pretty
print - depth and padding. Each subtree is printed further to the right.
widestElement tells us how much to pad elements so they align.

```haskell
show' :: (Show a) => Tree a -> Int -> Int -> String
show' EmptyTree _ _ = " "
show' (Node a left right) depth width =
    leftside ++ "\n" ++ center ++ rightside
    where center    = replicate depth ' ' ++ show a
          leftside  = show' left (depth + width) width
          rightside = show' right (depth + width) width

widestElement :: (Show a) => Tree a -> Int
widestElement EmptyTree = 0
widestElement (Node center left right) = maximum [l, r, c]
    where l = widestElement left
          r = widestElement right
          c = length $ show center
```

# Building Trees

Manually defining trees can be a bit of work, so let's define some helper
functions.

- `makeTree` takes a list of elements and inserts them into an EmptyTree

```haskell
makeTree :: (Ord a) => [a] -> Tree a
makeTree = foldr treeInsert EmptyTree . reverse
```

```
haskell> makeTree [14,3,16,37,250,21]

   3
14
   16
         21
      37
         250
```

- `randomTree` produces a random tree of numbers with the specified number of
  elements

```haskell
randomTree :: Int -> IO (Tree Int)
randomTree n = do
    numbers <- randomList n
    return $ makeTree numbers

import System.Random (randomRIO)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
    r  <- randomRIO  (1,99)
    rs <- randomList (n-1)
    return (r:rs)
```

```
haskell> randomTree 5
   16
      57
73
   75
      87
```

# Summary

Haskell's type system makes defining types straight forward. The type
declarations for the functions we define force us to answer interesting
questions about how we expect things to behave, and the constraints required.

```
haskell> randomTree 20

         2
            3
      6
               8
            12
               14
         20
   22
23
         34
      37
         43
   70
         73
      84
         86
               92
            96
               99
```
