Catalan Isomorphisms
====================

Experiments with type-safe encoding and translation of various mathematical objects which are counted by the Catalan numbers, such as Dyck paths, ordered trees, binary trees, and others.

Non-Negative and Dyck Paths
---------------------------

The central type defined in this module is that of a [Dyck path](http://mathworld.wolfram.com/DyckPath.html) — a lattice path which does not cross below the diagonal and terminates at the same height at which it began (illustrated below courtesy of the [Journal of Statistical Mechanics](http://iopscience.iop.org/1742-5468/2009/03/P03025/fulltext/)).

![A Dyck Path](dyck-path.jpg)

The datatype to represent this structure is called an `NNPath`, short for non-negative path. Non-negative paths are indexed by a type parameter indicating the height of their end; therefore, Dyck paths — and thus the type `Dyck` — are simply synonymous with non-negative paths of terminating height zero; in other words, `type Dyck = NNPath Z`.

With the Catalan package, you can construct the above-illustrated Dyck path in two different ways.

Using simply the type constructors (`U` for up, `D` for down, and `End` for an empty path):

```Haskell
D (D (D (D (U (U (U (D (U (D (U (U (D (U End)))))))))))))
```

I also introduce the `(-)` operator. It allows one to construct paths more succinctly, and in an order matching the diagram. In reality, `(-) = flip ($)`, but with a type restricting its application to contructors of non-negative paths:

```Haskell
(-) :: NNPath x -> (NNPath x -> NNPath y) -> NNPath y
```

With it, the same path is described as:

```Haskell
End-U-D-U-U-D-U-D-U-U-U-D-D-D-D
```

If you don't like that the module exports an operator which occludes the normal subtraction operator, suggestions for a different name are welcome. :)

Non-negative paths are implemented in this module as a GADT which prevents the construction of paths violating the non-negativity constraint. As such, the following is a compile-time type error:

```
> (D End)

Couldn't match type 'Z with 'S m0
    Expected type: NNPath ('S m0)
      Actual type: NNPath 'Z
    In the first argument of `D', namely `End'
    In the expression: (D End)
```

Concatenating Paths
-------------------

We can concatenate two non-negative paths to yield another non-negative path using the following operation:

```Haskell
(|+|) :: NNPath n -> NNPath m -> NNPath (Plus m n)
```

For instance:

```Haskell
End-U-U-D-D |+| End-U-D == End-U-U-D-D-U-D
```

The parameter order of the `|+|` function is such that it makes visual sense when used with the `End-U-D` construction syntax; it feels backwards when used with the `D (U End)` constructor application syntax. This is the case for many of the operations in this module, and as such, the former is the preferred syntax for constructing non-negative paths. (Note that when destructuring a non-negative path, you are still doing this from "right" to "left".)

Splitting Paths
---------------

Dyck paths can consist of multiple concatenated Dyck paths. We can `split` them apart into a list of Dyck paths:

```Haskell
split :: Dyck -> [Dyck]
```

```Haskell
split $ (End-U-D) |+| (End-U-U-D-U-D-D) == [(End-U-D),(End-U-U-D-U-D-D)]
```

> **Note**: It is the case that every resulting Dyck path from splitting will be *prime* — it will only touch zero at its start and end. This is not (yet) encoded in the types for Dyck paths in a meaningful way. Further work will be in the direction of doing this, so that we can define a type-safe operation to lower a prime Dyck path by one (removing its preceding and succeeding up and down segments). This will enable a more direct, transparent, and safe mapping from Dyck paths to trees.

Isomorphisms!
-------------

Of course, the whole purpose of this module is to show isomorphisms between Catalan objects. For this purpose, there is the `Catalan` typeclass. Catalan objects should all have an isomorphism to Dyck paths — by defining `toDyck` and `fromDyck` methods, all Catalan objects can be mapped to each other, using Dyck paths as an intermediate representation.

So far, I have only implemented an isomorphism between ordered trees and Dyck paths.

```Haskell
toDyck $ Node [Node [Node [],Node[]],Node[Node [Node [],Node []],Node []]]
== (End-U-U-D-U-D-D-U-U-U-D-U-D-D-U-D-D)

fromDyck (End-U-U-D-U-D-D-U-U-U-D-U-D-D-U-D-D) :: Tree
== Node [Node [Node [],Node[]],Node[Node [Node [],Node []],Node []]]
```
