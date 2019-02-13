# Deltas

An algebraic theory of incremental diffs between types.

# Simple Delta examples

Consider posessing an `initial :: Int`. If we have a `final :: Int`, we can compute the _delta_ between initial and final as `delta = final - initial :: Int`, such that `initial + delta = final`.

Notice that to pose the question, we need these objects:


```hs
-- Represents the type that captures differences between two elements
-- of type a

-- Represents a typeclass of objects which can be `patch`ed with
-- a Delta
class Patch a where
    type Delta a
    (.-.) :: a -> a -> Delta a
    (#+.):: Delta a -> a -> a
```

##### A note on notation:
- `.` refers to values/points
- `#` refers to hashes (think commits)/deltas
- `.-.` subtracts two points /values to give a delta.
- `#+.` adds a delta on the left to a point on the right.

##### Law:
We need to postulate the law such that `initial + delta = final` works:

```hs
 (a .-. b) #+. a = b
```

##### Implementation for int
```hs

instance Patch Int where
    type Delta Int = Int
    a .-. b = a - b
    d #+.b = d + b
```

# More complicated / compound examples

Now that we have seen the `delta` for `Int`, let's try to generalize it: Can we define it for any inductive type? We would be tempted to answer yes, since we intuitively should be able to find deltas for sums and products of types we already know how to take deltas of

## Deltas of products

```
instance (Patch a, Patch b) => Patch (a, b) where
    type Delta (a, b) = (Delta a, Delta b)
    (a, b) .-. (a', b') = (a .-. a', b .-. b')
    (da, db') #+. (a, b) = (da #+. a, db #+. b)
```

We can prove that this satisfies the laws as follows:

```hs
 ((a, b) .-. (a', b')) #+. (a, b) 
= (a .-.a', b .-.b' ) #+. (a, b) (definition of .-.)
= (a .-. a' #+. a, b .-. b' #+. b) (definition of #+.)
= (a', b') (law)
```

## Deltas of sums

Now, when we come to taking the delta of an `Either`, we get into an interesting situation: What if we need to take the delta of a `Left x` with a `Right y`? There's no way to do this in general! The only sane thing to do is to "save" the value of the `Right y` in the `Delta`. This leads us to definition:


```hs
data EitherDelta a b da db = LL da | LR b | RL a | RR db

instance (Delta a, Delta b) => Delta (Either a b) where
    type Delta (Either a b) = EitherDelta a b (Delta a) (Delta b)
    
    Left a .-. Left a' = LL (a .-. a')
    Right b .-. Right b' = RR (b .-. b')
    
    Left a .-. Right b = LR b
    Rifht b .-. Left a = RL a
    
    (LL da) #+. (Left a) = Left (da #+. a)
    (LR b) #+. (Left a) = Right b
    
    
    (RL a) #+. (Right b) = Left a
    (RR db) #+. (Right b) = Right (db #+. b)
```

Convinvce yourself that there is no other solution in general (and if there is, mail me!)


So, what in the world is going on? Why did we get an `EitherDelta` with 4 constructors? What's the _real_ algebra that's guiding this

# Directions (Modules) and Tensor Products

I'll be wishy washy here, but roughly, here is what's going on.

## Directions

### Product

For a product, we have no "choice" in the values inhabited by a type. An inhabitant of `(a, b)` contains both an `a` and a `b`.

We will represent this as `ab|0>` where `|0>` is a "direction" in some currently unspecified space.

### Sum

For a sum, we _do_ have a choice in the values inhabited by it. An inhabitant of `Either a b` contains either a `Left a` or a `Right `b`.

We will represent this as `a|0> + b|1>`, where now, `|0>, |1>` are two orthogonal directions.

## Tensor product

What we really are doing when trying to take a delta is comparing all possible inhabitants of a given type. This is similar to a tensor product, where we get all possible combinations of basis vectors. Let's try this:

### Tensor product for a product
```
a|0> (x) a|0> = aa |00>
```

### Tensor product for an `Either`

```
(a|0> + b|1>) (x) (a|0> + b|1>) = 
    aa |00> + ab |01> + ba|10> + bb |11>
```

## Constructing the delta type from the tensor product

Note that in both the product and the sum case, we were
left with terms of the form `aa`, and `ab`. Intutively,
in our implementation, when we had a case of `aa` (that is, a case of comparing an `a` with an `a`), we picked the approporiate `Delta a`.


In the sum case, when we had an `ab` (that is, something that transitions from an `a` to a `b`), we picked the type `b` as the choice of delta.

This suggests that we should now create a mapping

```
Delta2 :: Type -> Type -> Type
Delta2 a a = Delta a
Delta2 a b = b
```

That is, we realise that we need a `Delta` *between types*, which
generalizes our notion of `Delta` we had before.

## The rigorous (?) details

We have a noncommutative monoid of types under products, where the unit is `()`, and the product operation is `(,)`. it's non commutative since `(a, b) != (b, a)`

Over this base monoid, we construct some kind of [module](https://ncatlab.org/nlab/show/module+over+a+monoid), such that we can take tensor products of this. Each dimension of this module corresponds to an `Either`.

We next construct an operation called `Delta :: Type -> Type -> Type`, which is some sort of algebra over the monoid of types. I don't know if this thing plays well, so I need to think of laws for this thing.


- I've never seem modules of monoids before, so I'd love some references.

# Algebraic theory 1

```
T := <base> | T * T | T + T | 1 | 0

deltaT :: (T, T) -> T
deltaT(0, 0) = ?
deltaT(0, 1) = ?
deltaT(1, 0) = ?
deltaT(1, 1) = 0 
deltaT(x, y+z) = (deltaT(x, y) + deltaT(x, z)) * 2
deltaT(y+z, x) = (deltaT(y, x) + deltaT(z, x)) * 2

deltaT(x+y,x+y) 
    = 2*(deltaT(x,x+y) + deltaT(y,x+y)) = 
    = 2*(2*(deltaT(x,x) + deltaT(x,y)) + 2*(deltaT(y,x) + deltaT(y,y)))
    = 4 * (deltaT(x, x) + deltaT(x, y) + deltaT(y, x) + deltaT(y, y)) 


deltaT(1+1,1+1) = 4 * (deltaT 1 1 + deltaT 1 1 + deltaT 1 1 + deltaT 1 1) = 4 * 0 = 0?
```

# Algebraic theory 2

```
T := <base> | T * T | T + T | 1 | 0

deltaT :: (T, T) -> T
deltaT(0, 0) = ?
deltaT(0, 1) = ?
deltaT(1, 0) = ?
deltaT(1, 1) = 1 -- constructor carries one unit of information
deltaT(x, y+z) = (deltaT(x, y) + deltaT(x, z))
deltaT(y+z, x) = (deltaT(y, x) + deltaT(z, x))

deltaT(x+y,x+y) 
    = (deltaT(x,x+y) + deltaT(y,x+y)) = 
    = (deltaT(x,x) + deltaT(x,y)) + 2*(deltaT(y,x) + deltaT(y,y)))
    = (deltaT(x, x) + deltaT(x, y) + deltaT(y, x) + deltaT(y, y)) 


Also fails, we need to get 2 as the answer - either the bit flipped or it did not
deltaT(1+1,1+1) = (deltaT 1 1 + deltaT 1 1 + deltaT 1 1 + deltaT 1 1) = 4
```



# Algebraic theory 3

```
T := <base> | T * T | T|0> + T|1> | 1 | 0
deltaT(1, 1) = 0.
H (X|Y) = H(X) - H(Y)
---------------------

deltaT(x, y|0> + z|1>) = deltaT(x, y) |0> +  deltaT(x, z) |1>
deltaT(x|0> + y|1>, z) = deltaT(x, z) |0> +  deltaT(y, z) |1>

deltaT(x|0>+y|1>,x|0>+y|1|>
 = deltaT(x|0> + y|1>, x)|0> + deltaT(x|0> + y|1>, y)|1>
 = delta(x, x) |00> + delta(y, x) |10> + deltaT(x, y)|01> + deltaT(y,y)|11>

deltaT(1|0> + 1|1>, 1|0> + 1|1>) 
=  deltaT(1, 1) |00> + deltaT(1, 1) |10> + deltaT(1, 1) |01> + deltaT(1, 1) |11>
= 0 |00> + 0 |10> + 0 |01> + 0 |11>
= 
 
```

# Eureka

Let's go back to the examples of using vectors as directions. I'll explain the problem and the correct solution.

First, we need to agree that
```hs
delta(1, 1) = 1
```
This is because `1` contains `log 1 = 0` bits of information, and is still "reachable".

Now, let's consider `Bool`
```
Bool = 1 |L> + 1 |R>
```

```
delta(x, y |L> + y |R>) <= delta (x, y) |L> + delta (x, y) |R> 
delta(x|L> + y|L>, z) <= delta (z, x) |L> + delta (z, x) |R> 
```

yes, that's a `<=`, by which I mean, it's an *over-approximation*. What do I mean by this? Let's explain with the `Bool`
example:

```
delta(Bool, Bool) = 
delta (Bool, 1|L> + 1|R>)
= delta(Bool, 1) |L> + delta(Bool, 1) |R>
= delta(1|L> + 1|R>, 1) |L> + delta (1|L> + 1|R>, 1) |R>
= (1|L> + 1|R>)|L> + (1|L> + 1|R>)|R>
= 1 |LL> + 1 |RL> + 1 |LR> + 1 |RR>
```
See that our beautiful theory fails us `:(` It tells us we need 4 values to tell how
to transition from a `Bool` to a `Bool`, when we know full well that just `2` will do.
So, now what?

However, at this point, note that we are encoding information about _both_ where we came from,
and where we want to go. But in our theory of deltas, we don't care about storing where we
came from, since we assume the user will tell us that!So, all we need to store is
where we want to go. With this obervation, I can factor the previous expression as

```
1 |LL> + 1 |RL> + 1 |LR> + 1 |RR>
= (1|L> + 1|R>) (x) (1|L> + 1|R>)
= Bool (1|L> + 1|R>) 
```

(I use `(x)` to denote tensor product)

So, what we _actually_ need to store is (1|L> + 1|R>), which on being given the 
original value, allows us to recover the full delta.

I find it quite compelling that we wind up getting tensor products here, it tells
us that we're on the right track of abstraction.


So, in general, our correct delta type will be `overapproximate delta / original`, where
we can interpret `/` as the inverse operation to `(x)`. It's kind of like
tracing out a pure state from a quantum system, but I dont' know what such an operator is called
in general.

# TL;DR

- Types ~ modules over monoids.
- Deltas ~ mapping into a tensor product of modules, then a morphism applied to the scalars.
- Why? Fun. Also, maybe this is faster
- TODO: Consider the case where we want a `Patch` to be reversible.
- [Equality on (Cont r a)](https://www.reddit.com/r/haskell/comments/ahu6jp/fun_fact_the_continuation_monad_cont_r_a_has_an/)

