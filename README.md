# Deltas: An algebraic theory of diffs in haskell
[![Build Status](https://travis-ci.com/bollu/paper-deltas.svg?token=pjHzFXxnXziVY1C82Cs7&branch=master)](https://travis-ci.com/bollu/paper-deltas)

- [Downloaded current paper version here: `paper.pdf`](https://github.com/bollu/paper-deltas/releases)

# Work log
- Create roll forward not back infrastructure and __benchmark it__.
- Create diff instance for `Pairing g f -> Pairing (Cofree g) (Patch (Free f))`.
  for cofree comonad.
- Try to create similar instance for arrows.
- Create diff instance for `Mu f = Mu { unMu :: f (Mu f) }`.

# To read:
- [Composable memory transactions](https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf)
- [Transactional memory with data invariants](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/stm-invariants.pdf)
- [Safe programmable speculative parallelism](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/pldi026-vaswani.pdf)
- [Selective applicative functors](https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf)
- [`vector-patches` on Hackage](https://hackage.haskell.org/package/patches-vector-0.1.5.4/docs/Data-Patch-Internal.html)
- [Math.se question about affine groups](https://math.stackexchange.com/questions/3096752/affine-vector-spaces-with-groups)
- [Equality on (Cont r a)](https://www.reddit.com/r/haskell/comments/ahu6jp/fun_fact_the_continuation_monad_cont_r_a_has_an/)
- [Monads for incremental computing](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.8.3014&rep=rep1&type=pdf)
- [Free and Cofree for interpreters](http://abailly.github.io/posts/free.html)
- [Cofree meets Free](http://blog.sigfpe.com/2014/05/cofree-meets-free.html)
- [Entropy as information loss](https://johncarlosbaez.wordpress.com/2011/06/02/a-characterization-of-entropy/)
- [Categorical theory of patches](https://arxiv.org/pdf/1311.3903.pdf)
- [Partial monoids](https://arxiv.org/pdf/1002.2166.pdf)
- [Arrows and static ananlysis](https://elvishjerricco.github.io/2017/03/10/profunctors-arrows-and-static-analysis.html)
- [Roll forward not back](./reading/roll-forward-not-back.pdf)
- [A model for parallel incremental computation](./reading/two-for-the-price-of-one-parallel-and-incremental-computation.pdf)
- [Sketches of how to do proper deltas of sums](./SKETCH.md)

# To do
- Derive pretty `show` instances for the deltas to aid debugging
- See if you can use the deltas / incremental nature as some kind of `Monad`
- Try to check if `Free/Cofree` can be adapted for delta-style computation
  to automatically create React like interfaces
- Consider `Delta` on `Arrow`, which form networks.
# To Think

Try and derive deltas as as a list of `one-hole-contexts` + `delta at hole`.
That is, each delta is some (collection of) changes made at context(s).
Check if this results in the same formulation. If not, why does it not?

# To setup:
- [latex builds with Circle](https://discuss.circleci.com/t/latex-pdf-building/668/4)

# Learning how to generate TH
```
*Main> :set -XTemplateHaskell
*Main> runQ [d| type instance Patch Float = NumDelta Float |]
[TySynInstD Main.Patch (TySynEqn [ConT GHC.Types.Float] (AppT (ConT Main.NumDelta) (ConT GHC.Types.Float)))]
```

In general, ask it to generate whatever you want in the banana brackets, and it 
should work.

