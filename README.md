# Deltas: An algebraic theory of diffs in haskell
- [See current paper version here](TODO: fill in link)


# To read:
- [Math.se question about affine groups](https://math.stackexchange.com/questions/3096752/affine-vector-spaces-with-groups)
- [Equality on (Cont r a)](https://www.reddit.com/r/haskell/comments/ahu6jp/fun_fact_the_continuation_monad_cont_r_a_has_an/)
- [Monads for incremental computing](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.8.3014&rep=rep1&type=pdf)
- [Free and Cofree for interpreters](http://abailly.github.io/posts/free.html)
- [Cofree meets Free](http://blog.sigfpe.com/2014/05/cofree-meets-free.html)
- [Entropy as information loss](https://johncarlosbaez.wordpress.com/2011/06/02/a-characterization-of-entropy/)
- [Categorical theory of patches](https://arxiv.org/pdf/1311.3903.pdf)
- [Partial monoids](https://arxiv.org/pdf/1002.2166.pdf)
- [Arrows and static ananlysis](https://elvishjerricco.github.io/2017/03/10/profunctors-arrows-and-static-analysis.html)

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

