# Deltas: An algebraic theory of diffs in haskell
- [See current paper version here](TODO: fill in link)


# To read:
- [Math.se question about affine groups](https://math.stackexchange.com/questions/3096752/affine-vector-spaces-with-groups)
- [Equality on (Cont r a)](https://www.reddit.com/r/haskell/comments/ahu6jp/fun_fact_the_continuation_monad_cont_r_a_has_an/)
- [Monads for incremental computing](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.8.3014&rep=rep1&type=pdf)
- [Free and Cofree for interpreters](http://abailly.github.io/posts/free.html)
- [Cofree meets Free](http://blog.sigfpe.com/2014/05/cofree-meets-free.html)
- [Entropy as information loss](https://johncarlosbaez.wordpress.com/2011/06/02/a-characterization-of-entropy/)

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

