%% For double-blind review submission, w/o CCS and ACM Reference (max submission space)
\documentclass[acmsmall,review,anonymous]{acmart}\settopmatter{printfolios=true,printccs=false,printacmref=false}
%% For double-blind review submission, w/ CCS and ACM Reference
%\documentclass[acmsmall,review,anonymous]{acmart}\settopmatter{printfolios=true}
%% For single-blind review submission, w/o CCS and ACM Reference (max submission space)
%\documentclass[acmsmall,review]{acmart}\settopmatter{printfolios=true,printccs=false,printacmref=false}
%% For single-blind review submission, w/ CCS and ACM Reference
%\documentclass[acmsmall,review]{acmart}\settopmatter{printfolios=true}
%% For final camera-ready submission, w/ required CCS and ACM Reference
%\documentclass[acmsmall]{acmart}\settopmatter{}


%% Journal information
%% Supplied to authors by publisher for camera-ready submission;
%% use defaults for review submission.
\acmJournal{PACMPL}
\acmVolume{1}
\acmNumber{CONF} % CONF = POPL or ICFP or OOPSLA
\acmArticle{1}
\acmYear{2018}
\acmMonth{1}
\acmDOI{} % \acmDOI{10.1145/nnnnnnn.nnnnnnn}
\startPage{1}

%% Copyright information
%% Supplied to authors (based on authors' rights management selection;
%% see authors.acm.org) by publisher for camera-ready submission;
%% use 'none' for review submission.
\setcopyright{none}
%\setcopyright{acmcopyright}
%\setcopyright{acmlicensed}
%\setcopyright{rightsretained}
%\copyrightyear{2018}           %% If different from \acmYear

%% Bibliography style
\bibliographystyle{ACM-Reference-Format}
%% Citation style
%% Note: author/year citations are required for papers published as an
%% issue of PACMPL.
\citestyle{acmauthoryear}   %% For author/year citations


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Note: Authors migrating a paper from PACMPL format to traditional
%% SIGPLAN proceedings format must update the '\documentclass' and
%% topmatter commands above; see 'acmart-sigplanproc-template.tex'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Some recommended packages.
\usepackage{booktabs}   %% For formal tables:
                        %% http://ctan.org/pkg/booktabs
\usepackage{subcaption} %% For complex figures with subfigures/subcaptions
                        %% http://ctan.org/pkg/subcaption
\usepackage{minted}


% environments
\newenvironment{code}{\VerbatimEnvironment \begin{minted}{haskell}}{\end{minted}}
\newcommand{\hsmint}[1]{\mintinline{haskell}{#1}}


\begin{document}

%% Title information
%% [Short Title] is optional;
%% when present, will be used in
%% header instead of Full Title.
% \title[Deltas: an algbraic theory of difs]{Full Title}
\title{Deltas: An algebraic theory of diffing}
%% \titlenote is optional;
%% can be repeated if necessary;
%% contents suppressed with 'anonymous'
%\titlenote{with title note}
%% \subtitle is optional
%% \subtitlenote is optional;\subtitle{Subtitle}                     
%% can be repeated if necessary;
%% contents suppressed with 'anonymous'
%\subtitlenote{with subtitle note}       


%% Author information
%% Contents and number of authors suppressed with 'anonymous'.
%% Each author should be introduced by \author, followed by
%% \authornote (optional), \orcid (optional), \affiliation, and
%% \email.
%% An author may have multiple affiliations and/or emails; repeat the
%% appropriate command.
%% Many elements are not rendered, but should be provided for metadata
%% extraction tools.

%% Author with single affiliation.
\author{Siddharth Bhat}
\authornote{with author1 note}          %% \authornote is optional;
                                        %% can be repeated if necessary
\orcid{nnnn-nnnn-nnnn-nnnn}             %% \orcid is optional
\affiliation{
  \position{}
  \department{Computer Science}              
  \institution{International Institute of Information Technology, Hyderabad}
  \streetaddress{}
  \city{}
  \state{}
  \postcode{}
  \country{India}                    %% \country is recommended
}
\email{siddharth.bhat@research.iiit.ac.in}          %% \email is recommended


%% Abstract
%% Note: \begin{abstract}...\end{abstract} environment must come
%% before \maketitle command
\begin{abstract}
We consider the concept of diffing between two elements of a type,
and the ramifactions thereof. We find that there is the rich algebra of
Torsors which governs this process. We construct a process to automatically derive
the type of diffs of a given base type. This leads us into detours of
Torsors, Tensor algebra, and Information Theory. We explore usecases
of this infrastrcture for debugging, logging, and caching. 
We provide a literate implementation in Haskell which showcases these ideas.
We also consider some extentions to the theory outlined here.
\end{abstract}


%% 2012 ACM Computing Classification System (CSS) concepts
%% Generate at 'http://dl.acm.org/ccs/ccs.cfm'.
\begin{CCSXML}
<ccs2012>
<concept>
<concept_id>10011007.10011006.10011008</concept_id>
<concept_desc>Software and its engineering~General programming languages</concept_desc>
<concept_significance>500</concept_significance>
</concept>
<concept>
<concept_id>10003456.10003457.10003521.10003525</concept_id>
<concept_desc>Social and professional topics~History of programming languages</concept_desc>
<concept_significance>300</concept_significance>
</concept>
</ccs2012>
\end{CCSXML}

\ccsdesc[500]{Software and its engineering~General programming languages}
\ccsdesc[300]{Social and professional topics~History of programming languages}
%% End of generated code


%% Keywords
%% comma separated list
\keywords{types, algebra, functional pearl}  %% \keywords are mandatory in final camera-ready submission


%% \maketitle
%% Note: \maketitle command must come after title commands, author
%% commands, abstract environment, Computing Classification System
%% environment and commands, and keywords command.
\maketitle


\section{Introduction}

We first begin with our GHC incantations:
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs  #-}
import Prelude hiding ((.))
import Data.Monoid
import Control.Arrow
import Control.Category
import Generics.Eot
import GHC.Generics
import qualified Language.Haskell.TH as TH
import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Prettyprint.Doc.Util as P
import qualified Data.Text.Prettyprint.Doc.Render.String as P
import qualified WorkingGenerics as WG
import qualified Data.FingerTree as FT
import qualified Data.LCA.Online as LCA
import qualified Data.LCA.View as LCA

\end{code}
Helpers for pretty printing

Next, we define the typeclass which we will constantly end up using:
\begin{code}
-- forall s. mact mempty s = s
-- forall m1 m2 s. (m1 <> m2) <>> s = m2 <>> (m1 <>> s)
class Monoid m => MonoidAction m s  |  m -> s where
    mact :: m -> s -> s
    mact = (<>>)
    -- (<>>) :: MonoidAction m s => m -> s -> s
    (<>>) :: m -> s -> s
    (<>>) = mact


-- forall s. mdelta s s = mempty
-- forall s1 s2. (s2 <-> s1) <>> s1 = s2
class MonoidAction m s => MonoidTorsor m s | m -> s where
    mdelta :: s -> s -> m
    mdelta = (<->)
    (<->) :: s -> s -> m
    (<->) = mdelta
\end{code}

Now that we have defined a space that is able to support monoidal structures
over it, we begin to motivate this. We first introduce some terminology. We
call the diff between two values a \hsmint{Patch}. Notice
that a patch has the structure of \hsmint{MonoidTorsor}. Since the
diff of an element with iteself is the identity element \hsmint{(mempty = s <-> s)},
this acting on \hsmint{s} will produce no change, as we would expect.
Also notice that patches compose. That is, given two patches, we can
compose them to form a third patch which applies the original two patches
in sequence. Hence, patches have a \hsmint{MonoidAction} structure with
respect to the type they diff.

We will first create a framework that allows us to automatically derive
the $\hsmint{Patch}$ instance of a given type. We then showcase
this for multiple purposes, including pretty printing, caching, and debugging.
We construct a family of useful operators around this object.

% why default data family instances do not make sense
% https://stackoverflow.com/questions/18965939/data-family-default-instances
\begin{code}
type family Patch a = p | p -> a
type family GGPatch a = p | p -> a
-- GHC.Generic version of diff
class (Monoid p, MonoidAction p a, MonoidTorsor p a) => GGDiff a p | p -> a where
-- Generic version of diff
class (Monoid p, MonoidAction p a, MonoidTorsor p a) => GDiff a p | p -> a where
-- Concrete version of Diff
class (Monoid p, MonoidAction p a, MonoidTorsor p a) => Diff a p | p -> a where


instance (Monoid p, MonoidAction p x, MonoidTorsor p x, GDiff x p) => Diff x p

\end{code}

We now have a class \hsmint{Diff}, which represents a type that
has a structure which can be diffed. We have an associated data
family \hsmint{Patch}, which is the type of patches for a diff. We
then instantiate the boilerplate for the correct \hsmint{Monoid},
\hsmint{MonoidAction}, and \hsmint{MonoidTorsor} defintions.


We now use the \texttt{Generics} support in haskell to construct a method
to derive the \hsmint{Diff} instance for any algebraic data type.

% https://generics-eot.readthedocs.io/en/stable/tutorial.html
\begin{code}
-- class (p ~ Patch a) => Diff a p where
--     patchempty :: Patch a
--     patchappend :: p -> p -> p
--     diff :: a -> a -> p
--     patch :: p -> a -> a

data EitherPatch a b pa pb = PL a | PR b | PPL pa | PPR pb deriving(Show)

instance (Monoid pa, Monoid pb) => Monoid (EitherPatch a b pa pb) where
    mempty = PPL mempty
    mappend _ (PL a) = (PL a)
    mappend _ (PR b) = (PR b)
    mappend (PPL pa) (PPL pa') = PPL (pa <> pa')
    mappend (PPR pb) (PPR pb') = PPR (pb <> pb')
    mappend x y = error $ "unable to combine patches!"


instance (MonoidAction pa a, MonoidAction pb b) => 
    MonoidAction (EitherPatch a b pa pb) (Either a b) where
    mact (PL a) _ = Left a
    mact (PR b) _ = Right b
    mact (PPL pa) (Left a)  = Left (pa <>> a)
    mact (PPR pb) (Right b)  = Right (pb <>> b)


instance (MonoidTorsor pa a, MonoidTorsor pb b) => MonoidTorsor (EitherPatch a b pa pb) (Either a b) where
    mdelta (Left a) (Left a') = PPL (mdelta a a')
    mdelta (Right b) (Right b') = PPR (mdelta  b b')
    mdelta (Left _) (Right b) = PR b
    mdelta (Right _) (Left a) = PL a


type instance Patch (Either a b) = EitherPatch a b (Patch a) (Patch b)
type instance GGPatch (Either a b) = EitherPatch a b (GGPatch a) (GGPatch b)
instance (GDiff a pa, GDiff b pb) => GDiff (Either a b) (EitherPatch a b pa pb) where


type instance Patch () = ()
type instance GGPatch () = ()

instance MonoidAction () () where
    mact () () = ()

instance MonoidTorsor () () where
    mdelta () () = ()

instance GDiff () () where

type instance Patch (x, xs) = (Patch x, Patch xs)
type instance GGPatch (x, xs) = (GGPatch x, GGPatch xs)
instance (MonoidAction pa a, MonoidAction pb b) => MonoidAction (pa, pb) (a, b) where
    mact (p, p') (x, x') = (mact p x, mact p' x')

instance (MonoidTorsor pa a, MonoidTorsor pb b) => MonoidTorsor (pa, pb) (a, b) where
    mdelta (x1, x1') (x2, x2') = (mdelta x1 x2, mdelta x1' x2')

instance (GDiff x p , GDiff y q) => GDiff (x, y) (p, q)

type instance Patch Void = Void
type instance GGPatch Void = Void
instance Monoid Void where
    mempty = undefined
    v `mappend` _ = absurd v
    

instance MonoidAction Void Void where
    mact v = absurd v

instance MonoidTorsor Void Void where
    v <-> _  = absurd v

instance GDiff Void Void

-- Trying to get the type system to give me an instance of Diff
-- from an instance of GenericDiff. Woe is me
-- instance (GenericDiff a, HasEot a, Patch (Eot a) ~ p) => Diff a (Patch (Eot a)) where

gdiff :: (HasEot a,  GDiff (Eot a) (Patch (Eot a))) => a -> a -> Patch (Eot a)
gdiff a a' = (toEot a) <-> (toEot a')

-- ggdiff :: (Repr a,  GGDiff (Rep a) (GGPatch (Rep a))) => a -> a -> GGPatch (Rep a)
-- ggdiff a a' = (rep a) <-> (rep a')


gpatch :: (HasEot a, GDiff (Eot a) (Patch (Eot a))) => Patch (Eot a) -> a -> a
gpatch pa a = fromEot $  pa <>> toEot a

\end{code}

\section{Diff using GHC Generics so I can work on recursive types}



\section{Custom Diff instance for numbers}
\begin{code}
newtype NumDelta a = NumDelta a deriving(Eq, Show, Ord)
type instance Patch Int = NumDelta Int
instance Num a => Monoid (NumDelta a) where
    mappend (NumDelta a) (NumDelta b) = NumDelta $ a + b
    mempty = NumDelta 0

instance Num a => MonoidAction (NumDelta a) a where
    NumDelta a <>> b = a + b

instance Num a => MonoidTorsor (NumDelta a) a where
    a <-> b = NumDelta (a - b)

instance Num a => Diff a (NumDelta a)
instance Num a => GDiff a (NumDelta a)
\end{code}

\section{Diffs for recursive types}

\section{Efficient diffs exploiting sparsity}


% https://github.com/RafaelBocquet/haskell-mgeneric/
\section{Simple uses of code}
\begin{code}
data Foo = FooL { ca :: (), cb :: () } | 
    FooR {cc :: Int, cd :: ()} deriving(Show, Generic)
data Bar a = Bar a deriving(Show, Generic)
-- The fact that a new Data declaration works strongly suggests
-- that my type family should become a data family.

-- This is fucked, because EOT cannot do recursive types properly.
-- I need to switch to GHC generics, it looks like :(
data Nat = Z | S (Nat) deriving(Show, Generic)

{--
*Main> :kind! (Patch (Eot Nat))
(Patch (Eot Nat)) :: *
= EitherPatch
    ()
    (Either (Nat, ()) Void)
    ()
    (EitherPatch (Nat, ()) Void (Patch Nat, ()) Void)

This is a problem, because "Patch (Eot Nat)" refers to "Patch Nat".
I can't equationally define "Patch Nat = Patch (Eot Nat)" because GHC
reasons, so I think I will need to jump GHC generics hoops.
--}

n1 :: Nat
n1 = S Z
n2 :: Nat
n2 = S (S Z)

-- diffn1n2 = gdiff n1 n2


el :: Either () ()
el = Left ()

er :: Either () ()
er = Right ()

diffeler = gdiff el el

foo1 = FooL () ()
foo2 = FooR 5 ()

difffoo1foo1 = gdiff foo1 foo1
difffoo1foo2 = gdiff foo1 foo2
 
deltaint :: NumDelta Int
deltaint = 4 <-> 5


-- deltatuple :: NumDelta (Int, Int)
deltatuple = gdiff (3 :: Int, 2 :: Int) (2, 3)


l2 :: Either Int Int
l2 = Left 2


l3 :: Either Int Int
l3 = Left 3


r2 :: Either Int Int
r2 = Right 2

r3 :: Either Int Int
r3 = Right 3


deltall = gdiff l2 l3
deltalr = gdiff l2 r3
deltarl = gdiff r2 l3
deltarr = gdiff r2 r3

\end{code}

\section{Checkpoints}

Now that we have the infrastructure to talk about diffs, we can now
recreate certain well-known patterns with this machinery: One of them
being that of the command pattern, which allows for undo and redo-ing
of operations. An implementation would be:

\begin{code}
-- time of creation. TODO: should wrap some kind of non
-- monotonic structure.
newtype Time = Time { unTime :: Int } deriving(Show)
incrTime :: Time -> Time
incrTime (Time i) = Time (i + 1)

instance P.Pretty Time where
    pretty (Time t) = P.pretty "t" <> P.pretty t

instance {-# OVERLAPS #-} P.Pretty a => Show a where
    show a = let doc = P.layoutPretty P.defaultLayoutOptions (P.pretty a)
        in P.renderString doc

-- | TODO: think of the invariants here. Do we want to start with knowing
-- | the first value? I think we do
-- | Collects the last timestamp used, the current saved path in terms
-- | of a path of @s@, the previous @a@ value that was used to create the
-- | @s@ at the tip of the path,  and the current value being used @a@
data ChkTrace s a = ChkTrace !Time !(LCA.Path s) a a

instance (P.Pretty s, P.Pretty a) => P.Pretty (ChkTrace s a) where
    pretty (ChkTrace time path _ _) = P.vcat $ map P.pretty (LCA.toList path)


newChkTrace :: s -> a -> ChkTrace s a
newChkTrace s a = ChkTrace (Time 1) (LCA.cons 0 s LCA.empty) a a

-- | Compute a new value from the value at the tip of the chkTrace
computeTipChkTrace :: (a -> a) -> ChkTrace s a -> ChkTrace s a
computeTipChkTrace f (ChkTrace t p mprev a) =  ChkTrace t p mprev (f a)

-- | TODO: think of tradeoff. We can either only keep deltas
-- | and recompute the current "last known previous value" based on deltas?
-- | save the tip / commit the tip to memory
saveTipChkTrace :: (a -> a -> s)  -- Compute the current tip as a function 
                                        -- of the *previous value* and the *current value*
    -> ChkTrace s a 
    -> ChkTrace s a
saveTipChkTrace f (ChkTrace t p aprev a) = 
    let t' = incrTime t
        p' = LCA.cons (unTime t) (f aprev a) p
    in ChkTrace t' p' a a

data ChkInstr a where
    -- sequences two computations 
    ISequence :: ChkInstr a  -> ChkInstr a  -> ChkInstr a
    -- takes an instruction and checkpoints the final value.
    -- This can be extracted out after running the computation.
    IChk :: ChkInstr a
    -- branches off the computations
    IBranch :: ChkInstr a -> ChkInstr a -> ChkInstr a
    -- computes a pure function over the computation.
    ICompute :: (a -> a) -> ChkInstr a

instance P.Pretty (ChkInstr a) where
    pretty (ISequence a b) = P.hsep [P.pretty a, P.pretty b]
    pretty (IBranch a b) = P.vsep [P.pretty a, P.pretty b]
    pretty (IChk) = P.pretty "CHK"
    pretty (ICompute f) = P.pretty "compute"


-- run it using the most naive algorithm possible
runChkInstrsNaive :: ChkInstr a -> a -> [ChkTrace a a]
runChkInstrsNaive i  a = go i [newChkTrace a a] where
    go :: ChkInstr a -> [ChkTrace a a] -> [ChkTrace a a]
    go (ICompute f) trs =  map (computeTipChkTrace f) trs
    go (IChk) trs =  map (saveTipChkTrace (\_ a -> a)) trs
    go (ISequence i i') trs = go i' . go i $ trs 
    go (IBranch i i') trs =  (go i trs) ++ (go i' trs)

-- Reminder when I come back:
-- DISABLE CODE, ENABLE ONCE I IMPORT CORRECT DELTA IMPLEMENTATION
-- run the machinery if Diff is available.
runChkInstrsDelta :: (Monoid p, GDiff a p) => a -> ChkInstr a -> [ChkTrace p a]
runChkInstrsDelta a i = go i [newChkTrace mempty a] where
    -- go :: ChkInstr a -> [ChkTrace p a] -> [ChkTrace p a] 
    go (ICompute f) trs = map (computeTipChkTrace f) trs
    go (IChk) trs =  map (saveTipChkTrace (\aprev a -> gdiff a aprev)) trs
    go (ISequence i i') trs = go i' . go i $ trs 
    go (IBranch i i') trs =  (go i trs) ++ (go i' trs)

    

seqChkInstrs :: [ChkInstr a] -> ChkInstr a
seqChkInstrs (xs) = foldl1 ISequence xs

chkInstrsProg1 :: ChkInstr String
chkInstrsProg1 = seqChkInstrs [IChk, ICompute (++ "-a-"), IChk, ICompute (++ "-b-"), IChk]


chkInstrsProg2 :: ChkInstr String
chkInstrsProg2 = seqChkInstrs [IChk, ICompute (++ "-a-"), IChk, 
    IBranch (seqChkInstrs [ICompute (++ "-b1-"), IChk]) (seqChkInstrs [ICompute (++ "-b2-"), IChk])]


runChkProgram :: P.Pretty a => String -> ChkInstr a -> a -> IO ()
runChkProgram name p start = do
    putStrLn $ "===" ++ "running program: " ++ name ++ "==="
    let trs = runChkInstrsNaive p start
    pprint p

    putStrLn "* trace:"
    pprint trs

runChkPrograms :: IO ()
runChkPrograms = do
    runChkProgram "prog1" chkInstrsProg1 "start-"
    runChkProgram "prog2" chkInstrsProg2 "start-"

\end{code}



\section{Efficient encoding of deltas}
Till now, we have not actually discussed the cost of these encodings. 
However, we now have the ability to do so. Our main tool will be
to exploit the sparsity of the representation of deltas to decide
their encoding. For this task, we will reserve $c$ bits, where $c$
is the number of components in the delta. These will tell us if
the $i$th component has changed in the representation. Indeed, one
could imagine alternate representations. One can choose to generalize
over all possible representations using a Huffman code, but we shall not
do so here.



\section{Invertible diffs}
\begin{code}
class Monoid g => Group g where
    ginv :: g -> g

-- add new laws
class (Group g, MonoidAction g s) => GroupAction g s | s -> g where


-- add new laws
class (MonoidTorsor g s, GroupAction g s) => GroupTorsor g s | s -> g where
\end{code}

\begin{code}
-- | pretty print
pprint :: P.Pretty a => a -> IO ()
pprint a = do 
    P.putDocW 80 . P.pretty $ a
    putStrLn ""

main :: IO ()
main = do
    runChkPrograms
    WG.main
\end{code}


%% Acknowledgments
\begin{acks}                            %% acks environment is optional
                                        %% contents suppressed with 'anonymous'
  %% Commands \grantsponsor{<sponsorID>}{<name>}{<url>} and
  %% \grantnum[<url>]{<sponsorID>}{<number>} should be used to
  %% acknowledge financial support and will be used by metadata
  %% extraction tools.
  This material is based upon work supported by the
  \grantsponsor{GS100000001}{National Science
    Foundation}{http://dx.doi.org/10.13039/100000001} under Grant
  No.~\grantnum{GS100000001}{nnnnnnn} and Grant
  No.~\grantnum{GS100000001}{mmmmmmm}.  Any opinions, findings, and
  conclusions or recommendations expressed in this material are those
  of the author and do not necessarily reflect the views of the
  National Science Foundation.
\end{acks}


%% Bibliography
%\bibliography{bibfile}


%% Appendix
\appendix
\section{Appendix}

Text of appendix \ldots

\end{document}

