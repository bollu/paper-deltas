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
{-# LANGUAGE EmptyCase  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE Rank2Types  #-}
import Prelude hiding ((.))
import Data.Monoid
import Control.Arrow
import Control.Category
-- import Generics.Eot
import GHC.Generics
import GHC.Exts (Constraint)
import Data.String.Utils
import qualified Language.Haskell.TH as TH
import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Prettyprint.Doc.Util as P
import qualified Data.Text.Prettyprint.Doc.Render.String as P
import qualified WorkingGenerics as WG
import qualified Data.FingerTree as FT
import qualified Data.LCA.Online as LCA
import qualified Data.LCA.View as LCA
-- Test harness
import Test.Tasty
import qualified Test.QuickCheck as QC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Modifiers (NonEmptyList (..))
import Test.Tasty.HUnit
import Test.QuickCheck.Function

\end{code}
Helpers for pretty printing

Next, we define the typeclass which we will constantly end up using:

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
% https://stackoverflow.com/questions/18965938/data-family-default-instances

We now have a class \hsmint{Diff}, which represents a type that
has a structure which can be diffed. We have an associated data
family \hsmint{Patch}, which is the type of patches for a diff. We
then instantiate the boilerplate for the correct \hsmint{Monoid},
\hsmint{MonoidAction}, and \hsmint{MonoidTorsor} defintions.


We now use the \texttt{Generics} support in haskell to construct a method
to derive the \hsmint{Diff} instance for any algebraic data type.

\begin{code}
data Void  deriving(Generic)
absurd :: Void -> a
absurd v = case v of

-- | TODO: deal with partial diff and patch. Diff should be @a -> a -> Maybe (Patch a)@
class Diff a where
  type family Patch a :: *
  type Patch a = GPatch (Rep a) a

  diff :: a -> a -> Patch a
  default diff :: (Generic a, GDiff (Rep a), Patch a ~ (GPatch (Rep a)) a) => a -> a -> Patch a
  diff a a' = gdiff (from a) (from a')

  papply :: Patch a -> a -> a
  default papply :: (Generic a, GDiff (Rep a), Patch a ~ (GPatch (Rep a)) a) => Patch a -> a -> a
  papply p a = to (gpapply p (from a))
  -- default apply = undefined

-- operator for diff
(<->) :: Diff a => a -> a -> Patch a
(<->) = diff


-- operator for patch
(+$) :: Diff a => Patch a -> a -> a
(+$) = papply

class GDiff (gen :: * -> *)  where
  type family GPatch gen :: * -> *
  gdiff :: gen a -> gen a -> (GPatch gen) a
  gpapply :: (GPatch gen) a -> gen a -> gen a

instance GDiff V1 where
  type GPatch V1 = V1 
  gdiff v1 _ = undefined

  gpapply v1 _ = undefined

instance GDiff U1 where
  type GPatch U1 = U1
  gdiff u1 u1' = u1
  gpapply u1 _ = u1

-- products
instance (GDiff f, GDiff g) => GDiff (f :*: g) where
  type GPatch (f :*: g) = (GPatch f :*: GPatch g) 
  gdiff (f :*: g) (f' :*: g') = (gdiff f f') :*: (gdiff g g')
  gpapply (pf :*: pg) (f :*: g) = (gpapply pf f) :*: (gpapply pg g)


instance (GDiff f, GDiff g) => GDiff (f :+: g)  where
  type GPatch (f :+: g) = (f :+: g :+: GPatch f :+: GPatch g)
  gdiff (L1 f) (L1 f') = R1 (R1 (L1 (gdiff f f')))
  gdiff (R1 g) (R1 g') = R1 (R1 (R1 (gdiff g g')))
  gdiff (L1 f) (R1 g) = (L1 f)
  gdiff (R1 g) (L1 f) = R1 (L1 g)

  gpapply (L1 f) (R1 g) = L1 f 
  gpapply (R1 (L1 g)) (L1 f) = R1 g

  gpapply (R1 (R1 (L1 pf))) (L1 f) = L1 (gpapply pf f)
  gpapply (R1 (R1 (R1 pg))) (R1 g) = R1 (gpapply pg g)

-- meta info, we simply tunnel through
instance (GDiff f) => GDiff (M1 i t f)  where
  type GPatch (M1 i t f) =  M1 i t (GPatch f)
  gdiff (M1 x) (M1 x') = M1 $ gdiff x x'

  gpapply (M1 px) (M1 x) = M1 $ gpapply px x


-- recursion
-- TODO: This does not resolve! Factor out recursion separately and then
-- define it
instance (Diff f) => GDiff (K1 i f) where
   type GPatch (K1 i f) = (K1 i (Patch f))
   gdiff (K1 x) (K1 x') = K1 $ diff x x'

   gpapply (K1 p) (K1 x) = K1 $ papply p x


-- class Diff2 (d :: * -> *) where
--     type Patch2 d :: * -> *
--     diff2 :: forall a. Diff a => d a -> d a -> Patch2 d (Patch a)
--     apply2 :: forall a. Diff a => Patch2 d (Patch a) -> d a -> Maybe (d a)

-- Define Diff on free f a
data Free f a = Leaf a | Branch (f (Free f a)) deriving(Functor, Generic)
data FreeDiff a b c d = 
    Leaf2Leaf a |
    Leaf2Branch b |
    Branch2Leaf c |
    Branch2Branch d deriving(Show)

newtype FCompose f g a = FCompose { getFCompose :: f (g a) } deriving(Functor, Show)

data DiffMaybe a da = Nothing2Just | Just2Just da | Just2Nothing a | Nothing2Nothing deriving(Show)


mf1 :: Free Maybe Int
mf1 = Branch (Just (Branch Nothing))

mf2 :: Free Maybe Int
mf2 = Branch (Just (Branch (Just (Leaf 5))))

mf1_sub_mf2 = Branch (Just2Just (Branch Nothing2Just))
mf2_sub_mf1 = Branch (Just2Nothing ((Branch (Just (Leaf 5)))))

mf3 :: Free Maybe Int
mf3 = Branch (Just (Branch (Just (Leaf 6))))

mf4 :: Free Maybe Int
mf4 = Branch (Just (Branch (Just (Leaf 9))))


a1 :: Free (FCompose (FreeDiff (NumDelta Int) Int (Free Maybe Int)) (DiffMaybe Int)) (NumDelta Int)
a1 = Leaf (NumDelta 3)




a2 :: Free (FCompose (FreeDiff (NumDelta Int) Int (Free Maybe Int)) (DiffMaybe Int)) (NumDelta Int)
a2 = Branch (FCompose (Leaf2Branch 10))


a3 :: Free (FCompose (FreeDiff (NumDelta Int) Int (Free Maybe Int)) (DiffMaybe Int)) (NumDelta Int)
a3 = Branch (FCompose (Branch2Leaf ((Branch (Just (Leaf 5))))))

a4 :: Free (FCompose (FreeDiff (NumDelta Int) Int (Free Maybe Int)) (DiffMaybe Int)) (NumDelta Int)
a4 = Branch (FCompose (Branch2Branch (Nothing2Just)))


a5 :: Free (FCompose (FreeDiff (NumDelta Int) Int (Free Maybe Int)) (DiffMaybe Int)) (NumDelta Int)
a5 = Branch (FCompose (Branch2Branch (Just2Nothing 30)))


a6 :: Free (FCompose (FreeDiff (NumDelta Int) Int (Free Maybe Int)) (DiffMaybe Int)) (NumDelta Int)
a6 = Branch (FCompose (Branch2Branch (Just2Just x))) where
    x = Leaf (NumDelta 10)



mcom1 :: Free ((,) Int) Int
mcom1 = Branch (10, Branch (20, Leaf 5))

mcom2 :: Free ((,) Int) Int
mcom2 = Branch (20, Branch (30, (Branch (40, Leaf 5))))

-- mcom1_sub_mcom2 :: Free ((,) Int) _
-- mcom1_sub_mcom2 = Branch (-10, Branch (-10, Leaf2Branch 5))

mlist1 :: Free [] Int
mlist1 = Branch [Branch [Leaf 1, Leaf 2], Leaf 3]


mlist2 :: Free [] Int
mlist2 = Branch [Branch [Leaf 1, Leaf 2], Branch [Leaf 4, Leaf 5], Leaf 5]


class Diff2 (f :: * -> *) where
    type family Patch2 f :: (* -> * -> *)
    diff2 :: (a -> a -> da) -> f a -> f a -> Patch2 f a da
    patch2 :: (da -> a -> a) -> Patch2 f a da -> f a -> f a

instance Diff2 Maybe where
    type Patch2 Maybe = DiffMaybe

    diff2 _ Nothing Nothing = Nothing2Nothing
    diff2 _ Nothing (Just a) = Nothing2Just 
    diff2 _ (Just a) Nothing  = Just2Nothing a
    diff2 f (Just a) (Just a')  = Just2Just (f a a')

    patch2 _ Nothing2Nothing Nothing = Nothing
    patch2 _ Nothing2Just (Just x) = Nothing
    patch2 _ (Just2Nothing a) Nothing = Just a
    patch2 f (Just2Just da) (Just a) = Just $ f da a


data DiffTuple da b db = DiffTuple da db deriving(Show)

instance Diff a => Diff2 ((,) a) where
    type Patch2 ((,) a) = DiffTuple (Patch a)
    diff2 f (a, b) (a', b') = DiffTuple (diff a a') (f b b')
    patch2 f (DiffTuple da db) (a, b) = (da +$ a, f db b)

type FreePatch f a = Free 
            (FCompose 
                (FreeDiff (Patch a) a (Free f a))
                (Patch2 f (Free f a)))
            (Patch a)

freeDiff :: Diff a => Diff2 f => 
    Free f a -> Free f a -> 
        Free 
            (FCompose 
                (FreeDiff (Patch a) a (Free f a))
                (Patch2 f (Free f a)))
            (Patch a)
-- a <-> b
freeDiff (Leaf a) (Leaf a') = Leaf (diff a a')
freeDiff (Leaf a) (Branch a') = Branch $ FCompose (Leaf2Branch a)
freeDiff br@(Branch a) (Leaf a') = Branch $ FCompose (Branch2Leaf br)
freeDiff (Branch br) (Branch br') = Branch $ FCompose $ Branch2Branch $ diff2 freeDiff br br'


-- p +$ b
freeApply :: (Diff a, Diff2 f) => FreePatch f a -> Free f a -> Free f a
freeApply (Leaf da) (Leaf a) = Leaf (da +$ a)
freeApply (Branch (FCompose (Leaf2Branch x))) _ =  Leaf x
freeApply (Branch (FCompose (Branch2Leaf x))) _ = x
freeApply (Branch (FCompose (Branch2Branch fx))) (Branch ffx) =  
    Branch $ patch2 freeApply fx ffx


instance (Eq a, Eq (f (Free f a))) => Eq (Free f a) where
    (Leaf a) == (Leaf a') = a == a'
    (Branch ffa) == (Branch ffa') = ffa == ffa'
    _ == _ = False

instance (Arbitrary a, Arbitrary (f (Free f a))) => Arbitrary (Free f a) where
    arbitrary = QC.oneof [Leaf <$> arbitrary, Branch <$> arbitrary]


instance (Show a, Show(f (Free f a))) => Show (Free f a) where
    show (Leaf a) =  "(leaf " ++ show a ++ ")"
    show (Branch ffa) =  "(branch " ++ show ffa ++ ")"


instance (P.Pretty a, P.Pretty (f (Free f a))) => P.Pretty (Free f a) where
    pretty (Leaf a) = P.pretty "(leaf " P.<> P.pretty a <> P.pretty ")"
    pretty (Branch ffa) = P.pretty "(branch " P.<> P.pretty ffa P.<> P.pretty ")"

instance Diff () 
instance Diff Void 
instance (Diff a, Diff b) => Diff (a, b) 
instance (Diff a, Diff b) => Diff (Either a b)
instance (Diff a) => Diff (Maybe a)
\end{code}


\begin{comment}
\begin{code}
data Proxy a = Proxy

-- | Quickcheck instance for diff
diffApplyPatch :: (Diff a, Eq a)  => Proxy a -> a -> a -> Bool
diffApplyPatch proxy a b = (b <-> a) +$ a == b

qcDiffApplyPatch :: (Diff a, Eq a, Arbitrary a, Show a) => 
    Proxy a -> String -> TestTree
qcDiffApplyPatch proxy name = 
     QC.testProperty 
        ("[" ++ name ++ "]: b <-> a +$ a == b")
        (diffApplyPatch proxy)

-- Fucked, I need injectivity?
-- Looks like I need to use TemplateHaskell to generate unique instances.
-- Oh well, I had to jump this ship sometime. Better now than never.
applyPempty :: (Diff a, Eq a) => Proxy a -> Patch a -> a -> Bool
applyPempty proxy pa a = (papply pa a) == a

-- qcApplyPempty :: (Diff a, Eq a, Arbitrary a, Show a) =>
--     Proxy a -> String -> TestTree
-- qcApplyPempty proxy name =
--     QC.testProperty (name ++ "pempty +$a == a")
--     (applyPempty proxy)


-- | QuickCheck instance for free diff
freeDiffApplyPatch :: (Diff a, Diff2 f, Eq (Free f a)) => 
    Proxy (Free f a) -> Free f a -> Free f a -> Bool
freeDiffApplyPatch proxy a b = freeApply (freeDiff a b) b == a

qcFreeDiffApplyPatch :: (Show (Free f a), 
    Arbitrary (Free f a), Diff a, Diff2 f, Eq (Free f a)) =>
    Proxy (Free f a)  -> String -> TestTree
qcFreeDiffApplyPatch proxy name = 
     QC.testProperty 
        ("[" ++ name ++ "]: b <-> a +$ a == b")
        (freeDiffApplyPatch proxy)


 
\end{code}
\end{comment}

\section{Diff instances for regular types}

\begin{code}

-- type instance DT Int = (NumDelta Int)
data NumDelta a = NumDelta a deriving(Show)

instance Show a => P.Pretty (NumDelta a) where
    pretty nd = P.pretty (show nd)


instance Diff Int  where
  type Patch Int = (NumDelta Int)
  diff a b =  NumDelta $ a - b
  papply (NumDelta d) a = a + d

  
data Ctor a = Ctor a deriving(Generic, Show)
data Ctor2 a = Ctor2 a deriving(Generic, Show)

instance (Diff a) => Diff (Ctor a) 

data And = And () () deriving(Generic, Show)
data And2 = And2 () () deriving(Generic, Show)
instance Diff And

data Or = One | Two deriving (Generic, Show)
data Or2 = One2 | Two2 | Three2 deriving(Generic, Show)
data OrDiff = OneOne | OneTwo | TwoOne | TwoTwo deriving(Generic, Show)
instance Diff Or 

data Stream a = a :< (Stream a) deriving(Generic, Show)

-- TODO: allow this derivation to work automatically
-- instead of getting stuck
-- instance Diff (Stream a)
instance Diff a => Diff (Stream a) where
  type Patch (Stream a) = Stream (Patch a)
  diff (x:<xs) (x':<xs') = (diff x x'):<(diff xs xs')
  papply (px:<pxs) (x:<xs) = (papply px x):<(papply pxs xs)




-- This 1. conflicts with string, 2. does not work properly.
-- instance {-# OVERLAPS #-}  Diff a => Diff [a] where
--   type Patch [a] = [Patch a]
--   diff (x:xs) (x':xs') = (diff x x'):(diff xs xs')

x :: Int
x = 3

y :: Int
y = 4

dxy :: NumDelta Int
dxy = diff x y


-- dxxyy :: (NumDelta Int, NumDelta Int)
dxxyy = diff (x, x) (y, y)

data Four a b c d = LL a | LR b | RL c | RR d

l :: Either Int Int
l = Left 4

r :: Either Int Int
r = Right 10

dlr = diff l r

\end{code}

\section{Inefficiency of the above encoding}
While the above encoding is clearly sufficient since we consider
all possible cases, is it really the minimal representation?

\begin{theorem}
Minimality of diffs: The diffs that are constructed by the tensoring
process are minimal in terms of size, as counted by number of bits
required for the representation
\end{theorem}
\begin{proof}
TODO
\end{proof}

\begin{theorem}
Types that allow for diff factorization: The class of types
whose diffs can be factors (like \hsmint{Bool}) is: \textbf{TODO}
\end{theorem}
\begin{proof}
TODO
\end{proof}


\section{Checkpoints, Roll forwards}

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

--  instance {-# OVERLAPS #-} P.Pretty a => Show a where
--      show a = let doc = P.layoutPretty P.defaultLayoutOptions (P.pretty a)
--          in P.renderString doc

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

-- TODO: make this use the roll forward not back technology
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
    -- merges two computations
    IMerge :: (a -> b -> c) -> ChkInstr a -> ChkInstr b -> ChkInstr c

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

runChkInstrsDelta :: (Monoid (Patch a), Diff a) => ChkInstr a -> a -> [ChkTrace (Patch a) a]
runChkInstrsDelta i a = go i [newChkTrace mempty a] where
    -- go :: ChkInstr a -> [ChkTrace (Patch a) a] -> [ChkTrace (Patch a) a] 
    go (ICompute f) trs = map (computeTipChkTrace f) trs
    go (IChk) trs =  map (saveTipChkTrace (\aprev a -> diff a aprev)) trs
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

runChkProgramDelta :: 
    (P.Pretty a, P.Pretty (Patch a), Diff a, Monoid (Patch a)) => String -> ChkInstr a -> a -> IO ()
runChkProgramDelta name p start = do
    putStrLn $ "===" ++ "running program: " ++ name ++ "==="
    let trs = runChkInstrsDelta p start
    pprint p

    putStrLn "* trace:"
    pprint trs


newtype StrPrefix = StrPrefix String  deriving(Show, Monoid)
instance P.Pretty StrPrefix where
    pretty (StrPrefix s) = P.pretty "Prefix(" P.<> (P.pretty s) P.<> (P.pretty ")")

-- crazy bad, terribly inefficient, implementation.
instance Diff String where
    type Patch String = StrPrefix
    diff s s' = 
        if startswith s' s
            then StrPrefix $ drop (length s') s
            else error $ "s: |" ++ s ++ "|  s': |" ++ s' ++ "|"

    papply (StrPrefix spr) s' = spr ++ s'
        

runChkPrograms :: IO ()
runChkPrograms = do
    runChkProgram "prog1" chkInstrsProg1 "start-"
    runChkProgramDelta "prog1" chkInstrsProg1 "start-"
    runChkProgram "prog2" chkInstrsProg2 "start-"
    runChkProgramDelta "prog2" chkInstrsProg2 "start-"
\end{code}

\section{Using checkpointing for MCMC}
We use the above constructed checkpointing infrastructre to implement
a lighweight markov-chain-monte-carlo implementation, which saves
the previously seen best results in terms of a diff. This sees
a huge difference in comparison the the naive method.

\textbf{TODO: evaluation section / metrics}

\section{Checkpointing for Hoopl}
We benchmark against the naive implementation of \hsmint{Checkpoint}
in \texttt{Hoopl} versus our optimised implementation. The results are:

\textbf{TODO: evaluation section / metrics}



\section{Automatically deriving delta-driven interpreters}
Recently, React and similar frameworks propose an API where one
pretends to be doing all the work from scratch. However, internal
to the API, only the delta to the previous state is computed and applied.
Here, we show a general framework that can be built on top of the
diff framework, using the Free monad/Cofree comonad pairing.

\begin{code}
-- f := free, g := functor 
bindFree :: Functor g => Free g a -> (a -> Free g b) -> Free g b
bindFree (Leaf a) a2fgb = (a2fgb a) 
bindFree (Branch fga) a2fgb = 
  Branch $ (`bindFree` a2fgb) <$> fga

-- Does this have a better instance?
apFree :: Functor g => Free g (a -> b) -> Free g a -> Free g b
apFree frga2b frga = 
  bindFree frga2b  (\a2b ->
    bindFree frga  (\a -> Leaf $ a2b a))

instance Functor f => Applicative (Free f) where
  pure = Leaf
  (<*>) = apFree

instance Functor f => Monad (Free f) where
 return = pure
 (>>=) = bindFree

-- cofree comonad
data Cofree g a = Cobranch a (g (Cofree g a)) deriving(Functor)

class Comonad w where
  extract :: w a -> a
  (=>>) :: w a -> (w a -> b) -> w b


instance Functor g => Comonad (Cofree g) where
  extract (Cobranch a _) = a
  (cur@(Cobranch a g_cofree_a)) =>> cofree_a_to_b = 
    let cur' = cofree_a_to_b cur 
        next = (fmap (=>> cofree_a_to_b) g_cofree_a)
    in Cobranch cur' next


coiter :: Functor f => (a -> f a) -> a -> Cofree f a
coiter psi a = Cobranch a (coiter psi <$> psi a)
 

class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

data Identity a = Identity a deriving(Show, Functor)

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (Cobranch a  _ ) (Leaf x)  = p a x
  pair p (Cobranch _ fs) (Branch gs) = pair (pair p) fs gs

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = pair (flip p) g f



liftF :: (Functor f) => f a -> Free f a
liftF fa = Branch $ Leaf <$> fa
\end{code}


\textbf{TODO: figure this out. Feels like it is doable, but how precisely?}


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

\begin{code}
pprint :: P.Pretty a => a -> IO ()
pprint a = do 
    P.putDocW 80 . P.pretty $ a
    putStrLn ""

tests :: IO ()
tests = defaultMain $ testGroup "QuickCheck" 
            [qcDiffApplyPatch (Proxy :: Proxy Int) "Int"
            ,qcDiffApplyPatch (Proxy :: Proxy String) "String"
            ,qcDiffApplyPatch (Proxy :: Proxy (Either Int Int)) "Either Int Int"
            ,qcFreeDiffApplyPatch (Proxy :: Proxy (Free Maybe Int)) "Free Maybe Int"
            ,qcFreeDiffApplyPatch (Proxy :: Proxy (Free ((,) Int) Int)) "Free ((,) Int) Int"
            ]

main :: IO ()
main = do
    runChkPrograms
    tests
    putStrLn "yay acme is cool"
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

