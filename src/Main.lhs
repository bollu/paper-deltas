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
-- Flexible instances for:
-- src/Main.lhs:216:20: error:
--     * Illegal instance declaration for MonoidAction (Patch a) a
--         (All instance types must be of the form (T a1 ... an)
--          where a1 ... an are *distinct type variables*,
--          and each type variable appears at most once in the instance head.
--          Use FlexibleInstances if you want to disable this.)
--     * In the instance declaration for MonoidAction (Patch a) a
--     |
-- 216 | instance Diff a => MonoidAction (Patch a) a where
--     |                    ^^^^^^^^^^^^^^^^^^^^^^^^
-- 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Monoid
import Generics.Eot

\end{code}

Next, we define the typeclass which we will constantly end up using:
\begin{code}
-- forall s. mact mempty s = s
-- forall m1 m2 s. (m1 <> m2) <>> s = m2 <>> (m1 <>> s)
class Monoid m => MonoidAction m s | s -> m where
    mact :: m -> s -> s
    mact = (<>>)
    -- (<>>) :: MonoidAction m s => m -> s -> s
    (<>>) :: m -> s -> s
    (<>>) = mact


-- forall s. mdelta s s = mempty
-- forall s1 s2. (s2 <-> s1) <>> s1 = s2
class MonoidAction m s => MonoidTorsor s m | s -> m where
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
class Diff a p | a -> p, p -> a where
    patchempty :: p
    patchappend :: p -> p -> p
    diff :: a -> a -> p
    -- default diff :: (Has a, Diff ( a)) => a -> a -> Patch ( a)
    -- diff a a' = diff (to a) (to a')

    patch :: p -> a -> a

instance Diff a p => Monoid p where
    mempty = patchempty
    mappend = patchappend

instance Diff a p => MonoidAction p a where
    (<>>) = patch

instance Diff a p => MonoidTorsor a p where
    (<->) = diff
    
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

type instance Patch (Either a b) = EitherPatch a b (Patch a) (Patch b)
instance (Diff a pa, Diff b pb) => Diff (Either a b) (EitherPatch a b pa pb) where
    patchempty = PPL (patchempty)
    patchappend _ (PL a) = (PL a)
    patchappend _ (PR b) = (PR b)
    patchappend (PPL pa) (PPL pa') = PPL (pa <> pa')
    patchappend (PPR pb) (PPR pb') = PPR (pb <> pb')
    patchappend x y = error $ "unable to combine patches!"
    diff (Left a) (Left a') = PPL (diff a a')
    diff (Right b) (Right b') = PPR (diff  b b')
    diff (Left _) (Right b) = PR b
    diff (Right _) (Left a) = PL a
    patch (PL a) _ = Left a
    patch (PR b) _ = Right b
    patch (PPL pa) (Left a)  = Left (pa <>> a)
    patch (PPR pb) (Right b)  = Right (pb <>> b)


type instance Patch () = ()
instance Diff () () where
    patchempty = ()
    patchappend _ _ = ()
    diff _ _ = ()
    patch _ _ = ()

type instance Patch (x, xs) = (Patch x, Patch xs)
instance (Diff x p , Diff y q) => Diff (x, y) (p, q) where
    patchempty = (patchempty, patchempty)
    patchappend (p1, p1') (p2, p2') = (patchappend p1 p2, patchappend p1' p2')
    diff (x1, x1') (x2, x2') = (diff x1 x2, diff x1' x2')
    patch (p, p') (x, x') = (patch p x, patch p' x')

\end{code}

\begin{code}
class Monoid g => Group g where
    ginv :: g -> g

class GroupAction g s | s -> g where
    gact :: g -> s -> s

(<*>>) :: GroupAction g s => g -> s -> s
(<*>>) = gact


class GroupAction g s => GroupTorsor s g | s -> g where
    gdelta :: s -> s -> g


(<*->) :: GroupTorsor s g => s -> s -> g
(<*->) = gdelta
\end{code}

\begin{code}
main :: IO ()
main = print "foo"
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

