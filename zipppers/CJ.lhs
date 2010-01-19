%-----------------------------------------------------------------------------
%
%               Template for LaTeX Class/Style File
%
% Name:         sigplanconf-template.tex
% Purpose:      A template for sigplanconf.cls, which is a LaTeX 2e class
%               file for SIGPLAN conference proceedings.
%
% Author:       Paul C. Anagnostopoulos
%               Windfall Software
%               978 371-2316
%               paul@windfall.com
%
% Created:      15 February 2005
%
%-----------------------------------------------------------------------------

%options ghci

%if False

> {-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
> module CJ where

> import GHC.Prim
> import Control.Monad.Instances

%endif

\documentclass[preprint]{sigplanconf}



\usepackage{amsmath}
\usepackage{natbib}
\bibpunct();A{},
\let\cite=\citep

\usepackage{stmaryrd,wasysym,url,upgreek,eepic}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt
\DeclareMathAlphabet{\mathkw}{OT1}{cmss}{bx}{n}

\newcommand{\ignore}[1]{}

\newcommand{\F}{\mathsf}

%subst keyword a = "\mathkw{" a "}"
%subst conid a = "\mathsf{" a "}"

\renewcommand{\commentbegin}{\!\!\{\!-}
\renewcommand{\commentend}{\!-\!\}\;}

\begin{document}

\ignore{\eval{3}}

\conferenceinfo{POPL '08}{date, City.} 

\copyrightyear{2008} 

\copyrightdata{[to be supplied]} 



\titlebanner{A POPL Pearl Submission}        % These are ignored unless

\preprintfooter{Clowns and Jokers}   % 'preprint' option specified.



\title{Clowns to the Left of me, Jokers to the Right}

\subtitle{Dissecting Data Structures}



\authorinfo{Conor McBride}
           {University of Nottingham}
           {ctm@@cs.nott.ac.uk}

\maketitle

\begin{abstract}

  This paper, submitted as a `pearl', introduces a small but useful
  generalisation to the `derivative' operation on datatypes
  underlying Huet's notion of
  `zipper'~\cite{huet:zipper,conor:derivative,dfordata}, giving a
  concrete representation to one-hole contexts in data which is in
  \emph{mid-transformation}. This operator, `dissection', turns a
  container-like functor into a bifunctor representing a one-hole
  context in which elements to the left of the hole are distinguished
  in type from elements to its right.

  I present dissection for polynomial functors, although it is
  certainly more general, preferring to concentrate here on its
  diverse applications. For a start, map-like operations over the
  functor and fold-like operations over the recursive data structure
  it induces can be expressed by tail recursion alone. Moreover, the
  derivative is readily recovered from the dissection, along with
  Huet's navigation operations. A further special case of dissection,
  `division', captures the notion of \emph{leftmost} hole,
  canonically distinguishing values with no elements from those with
  at least one. By way of a more practical example, division and
  dissection are exploited to give a relatively efficient generic
  algorithm for abstracting all occurrences of one term from another
  in a first-order syntax.

  The source code for the paper is available online\footnote{\url{http://www.cs.nott.ac.uk/~ctm/CloJo/CJ.lhs}} and compiles with
  recent extensions to the Glasgow Haskell Compiler.

\end{abstract}

%I promise to find out what these mean.
%\category{CR-number}{subcategory}{third-level}
%\terms
%term1, term2
%\keywords
%keyword1, keyword2


\section{Introduction}

%format Expr2 = Expr
%format eval2 = "\F{eval}"
%format eval = "\F{eval}"
%format hload = "\F{load}"
%format hunload = "\F{unload}"
%format load = "\F{load}"
%format unload = "\F{unload}"
%format Either (p) (q) = p + q
%format e1 = "e_1"
%format e2 = "e_2"
%format v1 = "v_1"
%format v2 = "v_2"

There's an old \emph{Stealer's Wheel} song with the memorable chorus:
\begin{quote}
\begin{tabular}{@@{}r@@{}l}
\emph{`}&\emph{Clowns to the left of me, jokers to the right,}~~~~~~~~~~~~\\
&\emph{Here I am, stuck in the middle with you.'}\\
\multicolumn{2}{r}{Joe Egan, Gerry Rafferty}
\end{tabular}
\end{quote}

In this paper, I examine what it's like to be stuck in the middle of
traversing and transforming a data structure. I'll show both you and
the Glasgow Haskell Compiler how to calculate the datatype of a
`freezeframe' in a map- or fold-like operation from the datatype being
operated on. That is, I'll explain how to compute a first-class data
representation of the control structure underlying map and fold
traversals, via an operator which I call \emph{dissection}. Dissection turns
out to generalise both the \emph{derivative} operator underlying
Huet's `zippers'~\cite{huet:zipper,conor:derivative} and the notion of
\emph{division} used to calculate the non-constant part of a polynomial.
Let me take you on a journey into the algebra and differential calculus
of data, in search of functionality from structure.

Here's an example traversal---evaluating a very simple
language of expressions:

> data Expr2 = Val Int | Add Expr2 Expr2
>
> eval :: Expr2 -> Int
> eval (Val i)      = i
> eval (Add e1 e2)  = eval e1 + eval e2

What happens if we freeze a traversal? Typically, we shall have
one piece of data `in focus', with unprocessed data ahead of us and
processed data behind. We should expect something a bit like Huet's
`zipper' representation of one-hole contexts~\cite{huet:zipper}, but
with different sorts of stuff either side of the hole.

%format Left = L
%format Right = R

In the case of our evaluator, suppose we proceed
left-to-right. Whenever we face an |Add|, we must first go left into
the first operand, recording the second |Expr2| to process later; once
we have finished with the former, we must go right into the second
operand, recording the |Int| returned from the first; as soon as we
have both values, we can add. Correspondingly, a |Stack| of these
direction-with-cache choices completely determined where we are in the
evaluation process. Let's make this structure
explicit:\footnote{For brevity, I write |Either| for \(\mathsf{Either}\),
|Left| for \(\mathsf{Left}\) and |Right| for \(\mathsf{Right}\)}

> type Stack = [Either Expr2 Int]

Now we can implement an `|eval| machine'---a tail recursion,
at each stage stuck in the middle with an expression to decompose,
|load|ing the stack by going left, or a value to use, |unload|ing the
stack and moving right.

> eval2 :: Expr2 -> Int
> eval2 e = hload e []
>
> hload :: Expr2 -> Stack -> Int
> hload (Val i)      stk = hunload i stk
> hload (Add e1 e2)  stk = hload e1 (Left e2 : stk)
>
> hunload :: Int -> Stack -> Int
> hunload v   []                 = v
> hunload v1  (Left e2   : stk)  = hload e2 (Right v1 : stk)
> hunload v2  (Right v1  : stk)  = hunload (v1 + v2) stk

Each layer of this |Stack| structure is a \emph{dissection} of
|Expr2|'s recursion pattern. We have two ways to be stuck in the
middle: we're either |Left e2|, on the left with an |Expr2|s waiting
to the right of us, or |Right v1|, on the right with an |Int| cached
to the left of us.  Let's find out how to do this in general,
calculating the `machine' corresponding to any old fold over finite
first-order data.


\section{Polynomial Functors and Bifunctors}

%format (Sum1 (f) (g)) = f "+_{\!1}" g
%format (Prod1 (f) (g)) = f "\times_{\!1}" g
%format (P1 (x) (y)) = ( x "\,,_1" y)
%format id = "\F{id}"
%format L1 = L "_{1}"
%format R1 = R "_{1}"
%format K1 = "\mathsf{K}_1"

This section briefly recapitulates material which is quite
standard.  I hope to gain some generic leverage by exploiting the
characterisation of recursive datatypes as fixpoints of polynomial
functors. For more depth and detail, I refer the reader to the
excellent \emph{`Algebra of Programming'}~\cite{bird.demoor:algebra}.

If we are to work in a generic way with data structures, we need to
present them in a generic way. Rather than giving an individual |data|
declaration for each type we want, let us see how to build them from a
fixed repertoire of components.  I'll begin with the \emph{polynomial}
type constructors in \emph{one} parameter. These are generated by constants,
the identity, sum and product. I label them with a \({}_1\) subscript
to distinguish them their bifunctorial cousins.

> data K1 a           x = K1 a                   -- constant
> data Id             x = Id x                   -- element
> data ((Sum1 p q))   x = L1 (p x) | R1 (q x)    -- choice
> data ((Prod1 p q))  x = P1 (p x) (q x)         -- pairing

Allow me to abbreviate one of my
favourite constant functors, at the same time bringing it into line
with our algebraic style.

%format One = "\mathsf{1}_1"

> type One = K1 ()

Some very basic `container' type constructors can be expressed as
polynomials, with the parameter giving the type of `elements'.
For example, the |Maybe| type constructor gives a choice between
`|Nothing|', a constant,  and `|Just|', embedding an element.

< type Maybe = Sum1 One Id
<
< Nothing  = L1 (K1 ())
< Just x   = R1 (Id x)


Whenever I reconstruct a datatype from this kit, I shall make a habit
of `defining' its constructors \emph{linearly} in terms of the kit
constructors. To aid clarity, I use these \emph{pattern synonyms}
on either side of a functional equation, so that the coded type acquires
the same programming interface as the original. This is not standard
Haskell, but these definitions may readily be expanded to code which is
fully compliant, if less readable.

The `kit' approach allows us to establish properties of whole classes
of datatype at once. For example, the polynomials are all \emph{functorial}:
we can make the standard |Functor| class

%format fmap = "\F{fmap}"

%%%format fmap (f) = "\overline{" f "}"

< class Functor p where
<   fmap :: (s -> t) -> p s -> p t 

respect the polynomial constructs.

> instance Functor (K1 a) where
>   fmap f (K1 a)    = K1 a
>
> instance Functor Id where
>   fmap f (Id s)    = Id (f s)
>
> instance (Functor p, Functor q) => Functor ((Sum1 p q)) where
>   fmap f (L1 p)    = L1 (fmap f p)
>   fmap f (R1 q)    = R1 (fmap f q)
>
> instance (Functor p, Functor q) => Functor ((Prod1 p q)) where
>   fmap f (P1 p q)  = P1 (fmap f p) (fmap f q)

Our reconstructed |Maybe| is functorial without further ado.


\subsection{Datatypes as Fixpoints of Polynomial Functors}

%format Mu f = "\upmu\," f
%format valP = "\F{ValP}"
%format addP = "\F{AddP}"
%format val = "\F{Val}"
%format add = "\F{Add}"

The |Expr2| type is not itself a polynomial, but its branching structure
is readily described by a polynomial. Think of each node of an |Expr2|
as a container whose elements are the immediate sub-|Expr2|s:

> type ExprP = Sum1 (K1 Int) (Prod1 Id Id)
>
> valP i      = L1 (K1 i)
> addP e1 e2  = R1 (P1 (Id e1) (Id e2))

Correspondingly, we should hope to establish the isomorphism
\[
  |Expr| \cong |ExprP Expr|
\]
but we cannot achieve this just by writing

< type Expr = ExprP Expr

for this creates an infinite type expression, rather than an infinite type.
Rather, we must define a recursive datatype which `ties the knot': |Mu p|
instantiates |p|'s element type with |Mu p| itself.

> data Mu p = In (p (Mu p))

Now we may complete our reconstruction of |Expr2|

> type Expr = Mu ExprP
>
> val i      = In (valP i)
> add e1 e2  = In (addP e1 e2)

%format phi = "\phi"
%format (cata f) = "(\!|" f "|\!)"
%format eval3 = eval

Now, the container-like quality of polynomials allows us to define a fold-like
recursion operator for them, sometimes called the \emph{iterator} or
the \emph{catamorphism}.\footnote{Terminology is a minefield here: some people
think of `fold' as threading a \emph{binary} operator through the elements of
a container, others as replacing the constructors with an alternative
algebra. The confusion arises because the two coincide for \emph{lists}.
There is no resolution in sight.} How can we compute a |t| from a |Mu p|?
Well, we can expand a |Mu p| tree as a |p (Mu p)| container of subtrees,
use |p|'s |fmap| to compute |t|s recursively for each subtree, then
post-process the |p t| result container to produce a final result in |t|.
The behaviour of the recursion is thus uniquely determined by the
\emph{|p|-algebra} |phi :: p v -> v| which does the post-processing.

> cata :: Functor p => (p v -> v) -> Mu p -> v
> cata phi (In p) = phi (fmap (cata phi) p)

For example, we can write our evaluator as a catamorphism, with an
algebra which implements each construct of our language
for \emph{values} rather than expressions. The pattern synonyms for
|ExprP| help us to see what is going on:

< eval3 :: Mu ExprP -> Int
< eval3 = cata phi where
<   phi (ValP i)      = i
<   phi (AddP v1 v2)  = v1 + v2

%if False

> eval3 :: Mu ExprP -> Int
> eval3 = cata phi where
>   phi (L1 (K1 i))                = i
>   phi (R1 (P1 (Id v1) (Id v2)))  = v1 + v2

%endif

Catamorphism may appear to have a complex higher-order recursive
structure, but we shall soon see how to turn it into a first-order
tail-recursion whenever |p| is polynomial. We shall do this by dissecting
|p|, distinguishing the `clown' elements left of a chosen position from the
`joker' elements to the right.


\subsection{Polynomial Bifunctors}

Before we can start dissecting, however, we shall need to be able to manage
\emph{two} sorts of element. To this end, we shall need to introduce the
polynomial \emph{bifunctors}, which are just like the functors, but with
two parameters.

%format One2 = "\mathsf{1}_2"
%format K2 = "\mathsf{K}_2"
%format (Sum2 (f) (g)) = f "\,+_{\!2}\," g
%format (Prod2 (f) (g)) = f "\times_{\!2}" g
%format C2 = C "_2"
%format L2 = L "_2"
%format R2 = R "_2"
%format (P2 (x) (y)) = ( x "\,,_2" y )
%format s1 = s "_1"
%format t1 = t "_1"
%format s2 = s "_2"
%format t2 = t "_2"

> data K2 a           x y = K2 a
> data Fst            x y = Fst x
> data Snd            x y = Snd y
> data ((Sum2 p q))   x y = L2 (p x y) | R2 (q x y)
> data ((Prod2 p q))  x y = P2 (p x y) (q x y)
>
> type One2 = K2 ()

We have the analogous notion of `mapping', except that we must
supply one function for each parameter.

%format bimap = "\F{bimap}"

%%%%format bimap (f) (g) = "\overline{" f ";" g "}"

> class Bifunctor p where
>   bimap ::  (s1 -> t1) -> (s2 -> t2) -> p s1 s2 -> p t1 t2
>
> instance Bifunctor (K2 a) where
>   bimap f g (K2 a)    = K2 a
>
> instance Bifunctor Fst where
>   bimap f g (Fst x)   = Fst (f x)
>
> instance Bifunctor Snd where
>   bimap f g (Snd y)   = Snd (g y)
>
> instance  (Bifunctor p, Bifunctor q) =>
>           Bifunctor ((Sum2 p q)) where
>   bimap f g (L2 p)    = L2 (bimap f g p)
>   bimap f g (R2 q)    = R2 (bimap f g q)
>
> instance  (Bifunctor p, Bifunctor q) =>
>           Bifunctor ((Prod2 p q)) where
>   bimap f g (P2 p q)  = P2 (bimap f g p) (bimap f g q)

It's certainly possible to take fixpoints of bifunctors to obtain
recursively constructed container-like data: one parameter stands for
elements, the other for recursive sub-containers. These structures
support both |fmap| and a suitable notion of catamorphism. I can
recommend~\cite{gibbons:ssdgp} as a useful tutorial for this `origami'
style of programming.



\subsection{Nothing is Missing}

We are still short of one basic component: Nothing. We shall be
constructing types which organise `the ways to split at a position',
but what if there are \emph{no} ways to split at a position (because
there are no positions)? We need a datatype to represent impossibility
and here it is:

> data Zero

%format Zero1 = "\mathsf{0}_1"
%format Zero2 = "\mathsf{0}_2"
%format `seq` = "\:" ` "\F{seq}" ` "\:"
%format error = "\F{error}"
%format magic = "\F{magic}"
%format bot = "\bot"
%format inflate = "\F{inflate}"
%format inflateFst = "\F{inflateFst}"
%format inflateSnd = "\F{inflateSnd}"
%format theBigBad = "\F{unsafeCoerce\sharp}"

%if False

> theBigBad :: a -> b
> theBigBad = unsafeCoerce#

%endif

Elements of |Zero| are hard to come by---elements worth speaking of, that is.
Correspondingly, if you have one, you can exchange it for anything you want.

> magic :: Zero -> a
> magic x = x `seq` error "we never get this far"

I have used Haskell's \(\F{seq}\) operator to insist that |magic|
evaluate its argument. This is necessarily
|bot|, hence the |error| clause can never be executed. In effect
|magic| \emph{refutes its input}.

We can use |p Zero| to represent `|p|s with no elements'. For example,
the only inhabitant of |[Zero]| mentionable in polite society is |[]|.
|Zero| gives us a convenient way to get our hands on exactly the
constants, common to every instance of |p|. Accordingly, we should be
able to embed these constants into any other instance:

< inflate :: Functor p => p Zero -> p x
< inflate = fmap magic

However, it's rather a lot of work traversing a
container just to transform all of its nonexistent elements. If we cheat a
little, we can do nothing much more quickly, and just as safely!

> inflate :: Functor p => p Zero -> p x
> inflate = theBigBad

This |theBigBad| function behaves operationally like |\x -> x|, but
its type, |a -> b|, allows the programmer to intimidate the
typechecker into submission. It is usually present but well hidden in
the libraries distributed with Haskell compilers, and its use requires
extreme caution. Here we are sure that the only |Zero| computations
mistaken for |x|s will fail to evaluate, so our optimisation is safe.

Now that we have |Zero|, allow me to abbreviate

> type Zero1  = K1 Zero
> type Zero2  = K2 Zero

%if False

> instance Eq Zero where
>   (==) = magic

%endif


\section{Clowns, Jokers and Dissection}

\setlength{\unitlength}{0.0001in}
\newcommand{\clown}{\mathop{
  \begin{picture}(300,900)(0,0)
    \path(300,0)(0,0)(300,900)
  \end{picture}
}}
\newcommand{\joker}{\mathop{
  \begin{picture}(300,900)(0,0)
    \path(0,0)(300,0)(0,900)
  \end{picture}
}}
\newcommand{\diss}{\mathop{
  \begin{picture}(600,900)(0,0)
    \path(300,900)(0,0)(600,0)(300,900)(300,0)
  \end{picture}
}}

%format p'' = "\hat{p}"
%format q'' = "\hat{q}"

%format Cl x = "\clown" x
%format Jo x = "\joker" x
%format Di x = "\diss" x
%format Diss p p'' = "\diss" p "\mapsto" p''
%format step = "\F{step}"

\newcommand{\edge}{\!\!-\!\!}
\newcommand{\pel}{\bullet}
\newcommand{\pho}{\circ}
\newcommand{\pcl}{\!\blacktriangleleft\!}
\newcommand{\pjo}{\!\blacktriangleright\!}
\newcommand{\pcont}[1]{\{#1\}}

%format pel = "\pel"
%format pcl = "\blacktriangleleft"
%format pjo = "\blacktriangleright"

We shall need three operators which take polynomial functors
to bifunctors. Let me illustrate them: consider functors
parametrised by elements (depicted |pel|) and
bifunctors are parametrised by clowns (|pcl|) to the left and
jokers (|pjo|) to the right. I show a typical |p x| as a container of
|pel|s
\[
  \pcont{\edge\pel\edge\pel\edge\pel\edge\pel\edge\pel\edge}
\]

Firstly, `all clowns' |Cl p| lifts |p| uniformly to the bifunctor
which uses its left parameter for the elements of |p|.
\[
  \pcont{\edge\pcl\edge\pcl\edge\pcl\edge\pcl\edge\pcl\edge}
\]
We can define this uniformly:

> data Cl p c j = Cl (p c)
>
> instance Functor f => Bifunctor (Cl f) where
>   bimap f g (Cl pc) = Cl (fmap f pc)

Note that \(|Cl Id| \cong |Fst|\).

Secondly, `all jokers' |Jo p| is the analogue for the right parameter.
\[
  \pcont{\edge\pjo\edge\pjo\edge\pjo\edge\pjo\edge\pjo\edge}
\]

> data Jo p c j = Jo (p j)
>
> instance Functor f => Bifunctor (Jo f) where
>   bimap f g (Jo pj) = Jo (fmap g pj)

Note that \(|Jo Id| \cong |Snd|\).

Thirdly, `dissected' |Di p| takes |p| to the bifunctor which chooses a
position in a |p| and stores clowns to the left of it and jokers to
the right.
\[
  \pcont{\edge\pcl\edge\pcl\edge\pho\edge\pjo\edge\pjo\edge}
\]
We must clearly define this case by case. Let us work informally and
think through what to do each polynomial type constructor. Constants have no
positions for elements,
\[
  \pcont{\edge}
\]
 so there is no way to dissect them:

< Di (K1 a)       = Zero2

The |Id| functor has just one position, so there is just one way to dissect
it, and no room for clowns or jokers, left or right.
\[
  \pcont{\edge\pel\edge}  \quad \longrightarrow \quad \pcont{\edge\pho\edge}
\]

< Di Id          = One2

Dissecting a |Sum1 p q|, we get either a dissected |p| or a
dissected |q|.
\[
\begin{array}{r@@{\quad\longrightarrow\quad}l}
|L1|\:\pcont{\edge\pel\edge\pel\edge\pel\edge} &
|L2|\:\pcont{\edge\pcl\edge\pho\edge\pjo\edge} \\
|R1|\:\pcont{\edge\pel\edge\pel\edge\pel\edge} &
|R2|\:\pcont{\edge\pcl\edge\pho\edge\pjo\edge} \\
\end{array}\]

< Di ((Sum1 p q))   = Sum2 (Di p) (Di q)

So far, these have just followed Leibniz's rules for the derivative, but for
pairs |Prod1 p q| we see the new twist. When dissecting a pair, we choose
to dissect either the left component (in which case the right component is
all jokers) or the right component (in which case the left component is all
clowns).
\[
(\pcont{\edge\pel\edge\pel\edge\pel\edge} ,_1
 \pcont{\edge\pel\edge\pel\edge\pel\edge})
\longrightarrow
\left\{
\begin{array}{@@{}l@@{}}
|L2|\:(\pcont{\edge\pcl\edge\pho\edge\pjo\edge} ,_2
         \pcont{\edge\pjo\edge\pjo\edge\pjo\edge}) \\
|R2|\:(\pcont{\edge\pcl\edge\pcl\edge\pcl\edge} ,_2
         \pcont{\edge\pcl\edge\pho\edge\pjo\edge}) \\
\end{array}
\right.
\]

< Di ((Prod1 p q))  = Sum2 (Prod2 (Di p) (Jo q)) (Prod2 (Cl p) (Di q))

Now, in Haskell, this kind of type-directed definition can be done
with type-class programming~\cite{hallgren:fundep,conor:faking}. Allow
me to abuse notation very slightly, giving dissection constraints a
slightly more functional notation, after the manner
of~\cite{thiemann:fundep}:

> class (Functor p, Bifunctor p'') => Diss p p'' | p -> p'' where
>   -- methods to follow

In ASCII, |Diss p p''| is rendered relationally as \texttt{Diss p
  p''}, but the annotation \(\mid\)|p -> p''| is a \emph{functional
  dependency}, indicating that |p| determines |p''|, so it is
appropriate to think of |Di| as a functional operator, even if we
can't quite treat it as such in practice.

%format right = "\F{right}"
%format plug = "\F{plug}"

\newcommand{\rightDecl}{

>   right :: Either (p j) ((p'' c j, c)) -> Either ((j, p'' c j)) (p c)

}
\newcommand{\plugDecl}{

>   plug :: x -> p'' x x -> p x

}
%format rightN = "\F{right}"
%format plugN = "\F{plug}"
%format mindp = "\F{mindp}"
%format mindq = "\F{mindq}"

%format hack = "\!\!"

\newcommand{\DissKHead}{

> instance  Diss (K1 a) Zero2

}\ignore{

>  where

}\newcommand{\rightK}{

>   right{-|K1 a|-} x = case x of
>     Left (K1 a)      -> Right (K1 a)
>     Right (K2 z, c)  -> magic z

}\newcommand{\plugK}{

>   plug{-|K1 a|-} x (K2 z) = magic z

}

\newcommand{\DissIdHead}{

> instance  Diss Id One2

}\ignore{

>  where

}\newcommand{\rightId}{

>   right{-|Id x|-} x = case x of
>     Left (Id j)       -> Left (j, K2 ())
>     Right (K2 (), c)  -> Right (Id c)

}\newcommand{\plugId}{

>   plug{-|Id|-} x (K2 ()) = Id x

}

\newcommand{\DissSumHead}{

> instance    (Diss p p'', Diss q q'') =>
>           Diss (Sum1 p q) (Sum2 p'' q'')

}\ignore{

>  where

}\newcommand{\rightSum}{

>   right{-|Sum1 p q|-} x = case x of
>     Left (L1 pj)       -> mindp (right{-|p|-} (Left pj))
>     Left (R1 qj)       -> mindq (right{-|q|-} (Left qj))
>     Right (L2 pd, c)   -> mindp (right{-|p|-} (Right (pd, c)))
>     Right (R2 qd, c)   -> mindq (right{-|q|-} (Right (qd, c)))
>     where
>       mindp (Left (j, pd))  = Left (j, L2 pd)
>       mindp (Right pc)      = Right (L1 pc)
>       mindq (Left (j, qd))  = Left (j, R2 qd)
>       mindq (Right qc)      = Right (R1 qc)

}\newcommand{\plugSum}{

>   plug{-|Sum1 p q|-} x (L2 pd)   = L1 (plug{-|p|-} x pd)
>   plug{-|Sum1 p q|-} x (R2 qd)   = R1 (plug{-|q|-} x qd)

}

\newcommand{\DissProdHead}{

> instance    (Diss p p'', Diss q q'') =>
>           Diss (Prod1 p q) (Sum2 (Prod2 p'' (Jo q)) (Prod2 (Cl p) q''))

}\ignore{

>  where

}\newcommand{\rightProd}{

>   right{-|Prod1 p q|-} x = case x of
>     Left (P1 pj qj)                -> mindp (right{-|p|-} (Left pj))        qj
>     Right (L2 (P2 pd (Jo qj)), c)  -> mindp (right{-|p|-} (Right (pd, c)))  qj
>     Right (R2 (P2 (Cl pc) qd), c)  -> mindq pc (right{-|q|-} (Right (qd, c)))
>     where
>       mindp (Left (j, pd))  qj  = Left (j, L2 (P2 pd (Jo qj)))
>       mindp (Right pc)      qj  = mindq pc (right{-|q|-} (Left qj))
>       mindq pc (Left (j, qd))   = Left (j, R2 (P2 (Cl pc) qd))
>       mindq pc (Right qc)       = Right (P1 pc qc) 

}\newcommand{\plugProd}{

>   plug{-|Prod1 p q|-}  x (L2 (P2 pd (Jo qx)))  = P1 (plug{-|p|-} x pd) qx
>   plug{-|Prod1 p q|-}  x (R2 (P2 (Cl px) qd))  = P1 px (plug{-|q|-} x qd)

}



I shall extend this definition and its instances with operations shortly,
but let's start by translating our informal program into type-class Prolog:

\DissKHead
\vspace*{-0.2in}
\DissIdHead
\vspace*{-0.2in}
\DissSumHead
\vspace*{-0.1in}
\DissProdHead


Before we move on, let us just check that we get the answer we expect for
our expression example.

< Diss (Sum1 (K1 Int) (Prod1 Id Id)) (Sum2 Zero2 (Sum2 (Prod2 One2 (Jo Id)) (Prod2 (Cl Id) One2)))

A bit of simplification tells us:
\[
|Di ExprP Int Expr| \cong |Either Expr Int|
\]
Dissection (with values to the left and expressions to the right) has
calculated the type of layers of our stack!

\section{How to Creep Gradually to the Right}

%format rightb (p) = "\F{right}\{\!\!\!-" p "-\!\!\!\}"
%format left = "\F{left}"

If we're serious about representing the state of a traversal by a dissection,
we had better make sure that we have some means to move from one position to
the next. In this section, we'll develop a method for the |Diss p p''| class
which lets us move |right|ward one position at a time. I encourage you to move
leftward yourselves.

What should be the type of this operation? Consider, firstly, where
our step might start. If we follow the usual trajectory, we'll start
at the far left---and to our right, all jokers.
\[\begin{array}{r@@{}l}
\downarrow \vspace*{-0.08in}\\
& \pcont{\edge\pjo\edge\pjo\edge\pjo\edge\pjo\edge\pjo\edge}
\end{array}
\]
Once we've started our traversal, we'll be in a dissection. To
be \emph{ready} to move, we we must have a clown to put into the hole.
\[\begin{array}{c}
\pcl \vspace*{-0.05in}\\
\downarrow  \vspace*{-0.05in}\\
\pcont{\edge\pcl\edge\pcl\edge\pho\edge\pjo\edge\pjo\edge}
\end{array}\]

Now, think about where our step might take us. If we end up at the next
position, out will pop the next joker, leaving the new hole.
\[\begin{array}{c}
\pcont{\edge\pcl\edge\pcl\edge\pcl\edge\pho\edge\pjo\edge}
\vspace*{-0.05in}\\
\hspace*{0.28in}
 \begin{array}{c}\downarrow  \vspace*{-0.05in}\\
 \pjo
 \end{array}
\end{array}\]
But if there are no more positions, we'll emerge at the far right, all clowns.
\[\begin{array}{r@@{}l}
\pcont{\edge\pcl\edge\pcl\edge\pcl\edge\pcl\edge\pcl\edge}
 \vspace*{-0.08in}\\
& \downarrow
\end{array}
\]

Putting this together, we add to |class Diss p p''| the method

\rightDecl

Let me show you how to implement the instances by pretending to write
a \emph{polytypic} function after the manner of~\cite{jansson.jeuring:polyp},
showing the operative functor in a comment.

< right{-|p|-} hack ::  Either (p j) ((Di p c j, c)) -> Either ((j, Di p c j)) (p c)

You can paste each clause of |right{-\(p\)-} hack| into the corresponding
|Diss p| instance.

For constants, we jump all the
way from far left to far right in one go; we cannot be in the middle,
so we refute that case.

\rightK

We can step into a single element, or step out.

\rightId

For sums, we make use of the instance for whichever branch is appropriate,
being careful to strip tags beforehand and replace them afterwards.

\rightSum

For products, we must start at the left of the first component and end at
the right of the second, but we also need to make things join up in the
middle. When we reach the far right of the first component, we must continue
from the far left of the second.

\rightProd
\par
Let's put this operation straight to work. If we can dissect |p|, then we can
make its |fmap| operation tail recursive. Here, the jokers are the source
elements and the clowns are the target elements.

%format tmap = "\F{tmap}"
%format continue = "\F{continue}"

> tmap :: Diss p p'' => (s -> t) -> p s -> p t
> tmap f ps = continue (right{-|p|-} (Left ps)) where
>   continue (Left (s, pd))  = continue (right{-|p|-} (Right (pd, f s)))
>   continue (Right pt)      = pt

\subsection{Tail-Recursive Catamorphism}

If we want to define the catamorphism via dissection, we could just
replace |fmap| by |tmap| in the definition of |cata|, but that would
be cheating! The point, after all, is to turn a higher-order recursive
program into a tail-recursive machine. We need some kind of
\emph{stack}.

Suppose we have a |p|-algebra, |phi :: p v -> v|, and we're
traversing a |Mu p| depth-first, left-to-right, in order to compute a
`value' in |v|. At any given stage, we'll be processing a given node,
in the middle of traversing her mother, in the middle of traversing her
grandmother, and so on in a maternal line back to the root.
We'll have visited all the nodes left of this line and thus have
computed |v|s for them; right of the line, each node will contain a
|Mu p| waiting for her turn. Correspondingly, our stack is a list of
dissections:
\[
  |[Di p v (Mu p)]|
\]

%format tcata = "\F{tcata}"
%format next = "\F{next}"

We start, ready to |load| a tree, with an empty stack.

> tcata :: Diss p p'' => (p v -> v) -> Mu p -> v
> tcata phi t = load phi t []

To load a node, we unpack her container of subnodes and step in from the
far left.

> load :: Diss p p'' => (p v -> v) -> Mu p -> [p'' v (Mu p)] -> v
> load phi (In pt) stk = next phi (right{-|p|-} (Left pt)) stk

After a step, we might arrive at another subnode, in which case we had
better |load| her, suspending our traversal of her mother by pushing
the dissection on the stack.

> next ::  Diss p p'' => (p v -> v) ->
>          Either ((Mu p, p'' v (Mu p))) (p v) -> [p'' v (Mu p)] -> v
> next phi (Left (t, pd))   stk = load phi t (pd : stk)
> next phi (Right pv)       stk = unload phi (phi pv) stk

Alternatively, our step might have taken
us to the far right of a node, in which case we have all her subnodes' values:
we are ready to apply the algebra |phi| to get her own value,
and start |unload|ing.

Once we have a subnode's value, we may resume the traversal of
her mother, pushing the value into her place and moving on.

> unload :: Diss p p'' => (p v -> v) -> v -> [p'' v (Mu p)] -> v
> unload phi v (pd : stk)  = next phi (right{-|p|-} (Right (pd, v))) stk
> unload phi v []          = v

On the other hand, if the stack
is empty, then we're holding the value for the root node, so we're done!
As we might expect:

%format eval4 = eval

< eval4 :: Mu ExprP -> Int
< eval4 = tcata phi where
<   phi (ValP i)      = i
<   phi (AddP v1 v2)  = v1 + v2

%if False

> eval4 :: Mu ExprP -> Int
> eval4 = tcata phi where
>   phi (L1 (K1 i))                = i
>   phi (R1 (P1 (Id v1) (Id v2)))  = v1 + v2

%endif




\section{Derivative Derived by Diagonal Dissection}

%format plug = "\F{plug}"
%format D p = "\partial" p

The dissection of a functor is its bifunctor of one-hole contexts
distinguishing `clown' elements left of the hole from `joker' elements
to its right. If we remove this distinction, we recover the usual
notion of one-hole context, as given by the
\emph{derivative}~\cite{conor:derivative,dfordata}. Indeed, we've
already seen, the rules for dissection just refine the centuries-old
rules of the calculus with a left-right distinction. We can undo this
refinement by taking the diagonal of the dissection, identifying clowns with
jokers.
\[|D p x| = |Di p x x|\]
Let us now develop the related operations.


\subsection{Plugging In}

We can add another method to |class Diss p p''|,
\plugDecl
saying, in effect, that if clowns and jokers coincide,
we can fill the hole directly and without any need to traverse
all the way to the end. The implementation is straightforward.
\plugK\vspace*{-0.2in}
\plugId\vspace*{-0.2in}
\plugSum\vspace*{-0.15in}
\plugProd


\subsection{Zipping Around}

%format zUp = "\F{zUp}"
%format zDown = "\F{zDown}"
%format zRight = "\F{zRight}"
%format zLeft p= "\F{zLeft}"

We now have almost all the equipment we need to reconstruct Huet's
operations~\cite{huet:zipper}, navigating a tree of type |Mu p| for
some dissectable functor |p|.

> zUp, zDown, zLeft, zRight :: Diss p p'' =>
>   (Mu p, [p'' (Mu p) (Mu p)]) -> Maybe (Mu p, [p'' (Mu p) (Mu p)])

I leave |zLeft| as an exercise, to follow the implementation of the
leftward step operation, but the other three are straightforward
uses of |plug{-\(p\)-} hack| and |right{-\(p\)-} hack|. This implementation
corresponds quite closely to the Generic Haskell version from
\cite{hinze.jeuring.loh:tidts}, but requires a little less machinery.

> zUp (t, [])        = Nothing
> zUp (t, pd : pds)  = Just (In (plug{-|p|-} t pd), pds)
>
> zDown (In pt, pds) = case right{-|p|-} (Left pt) of
>   Left (t, pd)  -> Just (t, pd : pds)
>   Right _       -> Nothing
>
> zRight (t, [])        = Nothing
> zRight (t :: Mu p, pd : pds)  = case right{-|p|-} (Right (pd, t)) of
>   Left (t', pd')         -> Just (t', pd' : pds)
>   Right (_ :: p (Mu p))  -> Nothing

%if False

> zLeft = undefined

%endif

Notice that I had to give the typechecker a
little help in the definition of |zRight|. The trouble is that |Di| is
not known to be \emph{invertible}, so when we say
|right{-\(p\)-} (Right (pd, t))|, the type of |pd| does not actually
determine |p|---it's easy to forget that the |{-\(p\)-}| is only a comment.
I've forced the issue by collecting |p| from the type of the input tree
and using it to fix the type of the `far right' failure case. This is
perhaps a little devious, but when type inference is compulsory, what can
one do?


\section{Division: No Clowns!}

%format Q p = "\ell" p
%format divide = "\F{divide}"
%format edivid = divide "^{\smallsmile}"

The derivative is not the only interesting special case of
dissection.  In fact, my original motivation for inventing dissection
was to find an operator |Q| for `leftmost' on suitable functors |p|
which would induce an isomorphism reminiscent of the `remainder
theorem' in algebra.
\[
  |p x| \;\cong\; |Either ((x, Q p x)) (p Zero)|
\]

This |Q p x| is the `quotient' of |p x| on division by |x|, and it represents
whatever can remain after the \emph{leftmost} element in a |p x| has been
removed. Meanwhile, the `remainder', |p Zero|, represents those |p|s with
no elements at all. Certainly, the finitely-sized containers should give us
this isomorphism, but what is |Q|? It's the context of the leftmost hole.
It should not be possible to move any further left, so there should be
\emph{no clowns}! We need
\[
  |Q p x = Di p Zero x|
\]

For the polynomials, we shall certainly have

> divide :: Diss p p'' => p x -> Either ((x, p'' Zero x)) (p Zero)
> divide px = right{-|p|-} (Left px)

To compute the inverse, I could try waiting for you to implement the leftward
step: I know we are sure to reach the \emph{far} left, for your only
alternative is to produce a clown! However, an alternative is at the ready.
I can turn a leftmost hole into any old hole if I have

> inflateFst  :: Bifunctor p => p Zero y -> p x y
> inflateFst  = theBigBad  -- faster than |bimap magic id|

Now, we may just take

> edivid :: Diss p p'' => Either ((x, p'' Zero x)) (p Zero) -> p x
> edivid (Left (x, pl))  = plug{-|p|-} x (inflateFst pl)
> edivid (Right pz)      = inflate pz

It is straightforward to show that these are mutually inverse by induction
on polynomials.


\section{A Generic Abstractor}

So far this has all been rather jolly, but is it just a mathematical
amusement? Why should I go to the trouble of constructing an explicit
context structure, just to write a fold you can give directly by
higher-order recursion? By way of a finale, let me present a more
realistic use-case for dissection, where we exploit the first-order
representation of the context by inspecting it: the task is to abstract
all occurrences of one term from another, in a generic first-order
syntax.

\subsection{Free Monads and Substitution}

%format Star p = p "^{\ast}\!"
%format sig = "\sigma"
%format return = "\F{return}"
%format </> = "\mathbin{\downharpoonright}"
%format <\> = "\mathbin{\upharpoonright}"
%format <\\> = "\mathbin{\upharpoonright}"

What is a `generic first-order syntax'? A standard way to get hold of
such a thing is to define the \emph{free monad} |Star p| of a
(container-like) functor |p|~\cite{barr&wells:ttt:dist}.

> data Star p x = V x | C (p (Star p x))

The idea is that |p| represents the signature of constructors in our
syntax, just as it represented the constructors of a datatype in the
|Mu p| representation. The difference here is that |Star p x| also contains
\emph{free variables} chosen from the set |x|. The monadic structure
of |Star p| is that of substitution.

> instance Functor p => Monad (Star p) where
>   return x = V x
>   V x   >>= sig = sig x
>   C pt  >>= sig = C (fmap (>>= sig) pt)

Here |>>=| is the \emph{simultaneous} substitution from variables in
one set to terms over another. However, it's easy to build
substitution for a single variable on top of this. If we a term |t|
over |Maybe x|, we can substitute some |s| for the distinguished
variable, |Nothing|. Let us rename |Maybe| to |S|, `successor', for
the occasion:

> type S = Maybe
>
> (</>) :: Functor p => Star p (S x) -> Star p x -> Star p x
> t </> s = t >>= sig where
>   sig Nothing   = s
>   sig (Just x)  = V x

Our mission is to compute the `most abstract' inverse to |(</> s)|, for
suitable |p| and |x|, some

< (<\>) :: ... => Star p x -> Star p x -> Star p (S x)

such that |(t <\> s) </> s = t|, and moreover that |fmap Just s|
occurs \emph{nowhere} in |t <\> s|. In order to achieve this, we've
got to abstract \emph{every} occurrence of |s| in |t| as |V Nothing|
and apply |Just| to all the other variables. Taking |t <\> s = fmap
Just t| is definitely wrong!


\subsection{Indiscriminate Stop-and-Search}

%format presEq (q) (a) (b) = a  "\mathbin{\lceil" q "\rceil}" b
%format presEq2 (q) (r) (a) (b) = a  "\mathbin{\lceil" q "\mid" r "\rceil}" b
%format reverse = "\F{reverse}"
%format hasPrefix = "\F{hasPrefix}"
%format hasSuffix = "\F{hasSuffix}"
%format hasSuffix2 = "\F{hasSuffix}"

The obvious approach to computing |t <\> s| is to traverse |t| checking
everywhere if we've found |s|. We shall need to be able to test
equality of terms, so we first must confirm that our signature functor |p|
\emph{preserves} equality, i.e., that we can lift equality |eq| on |x| to
equality |presEq eq| on |p x|.

> class PresEq p where
>   presEq :: (x -> x -> Bool) -> p x -> p x -> Bool
>
> instance Eq a => PresEq (K1 a) where
>   presEq eq (K1 a1) (K1 a2) = a1 == a2
>
> instance PresEq Id where
>   presEq eq (Id x1) (Id x2) = eq x1 x2
>
> instance (PresEq p, PresEq q) => PresEq ((Sum1 p q)) where
>   presEq eq (L1 p1)  (L1 p2)  = presEq eq p1 p2
>   presEq eq (R1 q1)  (R1 q2)  = presEq eq q1 q2
>   presEq eq _        _        = False
>
> instance (PresEq p, PresEq q) => PresEq ((Prod1 p q)) where
>   presEq eq (P1 p1 q1) (P1 p2 q2) = presEq eq p1 p2 && presEq eq q1 q2
>
> instance (PresEq p, Eq x) => Eq (Star p x) where
>   V x   ==  V y   = x == y
>   C ps  ==  C pt  = presEq (==) ps pt
>   _     ==  _     = False

We can now make our first attempt:

> (<\\>) ::  (Functor p, PresEq p, Eq x) => Star p x -> Star p x -> Star p (S x)
> t     <\\> s | t == s  = V Nothing
> V x   <\\> s           = V (Just x)
> C pt  <\\> s           = C (fmap (<\\> s) pt)

Here, I'm exploiting Haskell's \emph{Boolean guards} to test for a
match first: only if the fails do we fall through and try to search
more deeply inside the term.  This is short and obviously correct, but it's
rather inefficient. If |s| is small and |t| is large, we shall
repeatedly compare |s| with terms which are far too large to stand a
chance of matching. It's rather like testing if |xs| has suffix |ys|
like this.

> hasSuffix :: Eq x => [x] -> [x] -> Bool
> hasSuffix xs        ys | xs == ys  = True
> hasSuffix []        ys             = False
> hasSuffix (x : xs)  ys             = hasSuffix xs ys

If we ask |hasSuffix "xxxxxxxxxxxx" "xxx"|, we shall test if |'x' ==
'x'| thirty times, not three.  It's more efficient to reverse both
lists and check \emph{once} for a \emph{prefix}. With fast |reverse|,
this takes linear time.

> hasSuffix2 :: Eq x => [x] -> [x] -> Bool
> hasSuffix2 xs ys = hasPrefix (reverse xs) (reverse ys)
>
> hasPrefix :: Eq x => [x] -> [x] -> Bool
> hasPrefix xs        []                 = True
> hasPrefix (x : xs)  (y : ys) | x == y  = hasPrefix xs ys
> hasPrefix _         _                  = False

\subsection{Hunting for a Needle in a Stack}

%format leftOrLeaf = "\F{leftOrLeaf}"
%format needle = "\F{needle}"
%format either = "\F{either}"
%format grow = "\F{grow}"
%format hunt = "\F{hunt}"
%format nextStub = "\F{next}"
%format checkStub = "\F{check}"
%format stub = "\cdots"
%format check = "\F{check}"
%format leafS = "\F{leafS}"
%format ^++ = "\mathbin{\uparrow\!\!\!\!+\!\!\!+}"


%if False

> stub = undefined

%endif

\newcommand{\leafEq}{

> instance (PresEq p, Eq x) => Eq (Leaf p x) where
>   VL x  == VL y  = x == y
>   CL a  == CL b  = presEq magic a b
>   _     == _     = False

}\newcommand{\stitch}{

> (^++) :: Bifunctor p => [p Zero y] -> [p x y] -> [p x y]
> [] ^++ pxys = pxys
> (pzy : pzys) ^++ pxys = inflateFst pzy : pzys ^++ pxys

}

We can adapt the `reversal' idea to our purposes.  The |divide| function tells
us how to find the leftmost position in a polynomial container, if it
has one. If we iterate |divide|, we can navigate our way down the left
spine of a term to its leftmost leaf, stacking the contexts as we
go. That's a way to reverse a tree!

A leaf is either a variable or a constant. A term either is a leaf or has
a leftmost subterm. To see this, we just need to adapt |divide| for the
possibility of variables.

> data Leaf p x = VL x | CL (p Zero)
>
> leftOrLeaf ::  Diss p p'' =>
>                Star p x -> Either ((Star p x, p'' Zero (Star p x))) (Leaf p x)
> leftOrLeaf (V x)      = Right (VL x)
> leftOrLeaf (C pt)     = fmap CL (divide pt)

Now we can reverse the term we seek into the form of a `needle'---a leaf
with a straight spine of leftmost holes running all the way back to
the root

> needle :: Diss p p'' => Star p x -> (Leaf p x, [p'' Zero (Star p x)])
> needle t = grow t [] where
>   grow t pls = case leftOrLeaf t of
>     Left (t', pl)    -> grow t' (pl : pls)
>     Right l          -> (l, pls)

Given this needle representation of the search term, we can implement
the abstraction as a stack-driven traversal, |hunt| which tries for a
match only when it reaches a suitable leaf. We need only |check| for our
needle when we're standing at the end of a left spine at least as
long. Let us therefore split our `state' into an inner left spine
and an outer stack of dissections.

> (<\>) ::  (Diss p p'', PresEq p, PresEq2 p'', Eq x) =>
>           Star p x -> Star p x -> Star p (S x)
> t <\> s = hunt t [] [] where
>   (neel, nees) = needle s
>   hunt t spi stk = case leftOrLeaf t of
>     Left (t', pl)  -> hunt t (pl : spi) stk
>     Right l        -> check spi nees (l == neel)
>       where
>         checkStub = stub

Current technology for type annotations makes it hard for me to write
|hunt|'s type in the code. Informally, it's this:

< hunt ::  Star p x -> [Q p (Star p x)] ->  [Di p (Star p (S x)) (Star p x)] ->
<          Star p (S x)

Now, |check| is rather like |hasPrefix|, except that I've used a little
accumulation to ensure the expensive equality tests happen after the cheap
length test.

>         check spi' [] True = next (V Nothing) (spi' ^++ stk)
>         check (spl : spi') (npl : nees') b  =
>           check spi' nees' (b && presEq2 (magic) (==) spl npl)
>         check _ _ _ = next (leafS l) (spi ^++ stk) where
>           leafS (VL x)   = V (Just x)
>           leafS (CL pz)  = C (inflate pz)

For the equality tests we need |presEq2|, the bifunctorial analogue of
|presEq|, although as we're working with |Q p|, we can just use |magic|
to test equality of clowns. The same trick works for |Leaf| equality:

\leafEq

Now, instead of returning a |Bool|, |check| must explain how to \emph{move
  on}. If our test succeeds, we must move on from our matching
subterm's position, abstracting it: we throw away the matching prefix
of the spine and stitch its suffix onto the stack.
However, if the test fails, we must move right from the current \emph{leaf}'s
position, injecting it into |Star p (S x)| and stitching the original
spine to the stack.
Stitching (|^++|) is
just a version of `append' which inflates a leftmost hole to a dissection.

\stitch

Correspondingly, |next| tries to move rightwards
given a `new' term and a stack. If we can go right, we get the next `old' term
along, so we start |hunt|ing again with an empty spine.

>   next t' (pd : stk)  = case right{-|p|-} (Right (pd, t')) of
>     Left (t, pd')   -> hunt t [] (pd' : stk)
>     Right pt'       -> next (C pt') stk
>   next t' []            = t'

If we reach the far right of a |p|, we pack it up and pop on out. If we
run out of stack, we're done!



%if False

> class PresEq2 p where
>   presEq2 :: (x -> x -> Bool) -> (y -> y -> Bool) -> p x y -> p x y -> Bool

> instance Eq a => PresEq2 (K2 a) where
>   presEq2 xeq yeq (K2 a1) (K2 a2) = a1 == a2

> instance PresEq2 Fst where
>   presEq2 xeq yeq (Fst x1) (Fst x2) = xeq x1 x2

> instance PresEq2 Snd where
>   presEq2 xeq yeq (Snd y1) (Snd y2) = yeq y1 y2

> instance (PresEq2 p2, PresEq2 q2) => PresEq2 (Sum2 p2 q2) where
>   presEq2 xeq yeq  (L2 p1)  (L2 p2)  = presEq2 xeq yeq p1 p2
>   presEq2 xeq yeq  (R2 q1)  (R2 q2)  = presEq2 xeq yeq q1 q2
>   presEq2 xeq yeq  _        _        = False

> instance (PresEq2 p, PresEq2 q) => PresEq2 (Prod2 p q) where
>   presEq2 xeq yeq (P2 p1 q1) (P2 p2 q2) =
>     presEq2 xeq yeq p1 p2 && presEq2 xeq yeq q1 q2

> instance PresEq p => PresEq2 (Cl p) where
>   presEq2 ceq jeq (Cl pc1) (Cl pc2) = presEq ceq pc1 pc2

> instance PresEq p => PresEq2 (Jo p) where
>   presEq2 ceq jeq (Jo pj1) (Jo pj2) = presEq jeq pj1 pj2

%endif

\section{Discussion}
%format Comp f g = f "\circ_1" g
%format Comp2 f (g) (h) = f "\circ_2 (" g ";" h ")"

The story of dissection has barely started, but I hope I have
communicated the intuition behind it and sketched some of its
potential applications.  Of course, what's missing here is a more
\emph{semantic} characterisation of dissection, with respect to which
the operational rules for |Di p| may be justified.

It is certainly straightforward to give a shapes-and-positions
analysis of dissection in the categorical setting of
\emph{containers}~\cite{alti:cont-tcs}, much as we did with the
derivative~\cite{dfordata}. The basic point is that where the
derivative requires element positions to have decidable
\emph{equality} (`am I in the hole?'), dissection requires a total
order on positions with decidable \emph{trichotomy} (`am I in the
hole, to the left, or to the right?'). The details, however, deserve a
paper of their own.

I have shown dissection for polynomials here, but it is clear that
we can go much further. For example, the dissection of \emph{list} gives
a list of clowns and a list of jokers:

< Di [] = Prod2 (Cl []) (Jo [])

Meanwhile, the \emph{chain rule}, for functor composition, becomes

< Di (Comp p q) = Prod2 (Di q) (Comp2 (Di p) (Cl q) (Jo q))

where

< data (Comp2 p q r) c j = Comp2 (p (q c j) (r c j))

That is, we have a dissected |p|, with clown-filled |q|s left of the
hole, joker-filled |q|s right of the hole, and a dissected |q| in the hole.
If you specialise this to \emph{division}, you get
\[
  |Q (Comp p q) x| \cong |Q q x| \times |Di p (q Zero) (q x)|
\]
The leftmost |x| in a |p (q x)| might not be in a leftmost |p| position:
there might be |q|-leaves to the left of the |q|-node containing the first
element. That is why it was necessary to invent |Di| to define |Q|,
an operator which deserves further study in its own right. For finite
structures, its iteration gives rise to a power series formulation of
datatypes directly,
finding all the elements left-to-right, where iterating |D| finds them
in any order. There is thus a significant connection with the notion
of \emph{combinatorial species} as studied by Joyal~\cite{Joyal:analytique}
and others.

The whole development extends readily to the \emph{multivariate} case,
although this a little more than Haskell can take at present. The general
\(\diss_i\) dissects a
mutli-sorted container at a hole of sort |i|, and splits all the sorts
into clown- and joker-variants, doubling the arity of its parameter.
The corresponding \(\ell_i\) finds the contexts in which an element of
sort |i| can stand leftmost in a container. This corresponds exactly to
Brzozowski's notion of the `partial derivative' of a regular
expression~\cite{brz}.

But if there is a message for programmers and programming language
designers, it is this: the miserablist position that types exist only
to police errors is thankfully no longer sustainable, once we start
writing programs like this. By permitting calculations of types and
from types, we discover what programs we can have, just for
the price of structuring our data. What joy!

\appendix



%\acks







\bibliographystyle{plainnat}
\bibliography{CJ}


\end{document}

1-59593-090-6/05/0007