From oleg-at-okmij.org Tue May 10 23:11:06 2005
To: haskell@haskell.org
Subject: Two-hole zippers and transactions of various isolation modes
From: oleg-at-pobox.com
Message-ID: <20050511061106.95395ABCC@Adric.metnet.navy.mil>
Date: Tue, 10 May 2005 23:11:06 -0700 (PDT)
Status: OR


In this part about zippers with two holes, we discuss the relationship
between zippers and (database) transactions of various isolation
modes. We show the updating enumerator and the corresponding zipper
that maximally preserve sharing and can walk terms with directed
loops. We demonstrate that a zipper can convert a (sequential) map to
a fold.


First we need to determine what a zipper with two holes -- two
updating cursors into the same data structure -- actually mean. Here
are a few plausible semantics:

1. We traverse and update the data structure with one zipper
first. After we're finished, we `open' another cursor on the result
and do another updating traversal.

2. We treat the term as if each subterm were mutable. Each cursor
may navigate to and mutate any subterm.

3. Two cursors operate on essentially separate copies of the same data
structure. The movement and updates by one cursor have no effect on the
other.  This semantics of complete isolation and treating a data
structure _as if_ it existed in two separate copies has been already
implemented in the first part. It is mentioned here for completeness
only: normally we do not wish to fork different versions of data, so
some interaction between updating traversals is required.


Semantics 1 corresponds to a so-called serializable isolation
mode. Two transactions essentially run sequentially. The challenge
here is to permit two transactions run in parallel but still assure
the serializable semantics.

Semantics 2 corresponds to a so-called `Dirty Read' (ANSI Read
Uncommitted) isolation mode -- that is, no isolation at all. Updates
by one transaction are instantly visible to the others.

There are points in-between: In Committed Read, updates by one
transaction are not visible to the others until that transaction
commits. In Repeatable Read, transaction 1 will see the updates of the
committed transaction 2 only in the parts of the structure that
transaction 1 hasn't visited yet. That is, one can't affect the past.

Obviously, the serializable isolation mode permits the least
concurrency -- it is a challenge even to permit any concurrency at
all. The dirty read mode allows the most concurrency -- often at a
peril to the programmer. Indeed, imagine what happens when we modify a
subterm with one cursor and truncate the term at the root with the
other cursor. Where is the first cursor to return to? Database manuals
are filled with horror stories of one transaction wishing to modify
rows that a concurrently running transaction has already deleted (see
PostgreSQL 8.0 documentation, Chapter 12.2 and especially Chapter
12.4).


As it turns out, zippers can support all these isolations modes, even
at a finer granularity: subtransactions. We can indeed traverse a term
_as if_ all of its subterms were mutable -- although none are.  We can
use either the `push' mode -- one cursor broadcasts its updates to the
others -- or the pull mode. A cursor may chose to broadcast
accumulated updates (rather than every update) -- at arbitrary
checkpoints of its own choosing. A cursor may examine updates made by
the other cursor and perhaps disregard some of them -- or apply in a
different order, after its own updates.  We are using terms zipper,
cursor and transaction somewhat interchangeably -- for a reason that
any update made through a zipper is `instantly' reversible. If we
remember the value of the zipper at the beginning or other suitable
point, we can in no time return to the state of the data at that
point. Also, delimited continuations let us view a single-thread
system as cooperatively multi-threading one.

The implementation of some database management systems does share
similarities with zipper-like traversals. For example, in Illustra (a
richer, short-lived brother of PostgreSQL), updates are done
functionally rather than destructively. Old versions of data were
preserved, so to permit the implementation of isolation modes,
roll-backs and time travel. Garbage collection -- vacuuming -- though
was not automatic.

As before, all our zippers are generic with respect to the data
structure: we do not need to derive data constructors that depend on
the term in question. Also, we can handle terms that are general
graphs with directed loops, rather than trees.


The design space is quite large and there is no hope of covering it
all in this message. Therefore, in this message we show an updating
traversal function that underlies all the zippers, and a dumb zipper,
on which we build smarter one- and two-hole zippers.  The two-hole
zipper in this message is not very good -- but it is the most explicit
and the easiest to explain (and besides, this code was written first,
a couple of weeks ago). The other messages, if any, will show one-hole
zippers with smarts built-in. We can the realize two-hole zippers of
various isolation modes, including the most challenging ones. The
smarter zippers indeed use multi-prompt delimited continuations in
non-trivial ways, with one delimited continuation capturing a _part_
of another.


A zipper is as good as the underlying traversal function. We start
therefore with an improved term enumerator that maximally preserves
sharing. If no updates were done, the result of the traversal is the
original term itself -- rather than its copy. Furthermore, this
property holds for each subterm. The new traversal function lets us
operate on subterms in pre-order, in-order, or post-order. More
importantly, it lets us effectively span a `canonical' spanning tree
on a term, so each node can be unambiguously identified. We do not
require equality on (sub)terms.

> {-# OPTIONS -fglasgow-exts #-}
> module Zipper2 where
>
> -- Part of the Dybvig, Sabry, Peyton-Jones' library
> -- adjusted from being a monad to be a monad transformer
> import CC_2CPST
>
> import Control.Monad.Identity
> import Control.Monad.Trans


We will be using the same term as in Part 1:

> data Term = Var String | A Term Term | L String Term
> data Direction = Down | DownRight | Up | Next deriving (Eq, Show)

Instead of directions Down and DownRight we could've defined a single
direction `Down Int', with `Down 0' meaning descent to the first child
subterm, `Down 1' meaning descent to the second child subterm etc. The
direction `Next' means to proceed in the `natural' (depth-first)
order.

The following function does an updating traversal of `term' in an
arbitrary monad. The user-supplied function `tf' receives the current
term and the direction how the current term was reached. That
direction is never `Next'. For a composite term, `tf' will normally be
called several times: once with the direction `Down' or `DownRight'
(when we first descended into that term), and once or more in the
direction `Up' (after we have finished walking each of the term's
children). The function `tf' returns a pair: Maybe newTerm and the new
Direction. The newTerm replaces the current term; the new Direction
tells where to go afterwards.

> traverse :: (Monad m) =>
> 	    (Direction -> Term -> m (Maybe Term, Direction)) -> Term -> m Term
> traverse tf term = traverse' id Down term >>= maybeM term id
>     where traverse' next_dir init_dir term =
> 	      do
> 	      (term', direction) <- tf init_dir term
> 	      let new_term = maybe term id term'
> 	      select (next_dir direction) new_term >>= maybeM term' Just
> 	    select Up t = return Nothing
> 	    select Next t@(Var _) = return Nothing
> 	    select dir t@(L v t1) | dir == Next || dir == Down =
> 	      do
> 	      t' <- traverse' id Down t1 >>= (return . fmap (L v))
> 	      traverse' (next Up) Up (maybe t id t') >>= maybeM t' Just
> 	    select DownRight t@(A t1 t2) =
> 	      do
> 	      t' <- traverse' id DownRight t2 >>=
> 	            (return . fmap (\t2'->(A t1 t2')))
> 	      traverse' (next Up) Up (maybe t id t') >>= maybeM t' Just
> 	    select dir t@(A t1 t2) | dir == Next || dir == Down =
>               do
> 	        t' <- traverse' id Down t1 >>=
> 	                     (return . fmap (\t1'->(A t1' t2)))
> 	        traverse' (next DownRight) Up (maybe t id t') >>=
> 		      maybeM t' Just
>
>           next next_dir dir = if dir == Next then next_dir else dir
> 	    maybeM onn onj v = return $ maybe onn onj v

As you can see, if we descend to a term `A t1 t2' which `tf' leaves
unmodified, traverse `t1' and `t2', again with no updates, check `A t1
t2' again and decline any modifications -- the result is the
original subterm `A t1 t2' itself, rather than its copy. When `tf' does
return a new term, we have to rebuild some subterms, but only to the
minimal extent necessary. So, the new term will share as much as
possible with the original one.

Our sample terms are the P2 numeral term1, which prints as

	\f.\x.((f \f.(f \f.\x.x)) ((f \f.\x.x) x))

and a tree spanning infinitely in depth and in breadth:

> term2 = L "f" (A (A f (A term2 f)) (A term2 f)) where f = Var "f"

If interpreted in lambda-calculus, this term satisfies the equality
	term2 f = f (term2 f) (term2 f)
which makes it sort of a wide Y combinator, with recursion
implemented by sharing. It is not a good idea to print term2.

We can traverse term1 in the Identity monad

> testt1 = runIdentity (traverse (\_ term -> return (Nothing,Next)) term1)

or, more illustratively, in the IO monad, so we can print the
encountered subterms:

> testt2 = traverse tf term1
>     where tf dir term = do print dir; print term; return (Nothing,Next)

We can also modify the term:

> testt4 = runIdentity (traverse tf term1)
>     where tf _ (L "x" (Var "x")) = return (Just (L "y" (Var "y")),Up)
> 	    tf _ _ = return (Nothing,Next)

the result is term1 with all occurrences of \x.x replaced with \y.y.

The infinite term2 is harder to traverse (and avoid looping). Zipper
is far better for that. In general, we observe that the enumerator
like `traverse' is better for context-insensitive and bulk
transformations. OTH, Zipper is better for making few and
highly-context-sensitive updates.

Zipper is an updating cursor into the data structure:

> data Zipper r m term dir = 
>    Zipper dir term (CC r m (Maybe term, dir) -> CC r m (Zipper r m term dir))
>  | ZipDone term

The `ZipDone' alternative represents the `End-of-File'.  As in part 1,
Zipper is a recursive data type representing the delimited
continuation of traversal. Unlike Gerard Huet's zipper, our zipper is
completely polymorphic with respect to the term and the traversal
direction. Our zipper is a derivative of the traversal function rather
than of a data structure itself:

> zip'term :: (Monad m, Monad (CC r m)) =>
> 	    ((dir -> term -> CC r m (Maybe term, dir)) -> term -> CC r m term)
> 	    -> term -> CC r m (Zipper r m term dir)
> zip'term enumerator term = 
>        promptP (\p -> enumerator (tf p) term >>= (return . ZipDone))
>     where tf p dir term = shiftP p (\k -> return (Zipper dir term k))

We can use zipper to do the full traversal, printing out subterms:

> lprint x = liftIO $ print x
>
> zip'through (ZipDone term) = lprint "Done" >> lprint term
> zip'through (Zipper dir term k) = do lprint dir; lprint term
> 				       nz <- k (return (Nothing,Next))
> 				       zip'through nz
> tz1 :: IO () = runCC (zip'term traverse term1 >>= zip'through)


More illustrative however are partial traversals with updates. The
term term2 makes especially good example as its full traversal is
impractical. 

> tz3 :: IO ()
>     = runCC (
> 	 do
> 	 zipper <- zip'term traverse term2
> 	 let max_depth = 5
> 	 t <- traverse_replace max_depth zipper 0
> 	 lprint "Final"; lprint t)
>       where 
>       traverse_replace max_depth (Zipper dir term k) depth = 
> 	  do
> 	  let new_depth = update_depth dir depth
> 	  let loop z = traverse_replace max_depth z new_depth
> 	  if new_depth <= max_depth then k (return (Nothing, Next)) >>= loop
> 	     else case term of
>                      L "f" _ -> k (return (Just (L "f" (Var "f")),Up)) >>=
> 				loop
> 		       _ -> k (return (Nothing, Next)) >>= loop
>       traverse_replace max_depth (ZipDone term) depth = return term
>
>       update_depth Up = (+ (-1))
>       update_depth _  = (+ 1)

In test tz3, we truncate term2 at depth 5 -- that is, we replace
\f.something forms that occur deeper than 5 levels with just \f.f. The
result

  \f.((f (\f.((f (\f.f f)) (\f.f f)) f)) (\f.((f (\f.f f)) (\f.f f)) f))

is a finite term, the result of a `partial unrolling'.  This is the
example of a highly context-sensitive update: we have to keep track of
the context (the subterm depth) and update only particular subterms
that occur at particular depths.

The example tz3 -- especially the function traverse_replace --
illustrates using zipper to implement `fold' over a data structure. We
explicitly carry the state of the traversal, the current depth. In
contrast, the original enumerator, `traverse', did _not_ permit the
function `tf' to pass any state among its invocations. It appears then
that a zipper lets us convert a map to a fold! That seems paradoxical
-- indeed, map can be evaluated in parallel -- until we realize that
only a specific kind of map can be converted to fold. Namely, a
monadic (aka, sequential, or serialized) map. Such a map already has
the state being passed from one invocation of the mapping function to
the other. This state is the `stack' -- an implicit argument to every
function. Delimited continuations have made this threaded-through
argument explicit and let us piggy-back our own state on it.


As an example of such a state is the current location in the term.  We
take note of the values of tf's dir argument to compute the path of
the current node -- the sequence of Down or DownRight moves from the
root to the current node.

We define a `higher-order' zipper:

> data ZipperD r m term dir = ZD{ zd_z:: Zipper r m term dir,
> 				  zd_path :: [dir] }
> zd_term ZD{ zd_z = Zipper _ term _ } = term
> zd_dir  ZD{ zd_z = Zipper dir _ _ }  = dir

the constructor function

> zipd'term enumerator term = 
>     zip'term enumerator term >>= (\z -> return $ ZD z [])

and the function to move and update the cursor, keeping track of the
path:

> zipd'move dir nt ZD{zd_z = Zipper _ _ k, zd_path = path} =
>     do z1 <- k (return (nt,dir))
>        return $
> 	   case (z1,path) of
> 	    (Zipper Up _ _,(_:rpath)) -> ZD z1 rpath
> 	    (Zipper dir _ _,_)        -> ZD z1 (dir:path)
> 	    (ZipDone _,[])            -> ZD z1 []

and the `end-of-file' function

> zipd'result ZD{zd_z = ZipDone term} = Just term
> zipd'result _ = Nothing

Now we can traverse the term and print the path to each node during
the depth-first traversal.

> tz4 :: IO () = runCC (
> 	 do
> 	 zd <- zipd'term traverse term1
> 	 let loop zd = maybe (traverse zd) final (zipd'result zd)
> 	     final term = do lprint "Finished"; lprint term
> 	     traverse zd@ZD{zd_z = Zipper dir term _, zd_path = path} =
> 	         do lprint $ (show dir) ++ "->" ++ (show path)
> 	            lprint term
> 	            zipd'move Next Nothing zd >>= loop
> 	 loop zd)

Here's an excerpt from tz4 output:

	"DownRight->[DownRight,Down,Down,Down]"
	\f.(f \f.\x.x)
	"Down->[Down,DownRight,Down,Down,Down]"
	(f \f.\x.x)
	"Down->[Down,Down,DownRight,Down,Down,Down]"
	f
	"Up->[Down,DownRight,Down,Down,Down]"
	(f \f.\x.x)
	"DownRight->[DownRight,Down,DownRight,Down,Down,Down]"
	\f.\x.x

In the next messages, we show how to push the path accumulation into
the zipper itself. For now, we will use ZipperD to implement a
two-hole zipper. This is not the best zipper, but it is more explicit
and easier to explain. Our Zipper2 is a push-mode zipper with a
particular isolation mode. We have two cursors. Updates made with
cursor1 are immediately propagated to cursor2. Updates made with
cursor2 are invisible to cursor1. The user of Zipper2 had better move
cursor2 slower than cursor1: cursor1 can modify cursor2's past and
thus cause a temporal paradox.

Zipper2 is just a pair of ZipperDs:

> data Zipper2 r m term dir = Zipper2 (ZipperD r m term dir)
> 				      (ZipperD r m term dir)
> make'zip2 term = do z1 <- zipd'term traverse term
> 		      z2 <- zipd'term traverse term
> 		      return $ Zipper2 z1 z2

As we know, two different zippers traversing the same data structure
are completely isolated. We wish to break the isolation, to let one
zipper see updates of the other. The key insight is that a zipper sees
its own updates. Therefore, to let z1 push its updates to z2, the
cursor z1, having received a new term from the user, not only updates
its own data structure. It also moves z2 to the position that matches
z1's position, pushes the update, and then returns z2 to its original
place. If z1 modifies a subterm in z2's future, this procedure is
safe. The path accumulation is the key: each cursor always knows where
it is. Cursor z1 also knows where z2 is. So z1 can move z2 arbitrarily
through the tree, provided it returns the cursor where it found it.
Cursor z1 moves z2 in an optimal way, rather than in the depth-first
traversal way.  This all works provided that the user treats Zipper2
as an abstract data type and does not, for example, references z1 and
z2 individually. In the next messages, we relax this requirement, and
so permit two seemingly independent and independently created cursors
to communicate.

The following code implements the simple idea of Zipper2:

> zip2'move1 Nothing dir (Zipper2 z1 z2) = 
>     do
>     lprint $ "move z1, no updates, " ++ (show dir) ++ "; path " ++
> 	     (show $ zd_path z1)
>     lprint $ zd_term z1
>     z1 <- zipd'move dir Nothing z1
>     return $ Zipper2 z1 z2
>
> zip2'move1 (Just nt) dir (Zipper2 z1 z2) = 
>     do
>     lprint $ "move z1, update, " ++ (show dir); lprint nt
>     z2 <- update_z2 z1 z2 nt          -- z1 updating z2
>     z1 <- zipd'move dir (Just nt) z1  -- before updating itself
>     return $ Zipper2 z1 z2
>
> zip2'move2 nt dir (Zipper2 z1 z2) = 
>     do
>     lprint $ "move z2 " ++ (show dir) ++ "; path " ++ (show $ zd_path z2)
>     lprint $ zd_term z2
>     z2 <- zipd'move dir nt z2
>     return $ Zipper2 z1 z2
>
> update_z2 z1 z2 nt = move_to_path target_path z2 >>=
> 		     zipd'move Up (Just nt) >>= move_to_path orig_path >>=
> 		     restore_state (zd_dir z2)
>     where orig_path = zd_path z2 ; target_path = zd_path z1
>
> move_to_path target_path z =
>     if target_path == zd_path z then return z
>     else if lsp > ltp then zipd'move Up Nothing z >>=
> 	                   move_to_path target_path
>     else if lsp < ltp then zipd'move (head target_path) Nothing z >>=
> 	                   move_to_path target_path
>     else if lsp == ltp && not (head sp == head target_path) then
> 	 zipd'move Up Nothing z >>=
> 	 move_to_path (tail target_path) >>=
> 	 zipd'move (head target_path) Nothing
>     else error $ "irreconcilable paths: " ++ (show sp) ++ " and "
> 	         ++ (show target_path)
>   where sp  = zd_path z
>         lsp = length sp
> 	  ltp = length target_path
>
> restore_state dir z =
>     case (dir, zd_dir z) of
> 	(Up,Up) -> return z
> 	(Up,_)  -> zipd'move Down Nothing z >>= zipd'move Up Nothing
> 	(_,Up)  -> do z <- zipd'move Up Nothing z 
> 		      maybe (zipd'move dir Nothing z) (zipd'term traverse)
> 			    (zipd'result z)
>       _       -> return z

We only need a couple of functions to zip through

> zip2'through1 zz@(Zipper2 z1 z2) | Just term <- zipd'result z1 =
>     do lprint $ "Done1: " ++ (show term); return zz
> zip2'through1 z = zip2'move1 Nothing Next z >>= zip2'through1
>
> zip2'through2 zz@(Zipper2 z1 z2) | Just term <- zipd'result z2 =
>     do lprint $ "Done2: " ++ (show term); return zz
> zip2'through2 z = zip2'move2 Nothing Next z >>= zip2'through2

and we can show an example:

> tt2 :: IO () = runCC (
> 	 make'zip2 term1 >>=
> 	 zip2'move1 Nothing Next >>=
> 	 zip2'move2 Nothing Next >>=
> 	 zip2'move1 Nothing Next >>=
> 	 zip2'move1 Nothing Next >>=
> 	 zip2'move1 (Just (A (Var "y") (Var "y"))) Up >>=
> 	 zip2'move2 Nothing Next >>=
> 	 zip2'move2 Nothing Next >>=
> 	 zip2'move2 Nothing Up   >>=
> 	 zip2'move2 Nothing Next >>=
> 	 zip2'move2 (Just (A (Var "z") (Var "z"))) Up >>=
> 	 zip2'through1 >>= zip2'through2 >> return ())

We use cursor 1 to navigate to a subterm and update it; we use cursor
2 to navigate and update. The final result of cursor 1 is

  \f.\x.((y y) ((f \f.\x.x) x))

and of cursor 2 is

  \f.\x.((y y) (z z))

The result of cursor 1 reflects only its own updates. The result of
cursor 2 two reflects the updates by cursor 1 and the updates made by
cursor 2 itself. It appears as if we first updated the term with
cursor 1 and only afterwards with cursor 2. The printed trace however
demonstrates that updates do proceed concurrently, so to speak. We
move both cursors first. We make the update with cursor 1. We navigate
with cursor 2 -- which already sees the update. We make another
update. We move cursor 1, which does _not_ see the update by cursor 2.


Appendix. For the sake of completeness

> instance Show Term where
>     showsPrec _ (Var v) = (v ++)
>     showsPrec _ (L v body) = ("\\"++) . (v ++) . ("."++) . 
> 			     (showsPrec 10 body)
>     showsPrec _ (A t1 t2) = ("("++) . showsPrec 5 t1 . (" "++) . 
> 			              showsPrec 5 t2 . (")"++)
>
> term1 = L "f" (L "x" (A (A f (L "f" (A f (L "f" (L "x" x)))))
> 			  (A (A f (L "f" (L "x" x))) x)))
> 	 where [f,x] = map Var ["f","x"]


Control operators

> abortP :: Monad m => Prompt r a -> CC r m a -> CC r m b
> abortP p e = letSubCont p (\_ -> e)
>
> promptP :: Monad m => (Prompt r a -> CC r m a) -> CC r m a
> promptP f = do p <- newPrompt; pushPrompt p (f p)
>
> shiftP :: Monad m => 
> 	  Prompt r b -> ((CC r m a -> CC r m b) -> CC r m b) -> CC r m a
> shiftP p f = letSubCont p $ \sk -> 
>                pushPrompt p (f (\c -> 
>                  pushPrompt p (pushSubCont sk c)))

