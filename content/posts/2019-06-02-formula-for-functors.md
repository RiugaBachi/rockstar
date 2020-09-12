---
title: "The Formula for Functors"
date: 02 June 2019
description: "An explanation of most Haskell functors encountered in the wild, with a focus on breaking down the name of a functor into its individual parts such that one can understand other exotic functors."
tags:
- haskell
- functors
---

Bifunctor. Protofunctor - no, wait - or was it _Profunctor_? Contra-? Co-? ...Variant? It can seem as though the Haskell community  resides atop an ivory tower and drops some of the most _confusing_ names for concepts and abstractions, down to the crowd of laymen at the bottom as some sort of indirect gesture of flexing their intelligence. I can assure you, however, that this is not the case, but a mere perception. My recent Google search shows that there _still_ aren't too many guides out there for laymen regarding anything more advanced than a regular `Functor`, so I've taken it into my own hands to contribute a missing piece to the Haskell community.

In this article, we will cover the common types of functors encountered in the wild, and how their names are constructed. By the end of this article, I hope that you will have gained a better understanding of the differences between the different types of functors, and be able to make sense of their names.

First thing's first: this guide assumes that you have a _basic_ understanding of regular `Functor`s; however, due the analogy that I am going to use for the rest of this post, I will briefly touch up on regular `Functor`s to make sure we're on the same page.

# Functor

A `Functor` simply defines `fmap :: (a -> b) -> f a -> f b`. The commonly used analogy for `Functor`s is a "boxed" or "wrapped" value. While this analogy is valid, I find that it confuses people once they move on to more advanced functors; that is to say, this analogy completely breaks down when talking about `Contravariant`, which we will cover later on.

Let me re-introduce the `Functor` using a new analogy: a machine, with both an input and and output end, however only the output end is chainable to other _function machines_, while the input side can _only insert_ inputs into the machine chain. You may be familiar with this analogy with regards to regular functions, however due to the restriction I placed on the insertion end, it is a bit less flexible than a regular function.

To quickly recap:
1. A `function` is a machine that can take an input and produce an output, and is freely chainable on both ends. We will refer to functions as _function machines_ in the context of our analogy.
2. A `Functor` is a machine that can take an input and produce an output, but only the output end is chainable to _function machines_, while the input end can only insert input values into it. We will explicitly refer to `Functors` as _functor machines_ in our analogy.

`fmap`, then, defines a new _functor machine_ by attaching a _function machine_ to the output side of our original _functor machine_, creating a _new functor machine_ comprised of our two original machines chained together. That's right. We don't think of the machines individually once we chain them; we think of the machine as _one unit_.

Simple enough, right?

## An Aside: The Mythical Cofunctor

Cofunctors seem to confuse newcomers to Haskell to no end.

It's just a joke in the community.

There's your politically incorrect answer.

But briefly, `co-`anything refers to the dual or "inverse" of that thing. Generally this means flipping the function arrows.

Given `fmap :: (a -> b) -> (f a -> f b)` (with explicit parentheses to emphasize currying), the dual of that would be `cofmap :: (a <- b) -> (f a <- f b)`, or, after flipping the arrows back around to fit regular Haskell notation, `cofmap :: (b -> a) -> f b -> f a`. It's not rocket science. It's exactly the same thing minus different type variable names.

TL;DR: `Cofunctor`s are just `Functor`s.

There exists an `acme-cofunctor` library on Hackage which further exacerbates the confusion. It's a _satirical library_; I mean for god's sake, it's prefixed with `acme` for crying out loud. There are other satirical libraries out there, so watch out.

# n-Functors

The next logical step in the functor complexity hierarchy are n-Functors. What's an n-Functor? You already know one! `Functor` is just a 1-Functor; but on a more serious note, you may have heard of `Bifunctor`. This is another example of an n-Functor. Besides `Bifunctor`s, there exists an infinite number of possible n-Functors. "Bi" simply means "two", as you all know. Trifunctors, Quadfunctors, Pentafunctors, etc., all theoretically exist; it's just that we don't encounter them in day-to-day programming, just like how the most common sort of tuple we use is the pair `(_, _)`, not the 3-tuple, 4-tuple, or whatever. Besides, once we can define pairs of something, we can infinitely nest pairs inside one another in order to define arbitrarily "long" (and deep) tuples.

To prove my point, [here's a definition of a Trifunctor encountered in the wild](http://hackage.haskell.org/package/origami-0.0.6/docs/Data-Origami-Internal-Trifunctor.html){target="_blank"}. The function signature for `trimap` is trivial once you understand `Bifunctor` and `bimap`.

## Bifunctors

So let's focus on `Bifunctor`s, as they are the most common type of n-Functor you will encounter. Once you understand `Bifunctor`s, every other n-Functor will make sense to you.

A `Bifunctor` in our analogy can be thought of as a modified _functor machine_, except that it has two _parallel pairs_ of input and output sides, which we refer to as the "left" side and the "right" side. The rule which governs _functor machines_ still apply to each side: only the output ends are chainable with regular _function machines_ and the input ends can only accept raw input. We will call this new machine the _bifunctor machine_.

The definition of `Bifunctor` consists of:

1. `first`, which chains a _function machine_ to the _left_ output of the _bifunctor machine_.
2. `second`, which chains a _function machine_ to the _right_ output of the _bifunctor machine_.
3. `bimap`, which takes two functions and chains the first one to the _left_ output of the _bifunctor machine_, and chains the right one to the _right_ output of the _bifunctor machine_.

That's it. Someone just took two _functor machines_ and wrapped them in a new casing to make it seem like one machine with 2 input ends and 2 output ends. Each side can be thought of as a regular functor. Hell, regular `Functor`s that accept pairs and output pairs are functionally the same as a `Bifunctor`, but a `Bifunctor` abstraction allows for slightly less messy syntax than a `Functor` on pairs. Take for instance, chaining a _function machine_ to a _functor machine operating on pairs_. Mapping one side would look somewhat ridiculous, like `fmap (\(leftOutput, rightOutput) -> (calculateSomething leftOutput, rightOutput))`, which can be reduced to just `fmap (calculateSomething *** id)` by utilizing the `Arrow` instance of regular functions `(->)`.

As you can imagine, Trifunctors, Quadfunctors, Pentafunctors, would just be 3, 4, 5 regular `Functor`s "wrapped" in a new casing, and so on. Again, not rocket science. A Trifunctor, for instance, might define `1map` `2map` `3map` and `trimap`.

# Contravariant Functors

Going back to 1-Functors (`Functor`), there is a sort of "counterpart" to it named `Contravariant`, which is just short for "contravariant functor". This functor is probably the most confusing one out of them all for beginners, since the type signature can make one's head implode _if they are used to the analogy of a "box" or "wrapped value"_.

Let's forget about the "box" or "wrapper" analogy for a second and redefine a `Contravariant` using our analogy of machines. A `Contravariant` functor is one with an _input side that can be chained to regular function machines_, while the output side _can only output a predefined result_. This is sort of a reversal of the rule that governs `Functor`.

The `Contravariant` counterpart to `fmap` is `contramap`. While `fmap` maps the _output_ of a functor to something different, `contramap` maps the input of the functor to something else.

You're probably still confused at this point. Let's take a look at the signature for `contramap`:

`contramap :: (b -> a) -> f a -> f b`

I sympathize with you if this type signature makes no sense. Several questions may be running through your head as you examine this type signature, including:

1. How can we get a wrapped 'b' (`f b`) if we _need_ a `b` in the first place to get an `a`?
2. If we get an `a` via our function, then why do we need to pass in a _wrapped a_ (`f a`)?
3. How does `f a` even get used when our function requires a `b` which can only be obtained via the output of the function?

This is why the analogy of a boxed or wrapped value fails miserably. Forgot about that analogy. Think in terms of machines. The type parameter of regular `Functors` (i.e. the `a` in `f a`) represents the _output type_ of the functor. For `Contravariant` functors, the type parameter represents the _input type_ of the functor.

In the signature for `contramap`, the functor `f a` represents our _original contravariant functor_, constructed via some means (that may not necessarily be via contramapping other `Contravariants`). `(b -> a)` is a function that _transforms b into the input of our original contravariant functor_. Given these, `contramap` ***produces a new contravariant functor that accepts an input `b` and internally transforms that `b` into an `a` and calls the original functor `f a` using that `a`***. Make sense now? If not, here's an illustration that may help.

![Contravariant Contramapping Illustration](https://i.imgur.com/uk5wOWU.png)

# Profunctors

And so we've arrived at the infamous `Profunctor`. The primary obstacle in understanding a `Profunctor` is understanding `Contravariant`, and if my explanation of `Contravariant` has clicked in your head, this should be easy.

A `Profunctor` is a machine with _both the input and output ends chainable with regular function machines_. It is a functor with no restrictions on either end, and can be both `fmapped` and `contramapped`.

`Profunctor` takes _two_ type arguments instead of the usual single argument found in `Functor` and `Contravariant`. The _left_ type argument represents the _input_ while the _right_ type argument represents the _output_. That said, the definition of `Profunctor` consists of:

1. `lmap`, which maps over the left type argument (input) of the functor, making it the equivalent of `contramap`.
2. `rmap`, which maps over the right type argument (output) of the functor, making it the equivalent of `fmap`.
3. `dimap`, which takes 2 functions, the first (left) one is applied to `lmap`, while the second (right) one is applied to `rmap`.

## Flipping Things Around: Orpvariant Functors

Some of you may encounter the ["orpvariant functor"](http://hackage.haskell.org/package/morphisms-functors-0.1.7/docs/Control-Functor-Polyvariant-Orpvariant.html){target="_blank"} on Hackage. Nobody actually uses this functor, practically speaking. "Orp" is simply the reverse of "Pro", and is not a formal Greek prefix like the ones found in the names of other functors. True to its name, it is simply a `Profunctor` with the type arguments switched around. `Orpvariant a b` is the same as `Profunctor b a`.

The library that defines `Orpvariant` only has one instance definition for Orpvariant: the reverse function arrow (`<--`), called `Opposite`. Naturally if a type represents a reverse function, where the first type argument represents the output and the second type argument represents the input, then instances of multiparameter typeclasses such as `Profunctor` need to have their type arguments reversed. As such, `Orpvariant` was invented for purposes specific to the library, but can be useful if you also need to define a `Profunctor` instance for something that has its "input" and "output" type arguments flipped, similar to `Opposite`. 

`Orpvariant` defines `orpmap` which is simply `dimap` with its function arguments flipped.

# The Formula for Functors

In this article, we have examined several common types of functors in order of complexity. I hereby present the formula for functor name construction:

`n-P-functor`

...Where `n` represents the the number of input/output pairs, _which can be omitted if n = 1_, and P represents the _type of functor_ (a type of functor must have some sort of rule that governs each pair in the context of our analogy), _which can be omitted if P is covariant_.

- For `Functor`, n = 1 (Mono), and P is covariant.
- For `Bifunctor`, n = 2 (Bi), and P is covariant.
- For `Contravariant`, n = 1 (Mono), and P is contravariant.
- For `Profunctor`, n = 1 (Mono), and P is provariant (yes, that's what pro- stands for).

It follows that Biprofunctors, [Tricontravariants](http://hackage.haskell.org/package/morphisms-functors-0.1.7/docs/Control-Functor-Polyvariant-Tricontravariant.html){target="_blank"}, [Bicontravariants](http://hackage.haskell.org/package/morphisms-functors-0.1.7/docs/Control-Functor-Polyvariant-Bicontravariant.html){target="_blank"}, Pentafunctors, etc., all exist; though in reality, we have no need for such complexity most of the time. 

# Conclusion

I hope that this article has revealed the rhyme and reason behind functors commonly encountered in the wild. I did not cover `Strong` nor `Costrong` as I believe they are out of scope (in the sense that they are types of _profunctors_ instead of types of _functors_ directly, if that makes sense). In the future I will cover _generalized functors_ such as `Star`, `Costar`, `Yoneda`, and `Coyoneda`, as well as abstractions built on top of `Profunctor`, such as `Strong` and `Costrong`, as well as their generalizations `Tambara` and `Pastro`.

If you have any comments or are still confused, leave a comment down below! If you believe that this is still confusing and that images would greatly aid in your understanding, leave a request down below and I'll consider adding some illustrations to this article.
