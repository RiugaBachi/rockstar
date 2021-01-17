---
title: "An Overview of Arrowized FRP"
date: 04 July 2019
description: "An explanation of AFRP, from motivations, to principles, and lastly, implementation."
tags:
- haskell
- afrp
---

I would like to start off by saying, happy 4th of July. May we never forget the true spirit of the United States of America despite any resentments we may have toward the status quo.

FRP is a confusing topic for many beginning Haskellers. Tutorials out there generally start at too high of a level, discouraging many from truly comprehending how FRP conceptually works.

In this article, I will not make the case for FRP, as a previous article of mine has already covered that. Instead, I will focus solely on explaning arrowized FRP from the bottom up.

Before we begin, I would like to make it clear that _all FRP libraries are implemented slightly differently_. We have many adjectives that we like to use when referring to FRP libraries, which will be too much to go over in a brief summary. This article will not be for any one library in particular, and will explain only the common, fundamental concepts of arrowized FRP which you can find in virtually all implementations.

# Arrowized vs Monadic

FRP implementations can be split into two predominant categories: monadic and arrowized. Monadic implementations typically use a custom monad type that implements `MonadIO`. This design choice allows the implementor to use IORefs and MVars behind the scene in order to improve performance benchmarks. The downside is that the user can only use the FRP implementation in an impure context, which is off-putting to many. Typically, non-Haskell FRP implementations (such as Rust's) are based off of the monadic model of FRP, as those languages are impure anyway. The IORefs and MVars are used to essentially perform IO streaming behind the scenes one way or another, thereby running on _push-based semantics_. I will explain what this term means in the next section.

The arrowized model of FRP - the one this article will cover - is encoded via Arrows. An Arrow can be thought of as something a bit more abstract than a Monad. A Monad of the form `(m a)` only encodes the output type via `a`. An arrow must have two type variables of the form `a b c` where `b` is the input and `c` is the output. In short, _an Arrow remembers both the input and output type while a Monad only remembers the output type_.

Typically, we like to think of Arrows as an abstraction over functions, for they must all define `arr :: (b -> c) -> a b c` which lifts a regular function into an arrow.

Arrows are used for FRP _since they can describe an automaton_.

# Automatons

The following is the signature for a basic automaton:

`newtype Automaton i o = Automaton { unwrapAutomaton :: i -> (o, Automaton i o) }`

Since an arrow can be thought of as a wrapped function, this type signature tells us that `Automaton` is a function that takes `i` and produces both `o` _and_ a new automaton that can take another `i` and produce another `o`.

***Depending on how one defines arrow composition (>>>) for their automaton, the returned `Automaton i o` does not necessarily produce the same `o` for a given `i` as the original automaton***.

Ultimately, ***automatons allow us to encode hidden, local state***.

This ability to encode state allows one to implement FRP in a pure, non-IO-streaming manner. While performance can be worse (and you are usually the mercy of GHC's optimisation behavior), it is a small price to pay for being able to use FRP in pure contexts.

# Basic Concepts of Arrowized FRP

Every arrowized FRP implementation typically has all or most of following core concepts:

- Some sort of automaton arrow.
- Events
- Behaviors
- Dynamics

## The Custom Automaton Type

The automaton type used by an FRP implementation is its most critical aspects, for every other concept cannot be defined without it.

These automatons take on various names depending on the implementation. The following are some popular ones:

- `Wire m a b` from `wires`
- `Wire s e m a b` from `netwire`
- `VarT m a b` from `varying`

What we notice is that most of these contains 3 types variables: the underlying monad `m` (for FRP would be fairly boring without being able to use monads), the input `a`, and the output `b`. Beyond that, other type variables are typically unnecessary and are strictly for any nuances the implementor has in mind for his or her library.

Generally, these automatons will be defined in the form `newtype Auto m a b = Auto { runAuto :: a -> (m b, Auto m a b) }`, which is trivial if understood the previous definition of non-monadic automatons.

## Events

Automatons by themselves are boring.

