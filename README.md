# ki-effectful

[![Build Status](https://github.com/TristanCacqueray/ki-effectful/workflows/Haskell-CI/badge.svg?branch=main)](https://github.com/TristanCacqueray/ki-effectful/actions?query=branch%3Amain)
[![Hackage](https://img.shields.io/hackage/v/ki-effectful.svg?logo=haskell)](https://hackage.haskell.org/package/ki-effectful)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)

## Description

A `StructuredConcurrency` effect for the [`effectful`][effectful] ecosystem.

## How to use

This library exposes the following elements:

* `StructuredConcurrency` — The type-level effect that you can declare in your type signatures.

example:
```haskell
runStructuredConcurrency :: IOE :> es => Eff (StructuredConcurrency : es) a -> Eff es a
```

* The [`ki`][ki] api lifted to Eff using the effect local rep to store the scope:

example:
```haskell
scoped :: StructuredConcurrency :> es => (Scope -> Eff es a) -> Eff es a
fork :: StructuredConcurrency :> es =>   Scope -> Eff es a -> Eff es (Thread a)
```

[effectful]: https://github.com/haskell-effectful/effectful
[ki]: https://github.com/awkward-squad/ki
