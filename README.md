# ki-effectful

## Description

A `StructuredConcurrency` effect for the [`effectful`][effectful] ecosystem.

## How to use

This library exposes the following elements:

* `StructuredConcurrency` — The type-level effect that you can declare in your type signatures.

example:
```haskell
myAction :: (StructuredConcurrency :> es) => Eff es ()
```

* The [`ki`][ki] api lifted to Eff.

example:
```haskell
scoped :: StructuredConcurrency :> es => (Scope -> Eff es a) -> Eff es a
fork ::   StructuredConcurrency :> es => Scope -> Eff es a -> Eff es (Thread a)
```

[effectful]: https://github.com/haskell-effectful/effectful
[ki]: https://github.com/awkward-squad/ki
