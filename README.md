# PureScript Result

[![Latest release](http://img.shields.io/github/release/ad-si/purescript-result.svg)
](https://github.com/ad-si/purescript-result/releases)
[![Build status](https://travis-ci.org/ad-si/purescript-result.svg?branch=master)
](https://travis-ci.org/ad-si/purescript-result)

The `Result` type is used to represent computations
that may succeed or fail.

This should be used instead of the more genereic `Either`.

Many other functional programming languages also support Result types:

Language | Type Name            | Success | Error | Documentation
---------|----------------------|---------|-------|--------------
Elm      | `Result error value` | Ok      | Err | [package.elm-lang.org][elm]
Rust     | `Result<T, E>`       | Ok      | Err   | [doc.rust-lang.org/book][rust]
OCaml    | `('a, 'b) result`    | Ok      | Error | [ocaml.org/learn][ocaml]
F#       | `Result<'T,'TError>` | Ok      | Error | [docs.microsoft.com][fsharp]

[rust]: https://doc.rust-lang.org/book/second-edition/ch09-02-recoverable-errors-with-result.html
[elm]: http://package.elm-lang.org/packages/elm-lang/core/latest/Result
[ocaml]: https://ocaml.org/learn/tutorials/error_handling.html#Result-type
[fsharp]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/results


I'm not a fan of useless abbreviations
(as if 3 instead of 5 characters would make a difference) but
a fan of readability and pronounceability of code
(e.g. dictating your coworker a snippet of code)
and therefore this package uses:

```purescript
data Result error value = Error error | Ok value
```


Because how typeclasses work in PureScript it must be `Result error value`
and not `Result value error`, so that the value can be `map`ed.
([details])

[details]: https://github.com/purescript/purescript/issues/3202#issuecomment-357465332


## Installation

```
bower install --save purescript-result
```


## Documentation

Module documentation is [published on Pursuit].

[published on Pursuit]: http://pursuit.purescript.org/packages/purescript-result


## Development

Deployment of new version:

```sh
npx pulp test
npx pulp version
npx pulp publish
```
