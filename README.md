# Testing ELM programs

## Resources

[Beginning Elm](http://elmprogramming.com)

[function-composition](http://elmprogramming.com/function-composition.html)

[easy to test](http://elmprogramming.com/easy-to-test.html)

[fuzz test](http://elmprogramming.com/fuzz-testing.html)

[package elm-test](http://package.elm-lang.org/packages/elm-community/elm-test/4.2.0)
[github elm-test](https://github.com/elm-community/elm-test/tree/4.2.0)

## Quote

“I get paid for code that works, not for tests, so my philosophy is to test as little as possible to reach a given level of confidence.” - Kent Beck

## What lies here

The rippleCarryAAdder is a nice candidate for fuzz testing. The elmprogramming.com ebook describes an older version of elm-test. This repo uses the current (as of 4.2.0) version.

See the elm-test [repo](https://github.com/elm-community/elm-test/tree/4.2.0) to learn about fuzzers and how to write property tests in elm.

See [QuickCheck](https://en.wikipedia.org/wiki/QuickCheck) for more about property tests in general.
