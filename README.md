# The foundation of the ohua parallelizing compiler

[![Build Status](https://travis-ci.org/ohua-dev/ohua-core.svg?branch=master)](https://travis-ci.org/ohua-dev/ohua-core)
[![Documentation Status](https://readthedocs.org/projects/ohua/badge/?version=latest)](https://ohua.readthedocs.io/en/latest/?badge=latest)

The `ohua-core` library is the heart of the ohua suite of parallelizing compilers.

For more information about the ohua compiler suite, goals, function etc visit
the [official documentation](https://ohua.readthedocs.org)

The core library comprises the fundamental and platform independent parts used
in each instance of the ohua compiler such as the [EDSL for the
JVM](https://github.com/ohua-dev/ohua-jvm-integration) and the [standalone ohua
compiler](https://github.com/ohua-dev/ohuac).

In its essence the ohua core transforms an expression based language called
ALang, which is short for "algorithm language" into a dataflow graph which can
be executed by any runtime that implements the ohua exeution semantics on any
platform capable of implementing the ohua core operators. Tha tasks performed by
the compiler are

- verifying the input expressions are executable by the runtime
- lowering into a dataflow
- performing generic optimisations

Furthermore the compiler defines a set of hooks for adding custom manipulations
to the compilation pipeline.

## Code formatting

The default formatting for code is done using the `hindent` library, the
configuration file `.hindent.yaml` can be found at the project root.

## Notes on Universum

I use a `Prelude` replacement called `Universum` in this project. This has a few
minor consequences for how code is written. Most importantly I activate the
`NoImplicitPrelude` extension by default, thus in every new module written you
should first do an `import Universum` (or `import Prelude` if you *really* like
`Prelude`).

The following are some notes on using `Universum`, assembled for your convenience:

- `error` from `Universum` uses `Text` rather than `String`

  This should not be an issue, most value we manipulate in this library are
  `Text` anyways.

- Trace functions (`trace`, `traceShowId` etc) are in scope by default, but
  raise a warning.

  Aka you can use them, but you have to remove them if you want the code to pass
  the CI.

- Universum uses `Text` rather than `String` by default.

  Aka use `<>` (also in scope by default) rather than `++` to concatenate
  strings.

- some IO is polymorphic

  `putStrLn "some string"` will raise an ambiguity error, you'll have to
  annotate the string like so `putStrLn ("some string" :: Text)`

- `mtl` stuff is in scope by default.

  The `Reader`, `ExceptT` and `State` monad (+ transformers, + classes) are in
  scope by default. No need to import `Control.Monad.Reader` etc


For more information on protolude check out [the GitHUb
repository](https://github.com/serokell/universum).
