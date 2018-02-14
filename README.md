# The foundation of the ohua parallelizing compiler

The `ohua-core` library is the heart of the ohua suite of parallelizing compilers.

For more information about the ohua compiler suite, goals, function etc visit the [official documentation](https://ohua.readthedocs.org)

The core library comprises the fundamental and platform independent parts used in each instance of the ohua compiler
such as the [EDSL for the JVM](https://github.com/ohua-dev/ohua-jvm-integration) and the [standalone ohua compiler](https://github.com/ohua-dev/ohuac).

In its essence the ohua core transforms an expression based language called ALang, which is short for "algorithm language" 
into a dataflow graph which can be executed by any runtime that implements the ohua exeution semantics on any platform
capable of implementing the ohua core operators.
Tha tasks performed by the compiler are

- verifying the input expressions are executable by the runtime
- lowering into a dataflow
- performing generic optimisations

Furthermore the compiler defines a set of hooks for adding custom manipulations to the compilation pipeline.
