/**
 * This header file defines readable aliases for version checks.  the
 * checks themselves are done with the standard cabal generated
 * macros. The idea is that one can define *what* a certain condition
 * guards against from its name, rather than having to guess.
 */

/**
 * The `Semigroup` typeclass was added to base in base version
 * 4.9.0.0.  We need this because in the future `Semigroup` will
 * become a super class of `Monoid`, meaning an instance for it will
 * be required.
 */
#define BASE_HAS_SEMIGROUP MIN_VERSION_base(4,9,0)

#if BASE_HAS_SEMIGROUP
#define SEMIGROUP_COMPAT_IMPORT import qualified Data.Semigroup as SG
#else
#define SEMIGROUP_COMPAT_IMPORT
#endif

/**
 * This check is necessary as before GHC 8.2 there was no `COMPLETE`
 * pragma and additionally the version that GHC 8.2 shipped with broke
 * some checks in certain patterns. That is why we only enable the
 * `COMPLETE` pragma for any GHC after 8.4, when the bug was
 * supposedly fixed.
 */
#define COMPLETE_PRAGMA_WORKS __GLASGOW_HASKELL__ >= 804

/**
 * The `Data.Aeson` module does not export `Options`, the fields for
 * that record and the helper functions like `camelTo2`.
 *
 * After this version we don't need to import `Data.Aeson.Types`
 * anymore which satisfies the `-funused-imports` warning.
 */
#define AESON_EXPORTS_OPTIONS !MIN_VERSION_aeson(1,2,2)

/**
 * With version 5.0 recursion schemes renamed the `Foldable` class to
 * `Recursive` and the `Unfoldable` to `Corecursive` to avoid name
 * clashes with the base library. This macro in addition to the
 * following two macros facilitates the use of these two different
 * class names.
 */
#define RECURSION_SCHEMES_NEW_CLASS_NAMES MIN_VERSION_recursion_schemes(5,0,0)

#if RECURSION_SCHEMES_NEW_CLASS_NAMES
#define RECURSION_SCHEMES_RECURSIVE_CLASS Recursive
#define RECURSION_SCHEMES_CORECURSIVE_CLASS Corecursive
#else
#define RECURSION_SCHEMES_RECURSIVE_CLASS Foldable
#define RECURSION_SCHEMES_CORECURSIVE_CLASS Unfoldable
#endif

/**
 * From version 4.9.0.0 base got a new API for referring to call
 * stacks. The following macros provide a backwards compatible way to
 * get call stacks.
 */
#define NEW_CALLSTACK_API MIN_VERSION_base(4,9,0)

/**
 * The <&> operaator, which I use a lot was added to microlens in
 * version 4.5.0.0.
 */
#define MICROLENS_HAS_FLIP_FMAP MIN_VERSION_microlens(0,4,5)

/**
 * Bundling pattern synonyms with datatypes only became available in GHC 8. In
 * earlier versions the patterns had to be exported separately.
 */
#define GHC_HAS_BUNDLED_PATTERN_SYNONYMS __GLASGOW_HASKELL__ >= 800
