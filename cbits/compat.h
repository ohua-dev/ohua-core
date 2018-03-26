/*
 * This header file defines readable aliases for version checks.  the
 * checks themselves are done with the standard cabal generated
 * macros. The idea is that one can define *what* a certain condition
 * guards against from its name, rather than having to guess.
 */

/*
 * The `Semigroup` typeclass was added to base in base version
 * 4.9.0.0.  We need this because in the future `Semigroup` will
 * become a super class of `Monoid`, meaning an instance for it will
 * be required.
 */
#define BASE_HAS_SEMIGROUP MIN_VERSION_base(4,9,0)

/* 
 * This check is necessary as before GHC 8.2 there was no `COMPLETE`
 * pragma and additionally the version that GHC 8.2 shipped with broke
 * some checks in certain patterns. That is why we only enable the
 * `COMPLETE` pragma for any GHC after 8.4, when the bug was
 * supposedly fixed.
 */
#define COMPLETE_PRAGMA_WORKS __GLASGOW_HASKELL__ >= 804

/*
 * The `Data.Aeson` module does not export `Options`, the fields for
 * that record and the helper functions like `camelTo2`.
 *
 * After this version we don't need to import `Data.Aeson.Types`
 * anymore which satisfies the `-funused-imports` warning.
 */
#define AESON_EXPORTS_OPTIONS !MIN_VERSION_aeson(1,2,2)
