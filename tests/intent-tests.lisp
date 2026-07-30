(in-package :inquisitio-tests)

(in-suite inquisitio-suite)

;;; The intent layer is only worth declaring if it can be queried, and it is
;;; queried by symbol identity. A feature declared in the wrong package is a
;;; different symbol from the one its members name, so every member becomes an
;;; orphan — while each file still compiles, loads, and tests green. Nothing
;;; except this check notices.

(test test-intent-graph-resolves
  "Every :FEATURE / :BELONGS-TO reference must name a defined feature.
src/features.lisp once lived in CL-USER while its members lived in :INQUISITIO
and :INQUISITIO.CACHE, which silently orphaned all 38 declarations."
  (finishes (telos:assert-intent-references)))

(test test-every-feature-has-members
  "A feature with no members is either dead or a package mismatch away from its
members. INQUISITIO is the root and holds only sub-features; INQUISITIO-FFI has
no DEFUN/I forms yet, so it is the one knowingly-empty leaf."
  (dolist (feature '(inquisitio.cache::inquisitio-cache
                     inquisitio::inquisitio-conditions
                     inquisitio::inquisitio-core
                     inquisitio::inquisitio-simplified
                     inquisitio::inquisitio-vec))
    (is (plusp (+ (length (telos:feature-members feature :functions))
                  (length (telos:feature-members feature :classes))
                  (length (telos:feature-members feature :conditions))
                  (length (telos:feature-members feature :methods))))
        "Feature ~S has no members — check that it is declared in the same ~
         package as the DEFUN/I forms that name it."
        feature))
  (is (plusp (length (telos:feature-members 'inquisitio::inquisitio :features)))
      "The root feature INQUISITIO has no sub-features."))
