Unreleased
=====

* Replaced `mapAutoFilterValue` in `IsAutoFilter` typeclass with `Functor` superclass.
* Exposed `MkSomeFilter` constraint.
* Added support for building the project with `servant` version >= 0.19.
* Relax the parser of sorting keys to allow for non-alphanumeric identifier formats, like snake_case.

0.3
===

* Added support for `openapi3` by implementing necessary instances.

0.2
=====

* Added logging modifiers to API.
  In log config, printing function now accepts one more argument
  (feel free to ignore it by default).
  `HasLoggingServer` obtained `lcontext` type argument, constraints has changed in some instances.

0.1.0
=====

* Added logging.
* Added filtering.
* Added sorting.
* Added pagination.
* Added swagger tags.
* Added swagger error responses.
