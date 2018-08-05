# v1.1 (not yet released)

Added support for finding faces.

Added support for finding mode hooks and functions defined with
`defstruct`.

Fixed an issue with symbols in comments being assumed to be variables
(they may be functions or variables).

Fixed an issue jumping to definitions in syntactically invalid
code. Previously we assumed that we could always macroexpand code,
which isn't true on unfinished code.

Fixed a crash when starting from a symbol that was at the very start
or end of a buffer.

# v1.0

Initial release.
