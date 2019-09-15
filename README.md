# uname

Turn a code point (as a hexadecimal string) into its name.

- If code point possesses a correction alias, that will be used instead
- If code point has no name, control/figment alias will be used instead
- Implements NR1, NR2, NR3, and NR4 from the standard
- Certain "no-names" will be returned in the following cases:
  - Private use
  - Surrogate
  - Noncharacter
  - Reserved

Version is matched with the version of Unicode that is implemented
