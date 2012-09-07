Promised but not yet implemented:

  some, every, notany, notevery
  search, mismatch

--------

Use short vectors in Allegro 7 and up (ugh)

--------

The usefulness of the string-compaction hack in the seq tree implementation
varies, now that Unicode (particularly UTF-32) is catching on.

  () In 32-bit CLISP it's useless; `char-code-limit' is 1114112, and
     `base-char' = `character'.

  () In 32-bit SBCL it's useless to use wide strings; `base-char' is 8 bits,
     but `char-code-limit' is 1114112.

  () On the other hand, in 64-bit OpenMCL, it's worth using strings even
     though `char-code-limit' is 1114112, and `base-char' = `character',
     since this still cuts the size per character from 64 bits to 32.

So the way I have this conditionalized now is inadequate.  First, we need to
not compile code for two kinds of strings when there's really only one.  [Oh
good, I already had this right.]  Second, we need to not use extended
strings when they're not any smaller than 1 word/character.  [Done.]  (We
can worry later about whether it's worth using them even when they are
somewhat smaller.)

--------

