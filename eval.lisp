(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (safety 3) (debug 3) (space 0) (speed 0) (compilation-speed 0) (inhibit-warnings 0)))
  (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  (declaim (sb-ext:muffle-conditions sb-ext:code-deletion-note)))
