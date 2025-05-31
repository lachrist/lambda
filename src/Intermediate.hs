module Intermediate where

-- PushConst 42
-- MakeClosure L0        ;; create closure for `(lambda (x) x)`
-- Call                  ;; call with argument 42
-- Return

-- Label L0 (x):             ;; function body starts here
--   Lookup x
--   Return