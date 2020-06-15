;; example of dynamic VS lexical scope.

;; The value of a dynamic variable is looked up in the environment that calls
;; the function, while the value of a lexical variable is looked up in the
;; environment where the function was defined.

;; by standard, let define a lexical scope
(let ((y 7))
  (defun scope-test1 (x)
    (list x y)))

;; declare special to get dynamic scope
(let ((y 7))
  ;;(declare (special y))
  (defun scope-test2 (x)
    (list x y)))

;; because y is lexical scope, it does not matter what y is set to before the
;; function is called
(let ((y 5)) (scope-test1 3))
(let ((y 5)) (scope-test2 3))
