;;; ex1.el --- description -*- lexical-binding: t; -*-


(let ((x 0))
  (setq x 42)
  x)

(defvar x 42)
(message "%s" x)


(defun ftest1 (x y)
  (+ (* 2 x) (* 3 y)))

(ftest1 (+ 3 (+ 1 2)) 10)
