(defun hello (name &optional age gender)
  "say hello to `name'."
  (format t "hello ~a! ~&" name))
(hello "paw")

(defun hello (name &key (happy t))
  "if happy is 't, then print a smiley"
  (format t "hello ~a " name)
  (if happy
      (format t ":)"))
  (format t "~&"))
(hello "me" :happy nil)
(hello "me" :happy t)

(defun mean (x &rest numbers)
  (/ (apply #'+ x numbers)
     (+ 1 (length numbers))))

(mean 1)
(mean 1 2 3 4 5)

(defun foo1 (a b c)
  "returns a: value returned by last executed form of the body"
  a)

(defvar *res* (foo1 :a :b :c))

(defun foo2 (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  "add another variable to the default-values expression. This will be 't if the
called supplied an argument for this parameter and nil otherwise"
  (list a b c b-supplied-p))

(foo2 :a 1)

(defun foo3 (n)
  "use return-from to return as soon as two integers whose product is larger
than n, is found"
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo3 (list i j))))))

(foo3 10)
