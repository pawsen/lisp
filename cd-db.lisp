;; http://gigamonkeys.com/book/practical-a-simple-database.html

;; evaluate and insert lisp expression
;; C-u C-x C-e


;; plist is a list with keywords
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; defvar creates a global variable
;; (*) is a lisp naming convention to denote global variable.
;; let creates a local variable which is lexical, unless declared special in
;; which case it is a dynamic variable.

;; setq sets variable to new value
(defvar *db* nil)
(defun add-record (cd) (push cd *db*))

;; t is shorthand for *standard-output*
;; ~{ ~} process all elements of the list
;; ~a is aesthetic directive, ie. output in human-readable form
;; ~% newline
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; using ~{ ~}, dump-db can be constructed as a oneliner
(defun dump-db-oneline ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

;; force-output is used to ensure Lisp doesn't wait for newline before it print
;; the prompt
(defun prompt-read (promt)
  (format *query-io* "~a: " promt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   ;; parse-integer returns nil if not able to parse. or returns the first
   ;; non-nil value.
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]:"))
            (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; manually search *db* for artist
;; remove-if-not returns a new list
(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist ) artist))
   *db*))

;; instead, create generic selector.
;; we want remove-if-not to use the anonymous function passed as an argument to
;; select in the variable selector-fn. Not call selector-fn as a function.
;; Thus we should not use #' here.
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; specific selector.
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(select (artist-selector "Dixie Chicks"))

;; instead we make a generic selector that returns an anonymous function
;; If the parameter is non-nil, see if the :field is equal to the parameter.
;; If no parameter is supplied, return t.
;; Thus the anonymous function returns t only if all the fields supplied is
;; matched. If t is returned, remove-if-not in select will keep the record.
;; example:
;; #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks"))
;;
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)    title) t)
       (if artist   (equal (getf cd :artist)   artist) t)
       (if rating   (equal (getf cd :rating)   rating) t)
       (if ripped-p (equal (getf cd :ripped)   ripped) t)
       )))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
         (when (funcall selector-fn row)
           (if title    (setf (getf row :title)  title))
           (if artist   (setf (getf row :artist) artist))
           (if rating   (setf (getf row :rating) rating))
           (if ripped-p (setf (getf row :ripped) ripped)))
         row) *db* )))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;; write a generic where function - ie remove code duplication
;; this is a macro. ' prevents lisp from evaluating the variable
(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))
;; (make-comparison-expr :rating 10) ; => (EQUAL (GETF CD :RATING) 10)

;; in a back-quoted expression, any subexpression that's preceded by a comma is
;; evaluated. Notice the effect of the comma in the second expression:
`(1 2 (+ 1 2)) ; => (1 2 (+ 1 2))
`(1 2 ,(+ 1 2)) ; => (1 2 3)
'(1 2 (+ 1 2)) ; => (1 2 (+ 1 2))

;; Using a back quote, you can write make-comparison-expr like this:
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))


(update (where :artist "Dixie Chicks") :rating 11)
(select (where :artist "Dixie Chicks"))

(select (where :rating 7 :ripped t))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

(save-db "~/Documents/my-cds.db")
