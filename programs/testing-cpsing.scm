(use-modules (grand scheme) (grand symbol) (srfi srfi-88))
(add-to-load-path (getcwd))
(include-from-path "metacircular-with-defines.scm")
(include-from-path "cpsizer-ch4.scm")

(define primitive-operators
  '((+ pass+)
    (- pass-)
    (* pass*)
    (/ pass/)
    (% pass%)
    (= pass=)
    (< pass<)))

(define initial-environment
  `(((+ ,+)
     (* ,*)
     (- ,-)
     (/ ,/)
     (% ,modulo)
     (= ,=)
     (< ,<)
     ;; ...
     (exit ,(lambda (x) x)) ; :)
     )))

(define prelude
  '((define pass= (lambda (a b return) (return (= a b))))
    (define pass< (lambda (a b return) (return (< a b))))
    (define pass+ (lambda (a b return) (return (+ a b))))
    (define pass- (lambda (a b return) (return (- a b))))
    (define pass* (lambda (a b return) (return (* a b))))
    (define pass/ (lambda (a b return) (return (/ a b))))
    (define pass% (lambda (a b return) (return (% a b))))))



(define (testing-cps program)
  (equal? (result program)
          (result (append prelude
                          (passing-program program)))))


[e.g. (testing-cps '((define pow (lambda (m n)
                                   (if (= n 0)
                                       1
                                       (* m (pow m (- n 1))))))
                     (define ! (lambda (n)
                                 (if (= n 0)
                                     1
                                     (* n (! (- n 1))))))
                     (! (pow 2 3))))]

;;; (...)
(define program-with-closure
  '((define mk-adder (lambda (x) (lambda (y) (+ x y))))
    (define app (lambda (f x) (f x))) ;; without it cpsizer breaks
    (app (mk-adder 2) 3)))

[e.g. (result program-with-closure) ===> 5]

;;; not good TODO
[e.g. (passing-program evil-closures)
      ===>
      ((define pass-mk-adder (lambda (x return)
                               (return (lambda (y return)
                                         (pass+ x y return)))))
       (define pass-app
         (lambda (f x return) (pass-f x return)))
       (pass-mk-adder 2
                      (lambda (mk-adder/2/18)
                        (pass-app mk-adder/2/18 3 exit))))]

; (...)

[e.g.
 (passing-program (passing-program '((define sq (lambda (x) (* x x))) (sq 3))))
 ===>
 ((define pass-pass-sq
    (lambda (x return return)
      (pass-pass* x x return return)))
  (pass-pass-sq 3 exit exit))]
