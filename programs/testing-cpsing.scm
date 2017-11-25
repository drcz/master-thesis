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
[e.g. (passing-program program-with-closure)
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
 (begin
   (original-name)
   (passing-program `((define pass* (lambda (x y K) (K (* x y))))
                      . ,(passing-program
                          '((define sq (lambda (x) (* x x))) (sq 3))))))
 ===>
 ((define pass-pass* (lambda (x y K return)
                       (pass* x y
                              (lambda (x*y/1) (pass-K x*y/1 return)))))
  (define pass-pass-sq (lambda (x return return)
                         (pass-pass* x x return return)))
 (pass-pass-sq 3 exit exit))]
;; nie no...
[e.g.
 (begin
   (original-name)
   (passing-program '((define K* (lambda (x y k) (k (* x y))))
                      (define sqK (lambda (x return) (K* x x return)))
                      (sqK 3 exit)))) 

 ===> ((define pass-K*
         (lambda (x y k return)
           (pass* x y
                  (lambda (x*y/1) (pass-k x*y/1 return)))))
       (define pass-sqK
         (lambda (x return return)
           (pass-K* x x return return)))
       (pass-sqK 3 exit exit))]

;;; pass-k...?

(original-name)
[e.g.
 (passing-program '((define f (lambda (a b c) (+ (* a b) c))) (f 1 2 3))) 
 ===>
 ((define pass-f (lambda (a b c return)
                   (pass* a b
                          (lambda (a*b/1) (pass+ a*b/1 c return)))))
  (pass-f 1 2 3 exit))] ;;; piknie.

