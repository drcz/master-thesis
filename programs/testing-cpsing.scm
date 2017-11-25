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
