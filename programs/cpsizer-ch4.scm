;;; it is taken from Appendix E actually.
(use-modules (grand scheme) (grand symbol) (srfi srfi-88))

;;; ...was lacking any definition of `assert` so here's
;;; one -- you may want to add (exit) in it. I didn't.
(define-macro (assert p) 
  `(if (not ,p) (pretty-print '(assertion ,p failed!))))


(define primitive-operators
  '((+ pass+)
    (- pass-)
    (* pass*)
    (/ pass/)
    (% pass%)
    (&& pass&&)
    (|| pass||)
    (^ pass^)
    (<< pass<<)
    (>> pass>>)
    (= pass=)
    (< pass<)
    (<= pass<=)
    (<> pass<>)
    (>= pass>=)
    (> pass>)))

(define (comparison? operator)
  (any (lambda ((left right))
         (or (equal? operator left)
             (equal? operator right)))
       mutually-negating-comparisons))

(define (compound? expression)
  (pair? expression))

(define mutually-negating-comparisons
  '((< >=)
    (<= >)
    (= <>)))

(define (primitive-operator? operator)
  (any (lambda ((primop passing-function))
         (equal? operator primop))
       primitive-operators))

(define (inversion comparison)
  (any (lambda ((left right))
         (or (and (equal? left comparison)
                  right)
             (and (equal? right comparison)
                  left)))
       mutually-negating-comparisons))

(define (passing-function function-name)
  (or (any (lambda ((operator passing-name))
             (and (equal? operator function-name)
                  passing-name))
           primitive-operators)
      (and (comparison? function-name)
           (symbol-append 'pass function-name))
      (symbol-append 'pass- function-name)))

(define (expression-name expression)
  (match expression
    [(head . tail)
     (if (or (primitive-operator? head)
             (comparison? head))
         (apply symbol-append
                (intersperse head (map expression-name tail)))
         (apply symbol-append (expression-name head) '/
                (intersperse ': (map expression-name tail))))]
    [_
     (pass expression ->string string->symbol)]))

(define original-name
  (let ((number 0))
    (lambda base
      (match base
        [()
         (set! number 0)]
        [(base)
         (set! number (+ number 1))
         (string->symbol (string-append
                          (->string (expression-name base))
                          "/"
                          (->string number)))]))))


(define (passing-arguments arguments names final)
  (assert (= (length arguments) (length names)))
  (match arguments
    [()
     (assert (null? names))
     final]
    [(argument . next)
     (let (((name . names) names))
       (if (compound? argument)
           (passing-arguments next names
                              (passing argument
                                       `(lambda (,name) ,final)))
                                        ;else
           (passing-arguments next names final)))]))

(define (passing expression continuation)
  (match expression
    [('quote _)
     `(,continuation ,expression)]

    [('if <condition> <then> <else>)
     (let ((result (original-name <condition>)))
       (passing <condition> `(lambda (,result)
                               (if ,result
                                   ,(passing <then> continuation)
                                   ,(passing <else> continuation)))))]

    [('lambda <args> <body>)
     `(,continuation (lambda (,@<args> return)
                       ,(passing <body> 'return)))]

    [(function . arguments)
     (let ((simple-arguments (map (lambda (argument)
                                    (if (compound? argument)
                                        (original-name argument)
                                        argument))
                                  arguments)))	   
       (passing-arguments arguments simple-arguments
                          `(,(passing-function function)
                            ,@simple-arguments
                            ,continuation)))]

    [_
     `(,continuation ,expression)]))


(define (passing-program program)
  (let (((('define names functions) ... expression) program))
    `(,@(map (lambda (name function)
               (let ((('return pass-function) (passing function 'return)))
                 `(define ,(passing-function name) ,pass-function)))
             names functions)
      ,(passing expression 'exit))))


(define (pass= a b return)
  (return (= a b)))

(define (pass- a b return)
  (return (- a b)))

(define (pass* a b return)
  (return (* a b)))

;;;; allright!
[e.g.
 (begin
   (original-name)
   (passing-program '((define pow
                        (lambda (m n)
                          (if (= n 0)
                              1
                              (* m (pow m (- n 1))))))
                      (define !
                        (lambda (n)
                          (if (= n 0)
                              1
                              (* n (! (- n 1))))))
                      (! (pow 2 3)))))
 ===>
 ((define pass-pow
    (lambda (m n return)
      (pass= n
             0
             (lambda (n=0/1)
               (if n=0/1
                   (return 1)
                   (pass- n
                          1
                          (lambda (n-1/3)
                            (pass-pow m
                                      n-1/3
                                      (lambda (pow/m:n-1/2)
                                        (pass* m pow/m:n-1/2 return))))))))))
  (define pass-!
    (lambda (n return)
      (pass= n
             0
             (lambda (n=0/4)
               (if n=0/4
                   (return 1)
                   (pass- n
                          1
                          (lambda (n-1/6)
                            (pass-! n-1/6
                                    (lambda (!/n-1/5) (pass* n !/n-1/5 return))))))))))

  (pass-pow 2 3 (lambda (pow/2:3/7) (pass-! pow/2:3/7 exit))))]

;;; beautiful.
