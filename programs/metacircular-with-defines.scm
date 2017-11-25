;;; extended with topenv -- "free of charge"
(use-modules (grand scheme))

(define (value expression environment top-environment)
  (match expression
    (('lambda args body)
     `(procedure-tag ,args ,body ,environment))

    (('quote literal)
     literal)

    (('if condition consequent alternative)
     (if (value condition environment top-environment)
         (value consequent environment top-environment)
     ;else
         (value alternative environment top-environment)))

    ((operator . operands)
     (let ((procedure (value operator environment top-environment))
           (arguments (map (lambda (operand)
                              (value operand environment top-environment))
			   operands)))
         (application procedure arguments top-environment)))

    (_
     (cond ((symbol? expression)
            (lookup expression `(,@environment ,top-environment)))
           ((number? expression)
            expression)
           (else
            (error 'unrecognized-expression expression))))))


(define (application procedure arguments top-environment)
  (match procedure
    (('procedure-tag parameters body closure)
     (let ((environment `(,(tie parameters arguments) . ,closure)))
       (value body environment top-environment)))
    (_ ; a primitive procedure
     (apply procedure arguments))))


(define (lookup-frame name frame)
  (match frame
    (((key value) . rest)
     (if (equal? key name)
       `(,key ,value)
     ;else
       (lookup-frame name rest)))
    (()
     #false)))

(define (lookup name environment)
  (let (((frame . frames) environment))
    (match (lookup-frame name frame)
      ((key value)
       value)
      (_
       (lookup name frames)))))

(define (tie names values)
  (match names
    (()
     '())
    ((name . other-names)
     (let (((value . other-values) values))
       `(,@(tie name value) . ,(tie other-names other-values))))
    (rest
     `((,rest ,values)))))


(define initial-environment
  `(((cons ,cons)
     (car ,car)
     (cdr ,cdr)
     (eq? ,eq?)
     (pair? ,pair?)
     (number? ,number?)
     (+ ,+)
     (* ,*)
     (- ,-)
     (= ,=)
     (< ,<)
     ;; ...
     )))


(define (result program)
  (let* ((((’define names functions)
           ...
           expression) program)
         (topenv (tie names
                      (map (lambda (function)
                             (value function
                                    initial-environment
                                    '()))
                           functions))))
    (value expression initial-environment topenv)))

[e.g.
 (result
  '((define square (lambda (x) (* x x)))
    (define map (lambda (f xs) (if (eq? xs '())
                              '()
                              (cons (f (car xs))
                                    (map f (cdr xs))))))
    (map square '(1 2 3 4 5))))
 ===> (1 4 9 16 25)]

[e.g.
 (result
  '((define foldr (lambda (op e xs) (if (eq? xs '())
                                   e
                               ;else
                                   (op (car xs) (foldr op e (cdr xs))))))
    (define foldl (lambda (op acc xs) (if (eq? xs '())
                                     acc
                                 ;else
                                     (foldl op (op acc (car xs)) (cdr xs)))))
    (define concat (lambda (xs ys) (foldr cons ys xs)))
    (define snoc (lambda (x y) (cons y x)))
    (define reverse (lambda (xs) (foldl snoc '() xs)))
    (define dupsko (lambda (xs) (concat xs (reverse xs))))
    (dupsko '(k o b y ł a m a))))
 ===> (k o b y ł a m a a m a ł y b o k)]
