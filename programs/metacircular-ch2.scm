(use-modules (grand scheme))

(define (value expression environment)
  (match expression
    (('lambda args body)
     `(procedure-tag ,args ,body ,environment))

    (('quote literal)
     literal)

    (('if condition consequent alternative)
     (if (value condition environment)
         (value consequent environment)
     ;else
         (value alternative environment)))

    ((operator . operands)
     (let ((procedure (value operator environment))
           (arguments (map (lambda (operand)
                              (value operand environment))
			   operands)))
         (application procedure arguments)))

    (_
     (cond ((symbol? expression)
            (lookup expression environment))
           ((number? expression)
            expression)
           (else
            (error 'unrecognized-expression expression))))))

(define (application procedure arguments)
  (match procedure
    (('procedure-tag parameters body closure)
     (let ((environment `(,(tie parameters arguments) . ,closure)))
       (value body environment)))
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
     ;; ...
     )))


;;;; so, let's try it!

[e.g. (value ''() initial-environment) ===> ()]

[e.g. (value ''ponzi initial-environment) ===> ponzi]

[e.g. (value '(cons 'hi '(there)) initial-environment) ===> (hi there)]

[e.g. (value '((lambda (x) (cons x x)) 'o) initial-environment) ===> (o . o)]

(define #;a-horrible-version-of Y #;for-2-argument-functions-only
  '(lambda (f) ((lambda (x) (x x)) (lambda (g) (f (lambda (a1 a2) ((g g) a1 a2)))))))

[e.g. (value `([,Y (lambda (apd)
                     (lambda (xs ys)
                       (if (eq? xs '()) ys (cons (car xs) (apd (cdr xs) ys)))))]
               '(q w e) '(a s d))
             initial-environment)
      ===> (q w e a s d)]

;;; -- works like a charm!
