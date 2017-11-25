;;; appendix E continued...
(use-modules (grand scheme) (grand symbol) (srfi srfi-88))
(add-to-load-path (getcwd))
(include-from-path "assembler-ch3.scm")
(include-from-path "cpsizer-ch4.scm")


(define (primitive-passing-operator? operator) ;; ha!
  (any (lambda ((primop passing-function))
         (equal? operator passing-function))
       primitive-operators))


(define (passing-function-label name)
  (let (((content) (symbol-match "^pass-(.+)$" name)))
    (string->keyword content)))

(define (label . parts)
  (string->keyword (apply string-append (map ->string parts))))

(define new-label
  (let ((label-counter 0))
    (lambda parts
      (cond ((null? parts)
             (set! label-counter 0))
            (else
             (set! label-counter (+ label-counter 1))
             (apply label `(,@parts - ,label-counter)))))))

(define (maybe-register source)
  (cond ((symbol? source)
         `(,source))
        ((pair? source)
         (first source))
        (else
         '())))

(define (instruction-registers/read instruction)
  (match instruction

    (([target] '<- source)
     (union `(,target) (maybe-register source)))

    ((target '<- register/value/location)
     (maybe-register register/value/location))

    ((target '<- left x right)
     (union (maybe-register left) (maybe-register right)))

    ((target '<- operator operand)
     (maybe-register operand))

    (('if left >?< right 'goto target)
     (union (maybe-register left) (maybe-register right)
            (maybe-register target)))
    (('push register/value)
     (maybe-register register/value))

    (('goto register/value)
     (maybe-register register/value))

    (_
     '())))


(define (instruction-registers/modified instruction)
  (match instruction
    (('pop register)
     `(,register))
    ((target '<- . _)
     `(,target))
    (_
     '())))

(define ((operation-from? actions) pass-@)
  (any (lambda ((@ pass-?))
         (equal? pass-@ pass-?))
       actions))

(define ((operator-from actions) pass-@)
  (let (((@ pass-@) (find (lambda ((% pass-%))
                            (equal? pass-@ pass-%))
                          actions)))
    @))

(define (defined-function? operator)
  (symbol? operator))

(define sign (operator-from primitive-operators))

(define (used-registers machine-code)
  (match machine-code
    (()
     '())

    ((instruction . rest)
     (let ((read-registers (instruction-registers/read instruction))
           (modified-registers (instruction-registers/modified instruction)))
       (difference (union read-registers (used-registers rest))
                   (difference modified-registers read-registers))))))


(define (passing-program->assembly program/CPS)
  (let (((('define names passing-functions)
          ...
          expression) program/CPS))

    (define (argument-names function-name)
      (any (lambda (name ('lambda (arguments ... return) . _))
             (and (equal? name function-name)
                  arguments))
           names passing-functions))

    (define (assembly expression/cps registers)
      (match expression/cps
        (('lambda args body)
         (let ((label (new-label 'lambda)))
           `((result <- ,label)
             ,label
             ,@(assembly body registers))))

        (('return value)
         `((result <- ,value)
           (goto return)))

        ((pass<?> a b ('lambda (a<?>b) ('if a<?>b
                                       <then>
                                       <else>)))
         (let ((else (new-label 'else))
               (registers (union registers
                                 (maybe-register a)
                                 (maybe-register b))))
           `((if ,a ,(inversion (sign pass<?>)) ,b goto ,else)
             ,@(assembly <then> registers)
             ,else
             ,@(assembly <else> registers))))

        ((operator . operands)
         (call operator operands registers))))

    (define (call operator operands registers)
      (cond ((primitive-passing-operator? operator)
             (call-primitive operator operands registers))

            ((defined-function? operator)
             (call-defined operator operands registers))

            #;((anonymous-function? operator)
            (call-anonymous operator operands registers))))

    (define (call-primitive operator operands registers)
      (let (((left right continuation) operands))
        (match continuation

          (('lambda (result) body)
           `((,result <- ,left ,(sign operator) ,right)
             ,@(assembly body (union registers
                                     (maybe-register left)
                                     (maybe-register right)
                                     `(,result)))))

          (_ ;; a ``return'' continuation
           `((result <- ,left ,(sign operator) ,right)
             (goto ,continuation))))))

    (define (call-defined function arguments registers)

      (define (save registers)
        (map (lambda (register)
               `(push ,register))
             registers))

      (define (restore registers)
        (map (lambda (register)
               `(pop ,register))
             (reverse registers)))

      (define (pass values function)
        (let ((names (argument-names function)))
          (map (lambda (name value)
                 `(,name <- ,value))
               names values)))

      ;; body of `call-defined' begins here
      (let (((arguments ... continuation) arguments)
            (entry (passing-function-label function)))
        (match continuation

          (('lambda (result) body)
           (let* ((proceed (new-label 'proceed))
                  (sequel (assembly body registers))
                  (registers (intersection registers
                                           (used-registers sequel))))
             `(,@(save registers)
               ,@(pass arguments function)
               (push return)
               (return <- ,proceed)
               (goto ,entry)
               ,proceed
               (pop return)
               ,@(restore registers)
               ,@sequel)))

          (_ ;; tail call optimization
           `(,@(pass arguments function)
             (goto ,(passing-function-label function)))))))

    `((return <- end:)
      ,@(assembly expression '())
      ,@(append-map (lambda (name ('lambda args body))
                      `(,(passing-function-label name)
                        ,@(assembly body '())))
                    names passing-functions)
      end:
      (halt))))


[e.g.
 (begin
   (new-label)
   (passing-program->assembly
    '((define pass-!
        (lambda (n return)
          (pass= n 0
                 (lambda (n=0/1)
                   (if n=0/1
                       (return 1)
                   ;else
                       (pass- n 1
                              (lambda (n-1/3)
                                (pass-! n-1/3
                                        (lambda (!/n-1/2)
                                          (pass* n !/n-1/2
                                                 return))))))))))
      (pass-! 5 return))))

 ===> ((return <- end:)
       (n <- 5)
       (goto !:)
     !:
       (if n <> 0 goto else-1:)
       (result <- 1)
       (goto return)
     else-1:
       (n-1/3 <- n - 1)
       (push n)
       (n <- n-1/3)
       (push return)
       (return <- proceed-2:)
       (goto !:)
     proceed-2:
       (pop return)
       (pop n)
       (result <- n * !/n-1/2)
       (goto return)
     end:
       (halt))]


(define (compile scheme-program)
  (assemble (passing-program->assembly
             (passing-program scheme-program))))


[pretty-print
 (compile '((define pow (lambda (m n)
                          (if (= n 0) 1 (* m (pow m (- n 1))))))
            (define !
              (lambda (n)
                (if (= n 0) 1 (* n (! (- n 1))))))
            
            (! (pow 2 3)))) ]
