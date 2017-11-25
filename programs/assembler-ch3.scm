(use-modules (grand scheme)  ;; obviously!
             (srfi srfi-88)) ;; for the keyword thing.

(define (tree-map proc tree)
  (map (lambda (item)
         (if (pair? item)
             (tree-map proc item)
         ;else
             (proc item)))
       tree))

(define (assemble position-independent-code)

  (define (positions+labels (line labels) instruction)
    (if (keyword? instruction)
        `(,line ((,instruction . ,line) . ,labels))
    ;else
        `(,(+ line 1) ,labels)))

  (let* (((_ labels) (fold-left positions+labels
                                '(0 ())
                                position-independent-code))
         (instructions (filter (lambda (line)
                                 (not (keyword? line)))
                               position-independent-code))
         (assembled (map (lambda (instruction)
                           (tree-map (lambda (item)
                                       (if (keyword? item)
                                           (assoc-ref labels item)
                                        ;else
                                           item))
                                     instruction))
                         instructions)))
    (list->vector assembled)))


[e.g.
 (assemble
  '((n <- 5)
    (acc <- 1)
    factorial:
    (if n = 0 goto end:)
    (acc <- acc * n)
    (n <- n - 1)
    (goto factorial:)
    end:
    (halt)))
 ===> #((n <- 5)
        (acc <- 1)
        (if n = 0 goto 6)
        (acc <- acc * n)
        (n <- n - 1)
        (goto 2)
        (halt))]

;;; sweet!
