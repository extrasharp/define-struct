(define-module (extrasharp define-struct))

(use-modules (ice-9 format))

(define (format-symbol format-str . args)
  (string->symbol (apply format #f format-str args)))

(define-syntax define-struct
  (lambda (x)
    (let* ((expr (syntax->datum x))
           (struct-name (cadr expr))
           (struct-fields (cddr expr))
           (record-fields (map cadr struct-fields))
           (record-type-define
             `(define ,struct-name
                (make-record-type ,(symbol->string struct-name) ',record-fields)))
           (constructor-define
             `(define ,(format-symbol "_make-~a" struct-name)
                (record-constructor ,struct-name)))
           (predicate-define
             `(define ,(format-symbol "~a?" struct-name)
                (record-predicate ,struct-name)))
           (field-defines
             (let loop ((lst struct-fields)
                        (acc '()))
               (if (null? lst)
                   (reverse acc)
                   (let* ((obj (car lst))
                          (is-mutable (eq? (car obj) 'mut))
                          (name (cadr obj))
                          (accessor `(define ,(format-symbol "~a-~a" struct-name name)
                                       (record-accessor ,struct-name ',name)))
                          (modifier `(define ,(format-symbol "~a-~a!" struct-name name)
                                       (record-modifier ,struct-name ',name))))
                     (loop
                       (cdr lst)
                       (if is-mutable
                           (cons* accessor modifier acc)
                           (cons accessor acc))))))))
      (datum->syntax x
        `(begin ,record-type-define
                ,constructor-define
                ,predicate-define
                ,@field-defines)))))

(export define-struct)

