
(library (machine-code disassembler arm-private)
  (export define-encoding)
  (import (rnrs)
          (for (machine-code disassembler private)
               (meta -1)))
  
  ;; Syntax for defining instruction encodings. The fields of the
  ;; instructions are written with the index of the top field bit.
  ;; Fields can be given an identifier which will be bound to the
  ;; field value, or a field constraint. This matches what is seen in
  ;; the instruction set encoding chapters of the ARM manuals.

  (define-syntax define-encoding
    (lambda (x)
      (define (get-next-top-bit top-bit field-spec*)
        (syntax-case field-spec* ()
          [() -1]
          [((next-top-bit . _) field-spec* ...)
           ;; FIXME: verify that the bit is lower
           (and (fixnum? (syntax->datum #'next-top-bit))
                (fx<? (syntax->datum #'next-top-bit) (syntax->datum top-bit)))
           (syntax->datum #'next-top-bit)]))
      (define (wrap-body name lhs* rhs* body)
        (with-syntax ([(lhs* ...) (reverse lhs*)]
                      [(rhs* ...) (reverse rhs*)])
          (with-syntax ([err #`(raise-UD #,(string-append "Unallocated "
                                                          (symbol->string (syntax->datum name))
                                                          " op")
                                         `(lhs* ,rhs*) ...)])
            (let f ((body body))
              (syntax-case body (select decode)
                [(select pc instruction)
                 #'err]
                [(select pc instruction option option* ...)
                 #`(or (option pc instruction)
                       #,(f #'(select pc instruction option* ...)))]
                [(decode [test* expr*] ...)
                 #'(let ((lhs* rhs*) ...)
                     (cond [test* expr*] ...
                           [else err]))]
                #;
                [body
                 #'(let ((lhs* rhs*) ...)
                     body)])))))
      (syntax-case x ()
        [(_ (encoding-name pc instruction field-spec* ...))
         #'(define-encoding (encoding-name pc instruction field-spec* ...)
             (select pc instruction))]
        [(_ (encoding-name pc instruction field-spec* ...)
            body)
         (and (identifier? #'encoding-name) (identifier? #'instruction))
         (let loop ([field-spec* #'(field-spec* ...)]
                    [eq-mask 0] [eq-bits 0]
                    [neq-mask 0] [neq-bits 0]
                    [lhs* '()] [rhs* '()])
           (syntax-case field-spec* (= !=)
             [()
              (with-syntax ([wrapped-body (wrap-body #'encoding-name lhs* rhs* #'body)])
                (unless (= (bitwise-and eq-bits eq-mask) eq-bits)
                  (syntax-violation 'define-encoding "Bits do not match the mask, bad constraints?"
                                    x field-spec*))
                #`(begin
                    (define (decode-it pc instruction)
                      wrapped-body)
                    (define (encoding-name pc instruction)
                      (and (eqv? (bitwise-and instruction #,eq-mask) #,eq-bits)
                           (or (eqv? #,neq-mask 0)
                               (not (eqv? (bitwise-and instruction #,neq-mask) #,neq-bits)))
                           (decode-it pc instruction)))))]
             [((top-bit) field-spec* ...)
              (fixnum? (syntax->datum #'top-bit))
              ;; Ignore anything of the form (<n>).
              (loop #'(field-spec* ...) eq-mask eq-bits neq-mask neq-bits lhs* rhs*)]
             [((top-bit name) field-spec* ...)
              (and (fixnum? (syntax->datum #'top-bit)) (identifier? #'name))
              ;; Defines a field.
              (let* ((next-top-bit (get-next-top-bit #'top-bit #'(field-spec* ...))))
                (with-syntax ((accessor #`(bitwise-bit-field instruction
                                                             (+ #,next-top-bit 1) (+ top-bit 1))))
                  (loop #'(field-spec* ...) eq-mask eq-bits neq-mask neq-bits
                        #`(name #,@lhs*) #`(accessor #,@rhs*))))]
             [((top-bit (= field-bits)) field-spec* ...)
              (and (fixnum? (syntax->datum #'top-bit)) (fixnum? (syntax->datum #'field-bits)))
              ;; Defines a constraint (the field must be equal to field-bits).
              (let* ((next-top-bit (get-next-top-bit #'top-bit #'(field-spec* ...)))
                     (bottom-bit (fx+ next-top-bit 1))
                     (width (fx+ (fx- (syntax->datum #'top-bit) bottom-bit) 1)))
                (loop #'(field-spec* ...)
                      (bitwise-ior eq-mask
                                   (bitwise-arithmetic-shift-left (- (bitwise-arithmetic-shift-left 1 width) 1)
                                                                  bottom-bit))
                      (bitwise-ior eq-bits (bitwise-arithmetic-shift-left (syntax->datum #'field-bits) bottom-bit))
                      neq-mask neq-bits
                      lhs* rhs*))]
             [((top-bit (!= field-bits) name) field-spec* ...)
              (and (fixnum? (syntax->datum #'top-bit)) (fixnum? (syntax->datum #'field-bits)))
              ;; Defines a field with a constraint (the field must be unequal to field-bits).
              (let* ((next-top-bit (get-next-top-bit #'top-bit #'(field-spec* ...)))
                     (bottom-bit (fx+ next-top-bit 1))
                     (width (fx+ (fx- (syntax->datum #'top-bit) bottom-bit) 1)))
                (with-syntax ((accessor #`(bitwise-bit-field instruction
                                                             (+ #,next-top-bit 1) (+ top-bit 1))))
                  (loop #'(field-spec* ...)
                        eq-mask neq-bits
                        (bitwise-ior neq-mask
                                     (bitwise-arithmetic-shift-left (- (bitwise-arithmetic-shift-left 1 width) 1)
                                                                    bottom-bit))
                        (bitwise-ior neq-bits
                                     (bitwise-arithmetic-shift-left (syntax->datum #'field-bits) bottom-bit))
                        #`(name #,@lhs*) #`(accessor #,@rhs*))))]))]))))