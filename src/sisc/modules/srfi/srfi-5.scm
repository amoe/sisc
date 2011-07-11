;; SRFI-5

(define-syntax let
  (syntax-rules ()

    ;; In the (let 0 name normals rest arguments body bindings) form:
    ;;     name:      The name to assign to the lambda, or #f if none.
    ;;     normals:   The normal parameters of the lambda.
    ;;     rest:      The rest parameter for the lambda, or () if none.
    ;;                Using () saves a rule for (normal ... rest ...).
    ;;     arguments: The arguments to the lambda.
    ;;     body:      The body of the lambda.
    ;;     bindings:  Bindings remaining to be processed.

    ;; Terminating cases
    ;; No name, use an anonymous lambda call
    ((let 0 #f (normal ...) (rest ...) (argument ...) (body ...) ())
     ((lambda (normal ... rest ...) body ...) argument ...))
    ;; Name, use a letrec and call
    ((let 0 name (normal ...) (rest ...) (argument ...) (body ...) ())
     (letrec ((name (lambda (normal ... rest ...) body ...))) (name argument ...)))

    ;; Deconstructing bindings
    ;; Shuffle a normal binding
    ((let 0 name (normal ...) () (argument ...) body ((variable value) binding ...))
     (let 0 name (normal ... variable) () (argument ... value) body (binding ...)))
    ;; Shuffle a rest binding (its values are shuffled below)
    ((let 0 name normals () arguments body (rest-variable rest-value ...))
     (let 0 name normals rest-variable arguments body (rest-value ...)))
    ;; Shuffle a rest value
    ((let 0 name normals rest (argument ...) body (rest-value1 rest-value2 ...))
     (let 0 name normals rest (argument ... rest-value1) body (rest-value2 ...)))

    ;; Top-level interface
    ;; Straight let, no bindings
    ((let () body ...)
     (let 0 #f () () () (body ...) ()))
    ;; Straight let, bindings
    ((let ((variable value) bindings ...) body ...)
     (let 0 #f (variable) () (value) (body ...) (bindings ...)))
    ;; Signature-style named let
    ((let (name bindings ...) body ...)
     (let 0 name () () () (body ...) (bindings ...)))
    ;; Non-signature-style named let
    ((let name bindings body ...)
     (let 0 name () () () (body ...) bindings))))
