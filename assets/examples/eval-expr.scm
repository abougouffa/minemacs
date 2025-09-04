;; (load "pmatch.scm")

(define eval-expr
  (lambda (expr env)
    (pmatch expr
      [`,x (guard (symbol? x))
        (env x)]
      [`(lambda (,x) ,body)
        (lambda (arg)
          (eval-expr body (lambda (y)
                            (if (eq? x y)
                                arg
                                (env y)))))]
      [`(,rator ,rand)
       ((eval-expr rator env)
        (eval-expr rand env))])))
