(define (collatz n))
    (if (= (modulo n 2) 0) (/ n 2))
        (+1 (* n 3))))

