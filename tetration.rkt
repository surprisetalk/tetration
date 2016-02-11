#lang racket
(require plot)
;(require math)

(define precision 10e-15)

(define (tet a b) ; consider changing the name to ^^
  (foldr expt a (build-list (inexact->exact (- b 1)) (lambda (f) a))))

(define (^^ a b)
  (cond
    [(integer? b) (tet a b)]
    [(< b 1) (tetroot a (denominator (rationalize b precision)))]
    [(> b 1) (tet a (tet (/ (denominator (rationalize b precision))) (numerator (rationalize b precision))))]
  )
)

;find the root of (expt x x), doesn't work quite yet
;(define (tetroot x [e 25] [k 2])
;  (cond
;    [(= e 0) (/ (log (expt x x)) (log k))]
;    [else (/ (log (expt x x)) (log (tetroot x (- e 1))))]
;  )
;)

(define (disstet n b c)
  (tet (tetroot n b) (tet b c)))

(define (mean . numbers) 
  (/ (apply + numbers) (length numbers)))

(define (apply-until step done? x)
  (if (done? x) x (apply-until step done? (step x))))

(define (tetroot A [rot 2] [guess 1.5] [e 10e-15]) ; rot should be able to be entered as anything
  (let ([xLT guess][xGT guess])
    (if (< (tet guess rot) A) (set! xGT A) (set! xLT 0))
    ;(if (< (tet guess rot) A) (set! xGT (+ (tet guess rot) .1)) (set! xLT (- (tet guess rot) .1)))
    (define (rectest newguess)
      ;(display newguess)(display " ")(display xLT)(display " ")(display xGT)(newline)
      (if (< (tet newguess rot) A) (set! xLT newguess) (set! xGT newguess))
      (if (or (< (abs (- (tet newguess rot) A)) e) (= newguess 0)) newguess (rectest (mean xLT xGT)))
     )
    (rectest guess)
   )
)

;(define continuation (s x) ; continues upon (a^^b)^^x as (a^^b^^x) where s=(a^^b)
  

    #|(plot (list (function (lambda (x) (tetroot x 1)) #:color 1 #:label "x ^^ 1/1")
                (function (lambda (x) (tetroot x 2)) #:color 2 #:label "x ^^ 1/2")
                (function (lambda (x) (tetroot x 3)) #:color 3 #:label "x ^^ 1/3")
                (function (lambda (x) (tetroot x 4)) #:color 4 #:label "x ^^ 1/4")
                (function (lambda (x) (tetroot x 5)) #:color 5 #:label "x ^^ 1/5")
                (function (lambda (x) (tetroot x 6)) #:color 6 #:label "x ^^ 1/6")
                (function (lambda (x) (tetroot x 100)) #:color 7 #:label "x ^^ 1/100")
                ;(function (lambda (x) (expt (exp 1) (/ (exp 1)))) #:color 8 #:label "e ^ (1/e)")
                )
                        #:x-min 0 #:x-max 5
                        #:y-min 0 #:y-max 5
                        ;#:out-file "hw4log.jpg"
     )|#
    
 ;define tet as a condition where tet = tetup above 1 and tet = tetroot under 1
    ;get (tet 3/2)
    
 ;research tet mod n?
