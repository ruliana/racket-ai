#lang racket
(provide make-tile
         make-tile-random
         tile-move
         tile-left
         tile-right
         tile-up
         tile-down
         tile-solved?
         tile-possible-moves
         tile-cost
         tile->table)

(define (swap a-vector p1 p2)
  (define rslt (vector-copy a-vector))
  (define tmp1 (dict-ref a-vector p1))
  (define tmp2 (dict-ref a-vector p2))
  (vector-set! rslt p1 tmp2)
  (vector-set! rslt p2 tmp1)
  rslt)

(struct tile (dimensions x numbers) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b recursive-equal?)
     (equal? (tile-numbers a) (tile-numbers b)))
   (define (hash-proc a reh)
     (equal-hash-code (tile-numbers a)))
   (define (hash2-proc a reh)
     (equal-secondary-hash-code (tile-numbers a)))])

(define (make-tile a-list)
  (define v (apply vector-immutable a-list))
  (tile (sqrt (vector-length v)) (vector-member 0 v) v))

; Better to randomize moves.
; If we shuffle the numbers, some configurations
; are not solvable.
(define (make-tile-random width [moves 300])
  (define start (make-tile (stream->list (in-range (expt width 2)))))
  (for/fold ([tile start])
            ([x (in-range moves)])
    (tile-move (car (shuffle '(left right up down))) tile)))

(define (tile-borders t)
  (define dim (tile-dimensions t))
  (define x (tile-x t))
  (define col (remainder x dim))
  (values (= 0 col) ; at-left
          (= (sub1 dim) col) ;at-right
          (< x dim) ; at-top
          (<= (- (expt dim 2) dim) x))) ; at-bottom

(define (tile-possible-moves t)
  (define-values (at-left at-right at-top at-bottom) (tile-borders t))
  (filter identity (list
                    (and (not at-left) 'left)
                    (and (not at-right) 'right)
                    (and (not at-top) 'up)
                    (and (not at-bottom) 'down))))

(define (tile-swap t pos new-pos)
  (define ts (tile-numbers t))
  (struct-copy tile t
               [x new-pos]
               [numbers (vector->immutable-vector (swap ts pos new-pos))]))

(define (tile-move direction a-tile)
  (define pos (tile-x a-tile))
  (define-values (at-left at-right at-top at-bottom) (tile-borders a-tile))
  (define dim (tile-dimensions a-tile))
  (define-syntax-rule (safe-move t (dir limit np) ...)
    (cond [(and (eq? direction (quote dir)) (not limit)) (tile-swap t pos np)] ... [else t]))
  (safe-move a-tile
             [left at-left (sub1 pos)]
             [right at-right (add1 pos)]
             [up at-top (- pos dim)]
             [down at-bottom (+ pos dim)]))

(define tile-right (curry tile-move 'right))
(define tile-left (curry tile-move 'left))
(define tile-up (curry tile-move 'up))
(define tile-down (curry tile-move 'down))

(define (tile-possibilities t)
  (define moves (tile-possible-moves t))
  (for/list ([m moves]) (tile-move m t)))

(define (tile-solved? t)
  (apply < (vector->list (tile-numbers t))))

(define (tile-cost t)
  (define dim (tile-dimensions t))
  (define numbers (tile-numbers t))
  (for/sum ([r (in-range (vector-length numbers))]
            [n numbers]
            #:unless (zero? n))
    (define row-r (quotient r dim))
    (define col-r (remainder r dim))
    (define row-n (quotient n dim))
    (define col-n (remainder n dim))
    (+ (abs (- col-n col-r))
       (abs (- row-n row-r)))))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (define test-tile (make-tile '(7 2 4 5 0 6 8 3 1)))
  (define tests
    (test-suite "Heuristic cost"
                (check-equal? 18 (tile-cost test-tile))))
  (run-tests tests))

(define (tile->table t)
  (define dim (tile-dimensions t))
  (define numbers (tile-numbers t))
  (define (make-row a-list)
    (string-join a-list " ║ " #:before-first "║ " #:after-last " ║"))
  (define (make-sep width middle left connector right)
    (string-join (build-list width (λ (x) middle))
                 connector
                 #:before-first left #:after-last right))
  (define (make-top width)    (make-sep width "═"   "╔═" "═╦═" "═╗\n"))
  (define (make-inter width)  (make-sep width "═" "\n╠═" "═╬═" "═╣\n"))
  (define (make-bottom width) (make-sep width "═" "\n╚═" "═╩═" "═╝"))
  (define (format-number n)
    (if (zero? n) " " (~a n)))
  (let loop ([rslt '()]
             [ns (vector->list numbers)])
    (if (empty? ns) (string-join (reverse rslt)
                                 (make-inter dim)
                                 #:before-first (make-top dim)
                                 #:after-last (make-bottom dim))
        (let*-values ([(first rest) (split-at ns dim)]
                      [(row) (map format-number first)])
          (loop (cons (make-row row) rslt) rest)))))