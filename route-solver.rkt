#lang racket
(require "solver.rkt")

(define routes
  (hash 'a (hash 'b 1
                 'c 2)
        'b (hash 'a 1)
        
        'c (hash 'a 2
                 'd 1)
        
        'd (hash 'c 1)))

(struct route (city) #:transparent)

(struct routes-problem ()
  #:methods gen:problem
  [(define (step-cost prob state action)
     (define from (hash-ref routes (route-city state)))
     (hash-ref from action))
   (define (actions prob state)
     (hash-keys (hash-ref routes (route-city state))))
   (define (new-state prob state action)
     (route action))
   (define (heuristic-cost prob state) 0)
   (define (goal? prob state)
     (equal? 'd (route-city state)))])

(solver-a* (routes-problem)
           (route 'a))