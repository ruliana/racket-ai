#lang racket
(require racket/generic)
(require "solver.rkt")
(require "eight-tiles.rkt")

(struct tiles-problem ()
  #:methods gen:problem
  [(define (step-cost prob state action) 1)
   (define (actions prob state) (tile-possible-moves state))
   (define (new-state prob state action) (tile-move action state))
   (define (heuristic-cost prob state) (tile-cost state))
   (define (goal? prob state) (tile-solved? state))])

(define (display-solution node)
  (define steps (node-cost node))
  (define (display-node a-node)
    (displayln (tile->table (node-state a-node))))
  (define (loop a-node)
    (when a-node
      (loop (node-parent a-node))
      (if (node-action a-node)
          (printf "step: ~a move: ~a\n" (node-cost a-node) (node-action a-node))
          (printf "initial state, ~a steps\n" steps))
      (display-node a-node)))
  (loop node))

(define x (make-tile '(6 0 8 7 2 5 3 4 1)))
(define-values (rslt cpu-time real-time gc-time)
  (time-apply solver-a* (list (tiles-problem) x)))

(define solution (car rslt))

(display-solution solution)
(printf "cpu: ~ams real: ~ams gc: ~ams\n" cpu-time real-time gc-time)
