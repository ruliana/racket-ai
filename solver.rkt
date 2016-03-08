#lang racket
(require racket/generic)
(require data/heap)
(require rackjure/threading)

(provide gen:problem
         step-cost actions new-state goal?
         node-state node-parent node-action node-cost
         solver-a*)

(module+ test
  (require rackunit)
  (require rackunit/text-ui))

(define (make-node state [parent #f] [action #f] [cost 0])
  (node state parent action cost))

(struct node (state parent action cost) #:transparent)

(define (child-node problem parent action)
  (define parent-state (node-state parent))
  (define nstate (new-state problem parent-state action))
  (define ncost (+ (node-cost parent)
                   (step-cost problem parent-state action)))
  (node nstate parent action ncost))

(define (make-heap-queue heuristic-proc . elements)
  (define (node<=? heuristic-proc)
    (define (sum-cost a-node) (+ (heuristic-proc (node-state a-node))
                                 (node-cost a-node)))
    (λ elements (apply <= (map sum-cost elements))))
  (define (node-better<=? heuristic-proc)
    (define (tuple a-node) (list (+ (n-cost a-node) (h-cost a-node)) (h-cost a-node)))
    (define (n-cost a-node) (node-cost a-node))
    (define (h-cost a-node) (heuristic-proc (node-state a-node)))
    (λ elements (apply andmap <= (map tuple elements))))
  (define h (make-heap (node<=? heuristic-proc)))
  (define (zip a b) (map cons a b))
  (define nodes (map make-node elements))
  (heap-add-all! h nodes)
  (heap-queue (make-immutable-hash (zip elements nodes)) h))

(define-generics node-queue
  (empty? node-queue)
  (member? node-queue element)
  (pop node-queue)
  (peek node-queue)
  (append node-queue . elements)
  (size node-queue))

(struct heap-queue (hash heap)
  #:transparent
  #:methods gen:node-queue
  [(define (empty? q) (hash-empty? (heap-queue-hash q)))
   (define (member? q a-node)
     (define h (heap-queue-hash q))
     (define s (node-state a-node))
     (hash-has-key? h s))
   (define (peek q) (heap-min (heap-queue-heap q)))
   (define (pop q)
     (define hh (heap-queue-hash q))
     (define hp (heap-copy (heap-queue-heap q)))
     (define removable (heap-min hp))
     (let ([new-hash (hash-remove hh (node-state removable))])
       (heap-remove-min! hp)
       (heap-queue new-hash hp)))
   (define (append q . elements)
     (define (heap-remove-all! a-heap elements)
       (for ([e elements]) (heap-remove! a-heap e)))
     (define (part elements)
       (for/fold ([new-nodes '()]
                  [old-nodes '()])
                 ([e elements])
         (define s (node-state e))
         (define present (hash-ref hh s #f))
         (cond
           [(not present)
            (values (cons e new-nodes) old-nodes)]
           [(< (node-cost e) (node-cost present))
            (values (cons e new-nodes) (cons present old-nodes))]
           [else (values new-nodes old-nodes)])))
     (define hh (heap-queue-hash q))
     (define-values (addable removable) (part elements))
     (define less-hh
       (for/fold ([rslt hh]) ([r removable])
         (hash-remove rslt (node-state r))))
     (define more-hh
       (for/fold ([rslt less-hh]) ([a addable])
         (hash-set rslt (node-state a) a)))
     (define hp (heap-copy (heap-queue-heap q)))
     (heap-remove-all! hp removable)
     (heap-add-all! hp addable)
     (heap-queue more-hh hp))
   (define (size q) (hash-count (heap-queue-hash q)))])

(module+ test
  (define (fixed-cost x) x)
  (define empty-queue (make-heap-queue fixed-cost))
  (define simple-queue (make-heap-queue fixed-cost 3 2 1))
  (define popped-queue (~> simple-queue pop))
  (define poppe2-queue (~> simple-queue pop pop))
  (define poppe3-queue (~> simple-queue pop pop pop))
  (define append0-queue (append simple-queue (make-node 0)))
  (define append1-queue (append simple-queue (make-node 3 #f #f -10)))
  (define tests
    (test-suite "Heap and Hash queue"
                (check-true (empty? empty-queue))
                (check-true (member? simple-queue (make-node 2)))
                (check-true (member? simple-queue (make-node 2 #f #f 10)))
                (check-false (member? simple-queue (make-node 999)))
                (check-equal? 1 (node-state (peek simple-queue)))
                (check-equal? 2 (node-state (peek popped-queue)))
                (check-false (member? popped-queue (make-node 1)))
                (check-equal? 3 (node-state (peek poppe2-queue)))
                ;(check-exn exn:fail? (peek poppe3-queue))
                (check-true (empty? poppe3-queue))
                (check-true (member? append0-queue (make-node 0)))
                (check-true (member? append1-queue (make-node 3)))
                (check-equal? 3 (node-state (peek append1-queue)))
                ))
  (run-tests tests))

(define-generics problem
  (actions problem state)
  (step-cost problem state action)
  (new-state problem state action)
  (heuristic-cost problem state)
  (goal? problem state))

(define (solver-a* problem initial-state)
  (define initial-heap (make-heap-queue (curry heuristic-cost problem) initial-state))
  (define (search-fringe fringe visited steps)
    (define a-node (peek fringe))
    (define rest (pop fringe))
    (define this-state (node-state a-node))
    (define new-visited (set-add visited this-state))
    (define (make-child action) (child-node problem a-node action))
    (define (not-visited? this-node) (not (set-member? new-visited (node-state this-node))))
    (define children (~>> (actions problem this-state)
                          (map make-child)
                          (filter not-visited?)))
    (define new-fringe (apply append rest children))
    (let ([cost (node-cost a-node)]
          [heuristic (heuristic-cost problem this-state)]
          [fringe-size (size new-fringe)])
      (printf "~a) cost: ~a + ~a = ~a fringe: ~a\n"
              steps
              cost
              heuristic
              (+ cost heuristic)
              fringe-size))
    (cond
      [(empty? fringe) #f]
      [(goal? problem this-state) a-node]
      [else (search-fringe new-fringe new-visited (add1 steps))]))
  (search-fringe initial-heap (set) 1))


