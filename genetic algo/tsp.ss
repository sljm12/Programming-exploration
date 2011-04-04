#lang scheme
;MY GA implementation of the travelling sales man problem
(require srfi/1)

;(define list-of-node
; (m '((0 0) (0 1) (1 1) (1 0)))

;Computes a range of numbers
;starting from start to < end
;increasing by step each time
(define (range start end step)
    (unfold (lambda (x) (= x end))
            (lambda (x) (* x step))
            (lambda (x) (+ x 1)) start))

(define (deg-to-rad deta)
  (* (/ pi 180) deta))

(define (circle-x angle-deg radius)
  (* radius (cos (deg-to-rad angle-deg))))

(define (circle-y angle-deg radius)
  (* radius (sin (deg-to-rad angle-deg))))

(define (circle-xy angle-deg radius)
  (list (circle-x angle-deg radius) (circle-y angle-deg radius)))

(define list-of-node 
  (map circle-xy (range 0 37 10) (build-list 36 (lambda (x) 10))))

(define (distance n1 n2)
  (let* ([n1x (first n1)]
        [n1y (second n1)]
        [n2x (first n2)]
        [n2y (second n2)]
        [diffx (- n1x n2x)]
        [diffy (- n1y n2y)])
        
    (sqrt (+ (* diffx diffx)
          (* diffy diffy)))
    ))

;Fitness is the sum of the distance of all the nodes
;order is a list of node numbers where the first node number must appear as the last number also to close the loop
(define (fitness order)
  (if (= (length order) 1)
      0
      (+ (distance (first order) (second order)) (fitness (cdr order)))
  ))

;Helper to copy the first node to the last node so that the loop is a close loop
(define (copy-first-to-last nodes)
  (let ([first (first nodes)])
    (append nodes (list first))))

;Generate a random node list for use as a seed for the GA algo
(define (generate list-of-node)
  (if (zero? (length list-of-node))
      '()
  (let* ([pos (random (length list-of-node))]
         [selected-node (list-ref list-of-node pos)])
    (cons selected-node
          (generate (remove (lambda (x) (eq? x selected-node)) list-of-node))))))
          