#lang scheme/gui
;MY GA implementation of the travelling sales man problem
;This implementation runs in PLT Scheme 4.2.1

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
  (map circle-xy (range 0 37 10) (build-list 36 (lambda (x) 100))))

(define (swap nodes pos1 pos2)
  (swap-aux nodes pos1 pos2 '() 0))

(define (swap-aux l pos1 pos2 templist currpos)
 (cond [(= currpos (length l)) templist]
       [(= currpos pos1) (swap-aux l pos1 pos2 (append templist (list (list-ref l pos2))) (+ 1 currpos))]
       [(= currpos pos2) (swap-aux l pos1 pos2 (append templist (list (list-ref l pos1))) (+ 1 currpos))]
       [else (swap-aux l pos1 pos2 (append templist (list (list-ref l currpos))) (+ 1 currpos))]
  ))

;Finds the cyle in the 2 parents, the result is a list in the format
;((value position-value-in-parent1))
;Parameters
;parent1, parent2 The 2 parents
;pos The position that we are looking at now
;cycle-list The results of the cycle in the format ((value position-of-value-in-parent1) ...)
(define (find-cycle parent1 parent2 pos cycle-list)
  (let* ([member (list-ref parent1 pos)]
         [member-2 (list-ref parent2 pos)]
         [pos-member-2-in-1 (list-index 
                        (lambda (x) (eq? x member-2))
                        parent1)])
    (if (assoc member-2 cycle-list)
        (cons (list member pos) cycle-list)
        (find-cycle parent1 parent2 
                    pos-member-2-in-1 
                    (cons (list member pos) cycle-list))
    
    )))

(define (mutate list-of-node)
  (let* ([length (length list-of-node)]
        [pos1 (random length)]
        [pos2 (random length)])
    (swap list-of-node pos1 pos2)))

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

;Calculate fitness for a listofnodes output is a list of fitness values
(define (map-fitness list-of-nodes)
  (map fitness list-of-nodes))

;Combine the fitness score to the node so that we can sort them later
(define (combine-fitness fitness-map nodes-list)
  (map (lambda (x y)
         (list x y))
       fitness-map
       nodes-list))

;Sort it so that we can determine what nodes are not feasible for processing
(define (sort-fitness-nodes fitness-nodes)
  (sort fitness-nodes (lambda (x y)
                        (let ([f1 (car x)]
                              [f2 (car y)])
                          (< f1 f2)))))

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

;Generate a number of node-list for use as seed for the algo
(define (generate-all-nodes list-of-node num)
  (if (= num 0)
      '()
      (cons (generate list-of-node) (generate-all-nodes list-of-node (- num 1)))))


;Windowing stuff
(define frame (new frame% [label "TSP"] [width 300] [height 300]))
; Create a 300 x 300 bitmap
(define face-bitmap (make-object bitmap% 300 300))
; Create a drawing context for the bitmap
(define bm-dc (make-object bitmap-dc% face-bitmap))
(send bm-dc clear)
(define canvas (new canvas% [parent frame]
                    [paint-callback (lambda (canvas dc)
                                      (send dc draw-bitmap face-bitmap 0 0))]))
(define dc (send canvas get-dc))

(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))

(define (draw-node dc list-of-node)
  (map (lambda (xy) 
         (let ([x (first xy)]
               [y (second xy)])
         (send dc draw-rectangle (+ 150 x) (+ 150 y) 2 2)))
       list-of-node
  ))

(define (draw-line dc n1 n2 offsetx offsety)
  (send dc draw-line (+ (first n1) offsetx) (+ (second n1) offsety) 
        (+ (first n2) offsetx) (+ (second n2) offsety)))

(define (draw-lines dc list-of-node offsetx offsety)
  (if (= (length list-of-node) 1)
      #t
      (let ([n1 (first list-of-node)]
            [n2 (second list-of-node)])
        (draw-line dc n1 n2 offsetx offsety)
        (draw-lines dc (cdr list-of-node) offsetx offsety))))

(define (draw-window)
  (draw-node bm-dc list-of-node)
  (draw-lines bm-dc list-of-node 150 150)
  (send frame show #t))

;Process a list of nodes 
;Nodes are generated and pass into this function
;This fucntion will calculated the fitness and remove the "unfit" ones
;Then it will do recombination/mutation as necessary
(define (run nodes)
  (let* ([fitness (map-fitness nodes)]
        [fitness-map (sort-fitness-nodes (combine-fitness fitness nodes))]
        [fit-nodes (drop-right fitness-map 5)])
    fit-nodes
    ))