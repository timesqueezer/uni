#lang racket

#|
################################################################################
##                                                                            ##  
##            This file is part of the se3-bib Racket module v3.0             ##  
##                Copyright by Leonie Dreschler-Fischer, 2010                 ##
##              Ported to Racket v6.2.1 by Benjamin Seppke, 2015              ##  
##                                                                            ##  
################################################################################
|#

(provide paint-map  ; paint a pix map image
         draw-contour-map 
         ; draw the contours of connected regions of a pix map image
         *the-current-pixmap*; the current pix map to be painted
         *smiley-map* ; a sample image
         *pixel-width*
         make-empty-map make-constant-map ; contructor
         map-rows map-cols ;the size of the current map
         map-field ; retrieve a cell of the pix-map
         for-each-coord-do for-each-row-col-do; traverse the current map
         make-map-coord map-coord-row map-coord-col in-bounds?
         equal-coord?
         print-map-coord display-position
         ; contructor and accessors for grid coordinates
         canvas-pos-lower-left-corner canvas-pos-upper-left-corner
         canvas-pos-lower-right-corner canvas-pos-upper-right-corner
         canvas-pos-center ; map grid-coordinates to the canvas positions
         
         *the-current-color-map*; the current color map
         *bw-color-map*  ; a sample color map
         set-pix-map
         make-color-map ; construct a color map
         color-map-look-up ; an acessor for color entries
         set-color-map
         init-drawing ; create a canvas and set the parameters
         draw-cell ; paint one square cell of the pixmap
         ;   set-cell ; paint one square cell of the pixmap to a specific color
         draw-marker draw-big-marker draw-plain-marker
         ; draw a circular marker at the center of the cell
         ; region growing 
         region-list region-pos region-nodes 
         neighbors neighbor-list neighbors-of
         make-node node-pos node-info display-node retrieve-node
         neighbor-list->edge-list
         pixmap->connectivity-graph
         ; constructor, accessors for nodes of a graph 
         make-graph graph-nodes graph-edges
         make-edge node1 node2 display-edge
         draw-edge draw-edges 
         draw-node draw-nodes big-marker-node
         draw-graph *nikolaus* *niko-map*
         ) 

(require 
 2htdp/image
 (except-in 2htdp/universe space)
 lang/posn
 se3-bib/tools-module
 se3-bib/vector-graphics-module)

#|
This module provides function for painting pixels on a grid
  of cells. Pixmap images can be specified as a matrix of ascii-characters -
  the ascii-characters will be mapped to colors using a color map.

  The rows and columns of the grid are numbered starting by one.
|#

(define *smiley-map*
  (vector 
   "......."    
   ".*...*."    
   "......."    
   "...*..."    
   "......."    
   ".*...*." 
   "..***.."))

(define *house-pix-map*
  (vector 
   "1111111"    
   "1111111"    
   "2222333"    
   "2222333"    
   "2226333"    
   "4444555" 
   "4444555"))



(define (make-empty-map rows cols)
  ; create a pixmap initialized with spaces
  (make-vector rows 
               (make-string cols #\space)))

(define (make-constant-map rows cols background)
  ; create a pixmap initialized with the background character given
  (make-vector rows 
               (make-string cols background)))

(define *the-current-pixmap* *smiley-map*)

(define (set-pix-map pix-map) 
  (set! *the-current-pixmap* pix-map))

(define-struct map-coord (row col)) 
; a structure with two fields
; constructor make-map-coord
; accessors: map-coord-row, map-coord-col

(define (equal-coord? pos1 pos2)
  (and (equal? (map-coord-row pos1) (map-coord-row pos2))
       (equal? (map-coord-col pos1) (map-coord-col pos2))
       ))

(define (print-map-coord pos)
  (let (( printer (list "row: " (map-coord-row pos)
                        " col: " (map-coord-col pos))))
    (display printer)
    printer))


(define (display-position coord)
  (display (list "row: " (map-coord-row coord) 
                 " col:" (map-coord-col coord) " ")))

(define (map-rows map)
  ; how many rows has the map?
  (vector-length map))

(define (map-cols map)
  ; how many columns has the map?
  (string-length (vector-ref map 1)))

(define (in-bounds? coord)
  (and (<= 1  (map-coord-row coord) 
           (map-rows *the-current-pixmap*))
       (<= 1  (map-coord-col coord)
           (map-cols *the-current-pixmap*))))

(define (map-field coord)
  ; retrieve a square of the map
  (if (in-bounds? coord)
      (string-ref (vector-ref *the-current-pixmap* 
                              (sub1 (map-coord-row coord) ))
                  (sub1 (map-coord-col coord) ))
      #\space))



(define (for-each-coord-do p)
  ; perform (p coord) on each cell of the map
  (let* ((rows (map-rows *the-current-pixmap*))
         (cols (map-cols *the-current-pixmap*)))
    (do ((row 1 (add1 row) ))
      ((> row rows) #t)
      (do ((col 1 (add1 col) ))
        ((> col cols) #t)
        (p (make-map-coord row col))))))

(define (for-each-row-col-do p)
  ; perform (p row col) on each cell of the maze
  (let* ((rows (map-rows *the-current-pixmap*))
         (cols (map-cols *the-current-pixmap*)))
    (do ((row 1 (add1 row) ))
      ((> row rows) #t)
      (do ((col 1 (add1 col) ))
        ((> col cols) #t)
        (p row col)))))


; ==================================================
; drawing functions and constants
; ==================================================


(define *pixel-width* 12)
(define *line-color* 'black)
(define *background-color* 'white)

(define (set-pixel-width)
  (let ((max-no-map-cells (max 
                           (map-rows *the-current-pixmap*)
                           (map-cols *the-current-pixmap*)))
        (max-screen-cells 400))
    (set! *pixel-width* (quotient max-screen-cells max-no-map-cells ))))


(define (canvas-pos-lower-left-corner coord)
  ; lower left corner
  (make-posn (* (map-coord-col coord) *pixel-width*) 
             (* (add1 (map-coord-row coord)) *pixel-width*)))

(define (canvas-pos-lower-right-corner coord)
  ; lower left corner
  (make-posn (* (add1 (map-coord-col coord)) *pixel-width*) 
             (* (add1 (map-coord-row coord)) *pixel-width*)))

(define (canvas-pos-upper-left-corner coord)
  ; lower left corner
  (make-posn (* (map-coord-col coord) *pixel-width*) 
             (* (map-coord-row coord) *pixel-width*)))

(define (canvas-pos-upper-right-corner coord)
  ; lower left corner
  (make-posn (* (add1 (map-coord-col coord)) *pixel-width*) 
             (* (map-coord-row coord) *pixel-width*)))

(define (canvas-pos-center coord)
  ; the center of the square grid cell
  (make-posn (round (* (+ (map-coord-col coord) 0.5) *pixel-width*)) 
             (round (* (+ (map-coord-row coord) 0.5) *pixel-width*))))

;=================================================================

(define (make-color-map 
         pixel-codes; a list of characters
         colors; a list of colors (see draw.ss)
         ) 
  (map cons pixel-codes colors))

(define (make-color-pixel-map 
         color-map
         pix-size ) 
  ; a table of colored squares to paint the pixels
  (map (lambda (code-color-pair)
         (cons (car code-color-pair)
               (square pix-size 
                       'solid 
                       (cdr code-color-pair))))
       color-map))

(define *bw-color-map* 
  (make-color-map (list #\. #\*) (list 'white 'black)))

(define *house-color-map* (make-color-map
                           (list  #\1 #\2 #\3 #\4 #\5 #\6)
                           (list 'red 'green 'blue 'yellow 'black 'red)))


(define *the-current-color-map*  *bw-color-map*)

(define *the-current-color-pixel-map*  
  (make-color-pixel-map 
   *bw-color-map*
   *pixel-width*)) 

(define (set-color-map map)
  (set! *the-current-color-map* map)
  (set! *the-current-color-pixel-map* 
        (make-color-pixel-map map *pixel-width*))
  )

(define (color-map-look-up pixel-code)
  (let ((encoding 
         (assoc pixel-code  *the-current-color-map*)))
    (if encoding (cdr encoding)
        *background-color*)))

(define (color-map-look-up-map pixel-code map)
  (let ((encoding 
         (assoc pixel-code  map)))
    (if encoding (cdr encoding)
        *background-color*)))

(define (pixel-map-look-up pixel-code )
  (let ((pixel 
         (assoc pixel-code *the-current-color-pixel-map*)))
    (if pixel (cdr pixel)
        (square *pixel-width* 
                'solid 
                *background-color*))))

(define (cellcolor coord)
  ; retrieve the color code of the cell
  (color-map-look-up (map-field coord)))

(define (cellpix coord)
  ; retrieve the color rectangle of the cell
  (pixel-map-look-up (map-field coord)))

(define (draw-cell coord) ; draw one square of the map
  (let* ([x (round (* (+ (map-coord-col coord) 0.5) *pixel-width*))]
         [y (round (* (+ (map-coord-row coord) 0.5) *pixel-width*))]
         [pix (cellpix coord)])
    (unless  (equal? pix #f)
      (set-scene! (place-image pix x y (get-scene) ))
      ) ))

#|
(define (set-cell coord) ; draw one square of the map
    (draw-solid-rect
     (canvas-pos-upper-left-corner coord) 
     *pixel-width* *pixel-width* (cellcolor coord)))
|#


(define (draw-plain-marker coord color) 
  ; draw a circular marker at the center of the cell without a border 
  (let* 
      ([x (round (* (+ (map-coord-col coord) 0.5) *pixel-width*))]
       [y (round (* (+ (map-coord-row coord) 0.5) *pixel-width*))]
       [r (max (- (quotient *pixel-width* 2) 2) 3)];ensure at leat 3 pixel radius
       [thecolor (if (equal? color 'none)
                     *line-color* 
                     color) ];Farbe
       [pix (circle r 'solid thecolor)])
    (unless  (equal? color 'none)
      (set-scene!
       (place-image pix x y (get-scene))
       ))))


(define (marker-with-border
         coord 
         color
         inner-radius 
         border-thickness)
  ; draw a circular marker at the center of the cell without a border 
  (let* 
      ([x (round (* (+ (map-coord-col coord) 0.5) *pixel-width*))]
       [y (round (* (+ (map-coord-row coord) 0.5) *pixel-width*))]
       [r (max (- (quotient *pixel-width* 2) 2) 3)];ensure at leat 3 pixel radius
       [thecolor (if (equal? color 'none)
                     *line-color* 
                     color) ];Farbe
       [pix1 (circle (+ r border-thickness) 'solid thecolor)]
       [pix2 (circle r  'solid *background-color*)])
    (unless  (equal? color 'none)    
      (set-scene! 
       (place-image pix2 x y 
                    (place-image pix1 x y (get-scene)))))))


(define (draw-marker coord color) 
  ; draw a circular marker at the center of the cell with a border
  ; sized to fit into a single cell
  (let* ((inner-radius (round (* 0.9 *pixel-width*)))
         (border 1)); 1 pixel border
    (marker-with-border coord color inner-radius border)))


(define (draw-big-marker coord color) 
  ; draw a circular marker at the center of the cell with a border 
  (let* ((inner-radius (round (* 1.5 *pixel-width*)))
         (border 2)) ;2 pixels border
    (marker-with-border coord color inner-radius border)))

(define  (draw-the-frame) 
  ; draw the outer border of the map
  (let ((upper-left (canvas-pos-upper-left-corner (make-map-coord 1 1)))
        (upper-right (canvas-pos-upper-right-corner 
                      (make-map-coord 1 (map-cols *the-current-pixmap*))))
        (lower-left (canvas-pos-lower-left-corner 
                     (make-map-coord (map-rows *the-current-pixmap*) 1)))
        (lower-right (canvas-pos-lower-right-corner 
                      (make-map-coord (map-rows *the-current-pixmap*)
                                      (map-cols *the-current-pixmap*)))))
    (draw-poly 
     (list upper-left upper-right lower-right lower-left) *line-color*)))


(define (is-boundary? pos1 pos2)
  (not (equal? (map-field pos1) (map-field pos2))))

(define (draw-upper-boundary pos)
  (draw-solid-line 
   (canvas-pos-upper-left-corner pos)
   (canvas-pos-upper-right-corner pos)
   *line-color*))

(define (draw-left-boundary pos)
  (draw-solid-line 
   (canvas-pos-upper-left-corner pos)
   (canvas-pos-lower-left-corner pos)
   *line-color*))

(define (draw-contour-map)
  (draw-the-frame)
  (for-each-row-col-do 
   (lambda (row col)
     (let ((pos-this-pixel (make-map-coord row col)))
       (when (> col 1)
         (let ((pos-pixel-left (make-map-coord row (sub1 col))))
           (when (is-boundary? pos-this-pixel pos-pixel-left)
             (draw-left-boundary pos-this-pixel))))
       (when (> row 1)
         (let ((pos-pixel-up (make-map-coord (sub1 row) col)))
           (when (is-boundary? pos-this-pixel pos-pixel-up)
             (draw-upper-boundary pos-this-pixel))))))))

;;; =========================================================================
;;; region operations
;;; =========================================================================

(define (region-list)
  (let ((the-regions '()))
    (for-each-coord-do
     (lambda (coord)
       (let ((the-region-code (map-field coord)))
         (when (not (member the-region-code the-regions))
           (set! the-regions (cons the-region-code the-regions))))))
    the-regions))

(define (region-pos reg)
  (let ((size 0)
        (row-sum 0)
        (col-sum 0))
    (for-each-row-col-do
     (lambda (r c)
       (let ((code (map-field (make-map-coord r c))))
         (when (equal? code reg)
           (set! size (add1 size))
           (set! row-sum (+ row-sum r))
           (set! col-sum (+ col-sum c))))))
    (make-map-coord 
     (quotient row-sum size)
     (quotient col-sum size))))

(define (region-nodes)
  (let* ((regions (region-list))
         (region-poss (map region-pos regions)))
    (map make-node regions region-poss)))

(define (neighbors region)
  ; traverse the pixmap and construct a list of all neighbors of the region
  (let* ((the-neighbors '())
         (cols (map-cols *the-current-pixmap*))
         (rows (map-rows *the-current-pixmap*))
         (in-region? 
          (lambda (r c)
            (let ((the-pix (map-field (make-map-coord r c))))
              (equal? region the-pix))))
         (is-neighbor? 
          (lambda (r c) 
            (if (not (in-region? r c))
                (map-field (make-map-coord r c)); return the neighbor if found
                #f))))
    (for-each-row-col-do
     (lambda (r c)
       (when (and (< 1 r rows) (< 1 c cols) (in-region? r c))
         (let ((neighbor-up (is-neighbor? (sub1 r) c))
               (neighbor-down (is-neighbor? (add1 r) c))
               (neighbor-left (is-neighbor? r (sub1 c)))
               (neighbor-right (is-neighbor? r (add1 c)))
               (check (lambda (neighbor) ; check if new 
                        (when (and neighbor 
                                   (not (member neighbor the-neighbors)))
                          (set! the-neighbors 
                                (cons neighbor the-neighbors))))))
           (check neighbor-up) ; the pixel one step up
           (check neighbor-left) ; the pixel one step left
           (check neighbor-down) ; the pixel one step down
           (check neighbor-right) ; the pixel one step right 
           ))))
    the-neighbors))

(define (neighbor-list regions)
  ; an assoc list to retrieve the neighbors of a region 
  (map cons 
       regions 
       (map neighbors regions)))

(define (neighbors-of region neighbor-list)
  (let ((entry (assoc region neighbor-list)))
    (if entry (cdr entry) '())))

(define (neighbor-list->edge-list neighbor-list node-list)
  (let* ((region-codes (map car neighbor-list))
         (neighbor-codes (map cdr neighbor-list))
         (code->node (lambda (code)
                       (retrieve-node code node-list)))
         (edgelist 
          (apply append
                 (map (lambda (region its-neighbors)
                        (let ((region-node (code->node region))
                              (neighbor-nodes (map code->node its-neighbors)))
                          (map (curry make-edge region-node) 
                               neighbor-nodes)))
                      region-codes neighbor-codes))))
    edgelist))

(define (pixmap->connectivity-graph pixmap)
  (let* ((the-region-nodes (region-nodes))
         (the-neighbor-list 
          (neighbor-list (map node-info the-region-nodes)))
         (edges (neighbor-list->edge-list the-neighbor-list the-region-nodes))
         (the-graph (make-graph the-region-nodes edges)))
    the-graph))
;;; =========================================================================
;;; connection graph
;;; =========================================================================

(define (make-node info pos)
  (vector info pos))

(define (node-pos node) (vector-ref node 1))
(define (node-info node) (vector-ref node 0))

(define (retrieve-node info node-list)
  (let ((the-nodes  (filter (lambda (node)
                              (equal? info (node-info node)))
                            node-list)))
    (if (not (null? the-nodes)) (car the-nodes) #f)))

(define (display-node node)
  (display "node: ")
  (display (node-info node))
  (display-position (node-pos node)) (display " "))

(define (make-graph
         the-nodes ; a list of nodes
         the-edges ; a list of tupels (dotted pairs) of references to the nodes 
         )
  (list the-nodes the-edges))

(define (graph-edges graph) (cadr graph))

(define (graph-nodes graph) (car graph))

(define (make-edge node1 node2) (vector node1 node2))
(define (node1 edge) (vector-ref edge 0))
(define (node2 edge) (vector-ref edge 1))

(define (display-edge edge)
  (display #\newline)(display (list? edge))
  (display "Edge from: ") (display-node (node1 edge)) 
  (display "to: ") (display-node (node2 edge)))

(define (draw-edge edge)
  (let ((pos-node1 (canvas-pos-center (node-pos (node1 edge))))
        (pos-node2 (canvas-pos-center (node-pos (node2 edge)))))
    (draw-solid-line pos-node1 pos-node2 *line-color*)))

(define (draw-edges graph)
  (map draw-edge (graph-edges graph)))

(define (big-marker-node info color graph)
  (let* ((node (retrieve-node info (graph-nodes graph)))
         (coord (node-pos node)))
    (draw-big-marker coord color)))

(define (draw-node node color-table)
  ;retrieve the node and draw a big marker
  (let ((color (color-map-look-up-map (node-info node) color-table))
        (coord (node-pos node)))
    (draw-big-marker coord color)))

(define (draw-nodes graph color-table)
  (map (curryr draw-node color-table)
       (graph-nodes graph)))

(define (draw-graph graph color-table)
  (draw-edges graph)
  (draw-nodes graph color-table))



(define *nikolaus*
  (let* ((k1 (make-node #\1 (make-map-coord 1 4)))
         (k2 (make-node #\2 (make-map-coord 4 2)))
         (k3 (make-node #\3 (make-map-coord 4 6)))
         (k4 (make-node #\4 (make-map-coord 7 2)))
         (k5 (make-node #\5 (make-map-coord 7 6)))
         (nodes (list k1 k2 k3 k4 k5))
         (edges (list (make-edge k1 k2)
                      (make-edge k1 k3)
                      (make-edge k2 k3)
                      (make-edge k2 k4)
                      (make-edge k4 k5)
                      (make-edge k5 k2)
                      (make-edge k3 k4)
                      (make-edge k3 k5))))
    (make-graph nodes edges)))

(define *niko-map* (make-color-map
                    (list  #\1 #\2 #\3 #\4 #\5)
                    (list 'red 'green 'blue 'yellow 'black)))

(define (paint-map)
  (for-each-coord-do draw-cell)
  (display-result)
  )

(define (init-drawing 
         pix-map ; the pixmap image to be drawn
         color-map ; the color table to use
         drawing-color ; the drawing color for additional lines 
         background-color)  ; the background color  
  (set-pix-map pix-map)
  (set-pixel-width)
  (set-color-map color-map)

  (set! *line-color* drawing-color)
  (set! *background-color* background-color)
  
  (start (* *pixel-width* (+ (map-cols *the-current-pixmap*) 2))
         (* *pixel-width* (+ (map-rows *the-current-pixmap*) 2))) )

#|
(define (test11)
    (init-drawing 
     *smiley-map* ; the pixmap image to be drawn
     *bw-color-map* ; the color table to use
     'green ; the drawing color for additional lines 
     'white)
    (paint-map)
    (draw-graph *nikolaus* *niko-map*))

  
   (define (test12)
    (init-drawing 
     *house-pix-map* ; the pixmap image to be drawn
     *house-color-map* ; the color table to use
     'black ; the drawing color for additional lines 
     'white)
    (paint-map)
    (let ((thegraph (pixmap->connectivity-graph *house-pix-map*)))
      (draw-graph thegraph *house-color-map*)))
     
   
     
   (define (test13)
    (init-drawing 
     *house-pix-map* ; the pixmap image to be drawn
     *house-color-map* ; the color table to use
     'black ; the drawing color for additional lines 
     'white)
    (paint-map)
     (draw-contour-map)
    (let ((thegraph (pixmap->connectivity-graph *house-pix-map*)))
      (draw-graph thegraph *house-color-map*))) 
|#

