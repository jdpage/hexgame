;; Chicken Scheme board pathing tools
;; Copyright (C) 2018  Jonathan David Page <jonathan@sleepingcyb.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(declare (unit pathtools))
(declare (uses hex tictoc costheap))

(module pathtools
    (my-blue-board
     my-unblue-tile
     left-edge-tiles
     right-edge-tiles
     left-edge?
     min/key
     min-list/key
     max-list/key
     score-blue-paths
     shortest-blue-paths
     path-cost
     shortest-blue-path-cost
     lmin
     lmax
     xest-blue-path-cost
     )
  (import scheme chicken hex tictoc costheap)
  (use srfi-1 srfi-4 data-structures)

  (define-type hexboard (struct hexboard))
  (define-type hextile (struct hextile))
  (define-type hexcolor (or symbol false))

  (profiled

   (: my-blue-board (symbol hexboard --> hexboard))
   (define (my-blue-board my-color board)
     (if (eq? my-color 'blue)
         board
         (hexboard-flip board)))

   (: my-unblue-tile (symbol hextile --> hextile))
   (define (my-unblue-tile my-color tile)
     (if (eq? my-color 'blue)
         tile
         (hextile-flip tile)))

   (: left-edge-tiles (hexboard --> (list-of hextile)))
   (define (left-edge-tiles board)
     (map! (lambda (n) (hexboard-tile-ref board n 0))
           (iota (hexboard-size board))))

   (: right-edge-tiles (hexboard --> (list-of hextile)))
   (define (right-edge-tiles board)
     (let ((sz (hexboard-size board)))
       (map! (lambda (n) (hexboard-tile-ref board n (sub1 sz))) (iota sz))))

   (: left-edge? (hextile --> boolean))
   (define (left-edge? tile)
     (receive (row col) (hextile-coords tile) (zero? col)))

   (: blue-distance/c ((or symbol false) --> float))
   (define-inline (blue-distance/c color)
     (case color
       ((blue) 0.0)
       ((red) +inf.0)
       (else 1.0)))

   (: blue-distance (hextile --> float))
   (define (blue-distance t)
     (blue-distance/c (hextile-color-ref t)))

   (: min/key (forall (t) ((t -> float) (list-of t) -> t)))
   (define (min/key key lst)
     (reduce (lambda (a b) (if (< (key b) (key a)) b a)) #f lst))

   (: min-list/key (forall (t) ((t -> float) (list-of t) -> (list-of t))))
   (define (min-list/key key lst)
     (fold
      (lambda (e r)
        (cond ((or (null? r) (= (key (car r)) (key e))) (cons e r))
              ((< (key e) (key (car r))) (list e))
              (else r)))
      '() lst))

   (: max-list/key (forall (t) ((t -> float) (list-of t) -> (list-of t))))
   (define (max-list/key key lst)
     (min-list/key (compose - key) lst))

   ;; Returns a vector of path scores for the blue player
   (: score-blue-paths (hexboard --> f64vector))
   (define (score-blue-paths board)
     (let ((scores (make-f64vector (hexboard-index-count board) +inf.0))
           (queue (make-costheap (hexboard-index-count board) +inf.0)))

       (for-each (lambda (t)
                   (costheap-discount! queue (hextile-index t) (blue-distance t)))
                 (left-edge-tiles board))

       (let next ()
         (if (not (costheap-empty? queue))
             (begin
               (let-values (((k v) (costheap-min! queue)))
                 (f64vector-set! scores k v)
                 (for-each (lambda (n)
                             (let ((color (hexboard-color-ref/i board n)))
                               (costheap-discount! queue n (+ v (blue-distance/c color)))))
                           (hexboard-neighbors/i board k))
                 (next)))))

       scores))

   ;; Returns a list of the shortest paths through the board for the blue player
   (: shortest-blue-paths (hexboard f64vector --> (list-of hextile)))
   (define (shortest-blue-paths board scores)
     (profiled

      (: score-ref (hextile --> float))
      (define (score-ref t)
        (f64vector-ref scores (hextile-index t)))

      (: best-scores! ((list-of hextile) -> (list-of hextile)))
      (define (best-scores! ts)
        (map! car
              (filter!
               (lambda (t) (< (cdr t) +inf.0))
               (min-list/key cdr (map! (lambda (t) (cons t (score-ref t))) ts))))))

     (let ((done '()))
       (let next ((paths (map! list (best-scores! (right-edge-tiles board)))))
         (if (null? paths)
             done
             (next
              (append-map!
               (lambda (p)
                 (if (left-edge? (car p))
                     (begin
                       (set! done (cons p done))
                       '())
                     (map! (cut cons <> p)
                           (lset-difference! hextile-equal?
                                             (best-scores!
                                              (hextile-neighbors (car p)))
                                             p))))
               paths))))))

   (: path-cost (f64vector (list-of hextile) --> float))
   (define (path-cost scores path)
     (f64vector-ref scores (hextile-index (last path))))

   (: shortest-blue-path-cost (hexboard --> float))
   (define (shortest-blue-path-cost board)
     (let ((scores (score-blue-paths board)))
       (apply min (map (lambda (t) (f64vector-ref scores (hextile-index t)))
                       (right-edge-tiles board)))))

   (: lmin ((list-of float) --> float))
   (define (lmin lst)
     (reduce min +inf.0 lst))

   (: lmax ((list-of float) --> float))
   (define (lmax lst)
     (reduce max 0 lst))

   (: xest-blue-path-cost (((list-of float) -> float) hexboard -> float))
   (define (xest-blue-path-cost sel board)
     (let ((scores (score-blue-paths board)))
       (sel
        (filter! (compose not (cut = +inf.0 <>))
                 (map! (lambda (t) (f64vector-ref scores (hextile-index t)))
                       (right-edge-tiles board))))))
   ))
