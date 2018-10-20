;; Path-cost-oriented heap implementation
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

(declare (unit costheap))
(declare (uses tictoc))
(declare (usual-integrations))

(module costheap
    (make-costheap
     costheap?
     costheap-size
     costheap-empty?
     costheap-capacity
     costheap-ref
     costheap-insert!
     costheap-min
     costheap-min!
     costheap-discount!
     costheap-bench
     )
  (import scheme chicken tictoc)
  (use srfi-1 srfi-4 extras)

  ;; A costheap provides a min-priority queue for node costs. Nodes are referred
  ;; to by number, and the capacity of the heap must be one larger than the
  ;; largest node number. Internally, it uses three parallel vectors. The costs
  ;; vector is heap-ordered, while the permutation array maps from node numbers
  ;; to heap positions. We also maintain the inverse of the permutation array,
  ;; for mapping back from heap positions to node numbers.
  (define-type costheap (struct costheap))
  (: make-costheap* (fixnum fixnum f64vector s32vector s32vector -> costheap))
  (: costheap? (* --> boolean : costheap))
  (: costheap-size (costheap --> fixnum))
  (: costheap-capacity (costheap --> fixnum))
  (: costheap-costs (costheap --> f64vector))
  (: costheap-permutation (costheap --> s32vector))
  (: costheap-inverse (costheap --> s32vector))
  (define-record-type costheap
    (make-costheap* size capacity costs permutation inverse)
    costheap?
    (size costheap-size costheap-size-set!)
    (capacity costheap-capacity)
    (costs costheap-costs)
    (permutation costheap-permutation)
    (inverse costheap-inverse))

  ;; Creates a costheap with a given capacity. If no fill value is given, then
  ;; the heap will be of zero size. If a fill value is given, then the heap will
  ;; be sized to full capacity, using the given fill value for every node.
  (: make-costheap (fixnum #!optional float --> costheap))
  (define (make-costheap capacity . optfill)
    (make-costheap*
     (if (null? optfill) 0 capacity) capacity
     (make-f64vector capacity (if (null? optfill) +inf.0 (car optfill)))
     ;; The permutations are both initialized to the identity permutation. This
     ;; ensures that we can use the costheap-xchg! operation to safely increase
     ;; the size.
     (list->s32vector (iota capacity))
     (list->s32vector (iota capacity))))

  ;; Tests if the costheap is empty
  (: costheap-empty? (costheap --> boolean))
  (define (costheap-empty? ch)
    (zero? (costheap-size ch)))

  ;; Inlined version of costheap-ref.
  (: costheap-ref* (costheap fixnum --> float))
  (define-inline (costheap-ref* ch index)
    (f64vector-ref
     (costheap-costs ch)
     (s32vector-ref (costheap-permutation ch) index)))

  ;; Gets the cost of a given node by index.
  (: costheap-ref (costheap fixnum --> float))
  (define (costheap-ref ch index)
    (costheap-ref* ch index))

  ;; Sets the cost of a given node by index. This may leave the heap in an
  ;; inconsistent state. Returns the heap position of the node.
  (: costheap-set! (costheap fixnum float -> fixnum))
  (define-inline (costheap-set! ch index cost)
    (let ((hp (s32vector-ref (costheap-permutation ch) index)))
      (f64vector-set! (costheap-costs ch) hp cost)
      hp))

  ;; Exchanges the position of two nodes by heap position. This may leave the
  ;; heap in an inconsistent state. Returns its last argument.
  (: costheap-xchg!/heap (costheap fixnum fixnum -> fixnum))
  (define-inline (costheap-xchg!/heap ch lp rp)
    ;; Swapping a node with itself is a no-op.
    (if (not (= lp rp))
        (let* ((ps (costheap-permutation ch))
               (is (costheap-inverse ch))
               (cs (costheap-costs ch))
               (left (s32vector-ref is lp))
               (right (s32vector-ref is rp)))

          ;; Swap the costs...
          (let ((t (f64vector-ref cs lp)))
            (f64vector-set! cs lp (f64vector-ref cs rp))
            (f64vector-set! cs rp t))

          ;; ...and swap the permutations so that they point to the correct
          ;; costs. For the inverse, we note that if value A is at position B in
          ;; the original, then value B must be at position A in the inverse.
          (s32vector-set! ps left rp)
          (s32vector-set! ps right lp)
          (s32vector-set! is rp left)
          (s32vector-set! is lp right)))
    rp)

  ;; Appends the given node with the given cost to the heap. This may leave the
  ;; heap in an inconsistent state. Additionally, the given index must not be
  ;; already in the heap, otherwise the heap will become corrupted. Returns the
  ;; heap position of the appended node.
  (: costheap-append! (costheap fixnum float -> fixnum))
  (define (costheap-append! ch index cost)
    ;; check for overflow
    (if (>= (costheap-size ch) (costheap-capacity ch))
        (abort '(exn bounds)))

    ;; Unused nodes always point into unused space, so we don't have to worry
    ;; about overwriting something else. Then all we have to do is exchange the
    ;; new node with whatever was previously right at the end, and extend the
    ;; size.
    (let ((sz (costheap-size ch)))
      (costheap-size-set! ch (add1 sz))
      (costheap-xchg!/heap ch (costheap-set! ch index cost) sz)))

  ;; Up-heap operation indexed by heap position. Starting from the given
  ;; position, exchange values upwards until the parent is less than the
  ;; current, or we reach the top.
  (: costheap-up!/heap (costheap fixnum ->))
  (define (costheap-up!/heap ch hp)
    (let ((cs (costheap-costs ch)))
      (let next ((hp hp))
        (if (and (> hp 0)
                 (< (f64vector-ref cs hp) (f64vector-ref cs (fx/ (sub1 hp) 2))))
            (next (costheap-xchg!/heap ch hp (fx/ (sub1 hp) 2)))))))

  ;; Down-heap operation indexed by heap position. Starting from the given
  ;; position, exchange values downwards until current is less than both
  ;; children, or we reach the bottom.
  (: costheap-down!/heap (costheap fixnum ->))
  (define (costheap-down!/heap ch hp)
    (let ((cs (costheap-costs ch))
          (sz (costheap-size ch)))
      (let next ((hp hp))
        (let* ((lp (add1 (* hp 2)))
               (rp (add1 lp))
               (p hp))

          ;; we want to make sure we exchange with the smaller of the two, to
          ;; preserve the heap property.
          (if (and (< lp sz)
                   (< (f64vector-ref cs lp) (f64vector-ref cs p)))
              (set! p lp))
          (if (and (< rp sz)
                   (< (f64vector-ref cs rp) (f64vector-ref cs p)))
              (set! p rp))
          (if (not (= p hp))
              (next (costheap-xchg!/heap ch hp p)))))))

  ;; Inserts one or more index-cost pairs into the heap.
  (: costheap-insert! (costheap (list-of (pair fixnum float)) ->))
  (define (costheap-insert! ch data)
    (for-each (lambda (p)
                (costheap-up!/heap ch (costheap-append! ch (car p) (cdr p))))
              data))

  ;; Gets the minimum value from the heap without removing it.
  (: costheap-min (costheap --> fixnum float))
  (define (costheap-min ch)
    (if (costheap-empty? ch)
        (abort '(exn bounds)))
    (values
     (s32vector-ref (costheap-inverse ch) 0)
     (f64vector-ref (costheap-costs ch) 0)))

  ;; Gets the minimum value from the heap wile removing it.
  (: costheap-min! (costheap -> fixnum float))
  (define (costheap-min! ch)
    (receive (index cost) (costheap-min ch)
      (let ((sz (sub1 (costheap-size ch))))
        (costheap-size-set! ch sz)
        (costheap-xchg!/heap ch 0 sz)
        (costheap-down!/heap ch 0))
      (values index cost)))

  ;; Lowers the cost of the node by index. If the given cost is greater than or
  ;; equal to the current cost, this is a no-op. Returns the new cost.
  (: costheap-discount! (costheap fixnum float -> float))
  (define (costheap-discount! ch index cost)
    (let ((oldcost (costheap-ref* ch index)))
      (if (< cost oldcost)
          (begin
            (costheap-up!/heap ch (costheap-set! ch index cost))
            cost)
          oldcost)))

  ;; Performs a benchmark operation of the given size against a reference
  ;; implementation, and returns the costheap runtime divided by the reference
  ;; runtime.
  (: costheap-bench (fixnum --> float))
  (define (costheap-bench size)
    ;; generate a set of index/weight pairs to use
    (define data (map! (lambda (k) (cons k (exact->inexact (random size)))) (iota size)))

    ;; reference implementation
    (define reftimer (tic))
    (let ((scores (make-vector size +inf.0))
          (queue '()))

      ;; build queue
      (for-each (lambda (p)
                  (vector-set! scores (car p) (cdr p))
                  (set! queue (cons (car p) queue))) data)

      ;; drain queue
      (let next ((remaining queue))
        (if (not (null? remaining))
            (let ((k (reduce (lambda (a b)
                               (if (< (vector-ref scores a) (vector-ref scores b)) a b))
                             #f remaining)))
              (next (delete! k remaining))))))
    (define refelapsed (toc reftimer))

    ;; our implementation
    (define mytimer (tic))
    (let ((queue (make-costheap size +inf.0)))

      ;; build queue
      (for-each (lambda (p) (costheap-discount! queue (car p) (cdr p))) data)

      ;; drain queue
      (let next ()
        (if (not (costheap-empty? queue))
            (begin
              (costheap-min! queue)
              (next)))))
    (define myelapsed (toc mytimer))

    (/ myelapsed refelapsed))

  )
