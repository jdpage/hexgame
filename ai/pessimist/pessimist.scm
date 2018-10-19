;; "pessimist" Hex AI
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

(declare (uses hex tictoc pathtools))
;; (use srfi-1 data-structures)
(import hex tictoc pathtools)

(define ((ai-move size color) board)
  (if (hexboard-empty? board)
      ;; if the board is empty, just play near the middle. If we don't
      ;; special-case this, every single square is up for consideration, which
      ;; takes a while on larger boards, all to just pick a random square, which
      ;; might be near the edge. Note that some tiles will be in the selection
      ;; pool repeatedly (on a large enough board, center tile 7 times, inner
      ;; ring 3 times each, outer ring 1 or 2 times each). This is intended, for
      ;; weighting.
      (let* ((m (inexact->exact (floor (/ size 2))))
             (s1 (list (hexboard-tile-ref board m m)))
             (s2 (append-map! hextile-neighbors s1))
             (s3 (append-map! hextile-neighbors s2)))
        (hextile-coords (choose (append! s1 s2 s3))))

      ;; for simplicity, all the pathing code calculates paths for the blue
      ;; player. Therefore we need a representation of the board as if we were
      ;; playing blue, and one as if they were playing blue.
      (let* ((my-board (my-blue-board color board))
             (their-board (hexboard-flip my-board))
             (bb (best-blue-candidates my-board))
             (wb (map hextile-flip (worst-red-candidates their-board)))
             (cb (lset-intersection hextile-equal? bb wb)))
        (hextile-coords
         (my-unblue-tile color
                         (choose (find (compose not null?)
                                       (list cb wb bb))))))))

(profiled

 (define (choose lst)
   (list-ref lst (random (length lst))))

 (define (fast-tile-union board lsts)
   (let ((t (make-hash-table)))
     (for-each
      (lambda (lst)
        (for-each
         (lambda (tile)
           (hash-table-set! t (hextile-index tile) #t))
         lst))
      lsts)
     (map (cut make-hextile board <>) (hash-table-keys t))))

 (define (blue-candidates board)
   (filter!
    (lambda (t) (not (hextile-color-ref t)))
    (hexboard-tiles board)))

 ;; return a list of places for blue to play which minimise its maximum path
 ;; costs.
 (define (best-blue-candidates board)
   (map car
        (min-list/key cdr
                      (map
                       (cut hexboard-with-tile board <> 'blue
                            (lambda (b t)
                              (cons
                               (hextile-correlate t board)
                               (xest-blue-path-cost lmax b))))
                       (blue-candidates board)))))

 ;; return a list of places red should play to maximise blue's minimum path
 ;; costs
 (define (worst-red-candidates board)
   (map car
        (max-list/key cdr
                      (map
                       (cut hexboard-with-tile board <> 'red
                            (lambda (b t)
                              (cons
                               (hextile-correlate t board)
                               (xest-blue-path-cost lmin b))))
                       (blue-candidates board))))))


;; Interface to hexmon

#>
#include <hex/hexmon.h>

C_word init_callback(int size, char color);
void move_callback(C_word data, char *board, int *row, int *col);
void destroy_callback(void);

void move_callback_w(void *move, char *board, int *row, int *col)
{
  move_callback(CHICKEN_gc_root_ref(move), board, row, col);
}

void destroy_callback_w(void *move, char *board)
{
  CHICKEN_delete_gc_root(move);
  destroy_callback();
}

void pessimist_ai_init(hex_host_info *host, hex_ai_info *ai)
{
  C_word heap, stack, symbols;
  CHICKEN_parse_command_line(host->optc, host->optv, &heap, &stack, &symbols);
  if (!CHICKEN_initialize(heap, stack, symbols, C_toplevel)) { exit(1); }
  ai->data = CHICKEN_new_gc_root();
  ai->move_callback = &move_callback_w;
  ai->destroy_callback = &destroy_callback_w;
  CHICKEN_run(NULL);
  CHICKEN_gc_root_set(ai->data, init_callback(host->size, host->color));
}
<#

(define-external (init_callback (int size) (char color)) scheme-object
  (ai-move size (if (eqv? color #\b) 'blue 'red)))

(define-external (move_callback
                  (scheme-object move-callback)
                  (c-string board)
                  ((c-pointer int) row)
                  ((c-pointer int) col))
  void

  (receive (r c) (move-callback (string->hexboard board))
    (pointer-s32-set! row r)
    (pointer-s32-set! col c)
    (profile-collect)))

(define-external (destroy_callback) void
  (profile-collect)
  (for-each (lambda (r)
              (display r (current-error-port))
              (newline (current-error-port)))
            (profile-format)))

(return-to-host)

