(declare (uses hex))
(use srfi-1)

(define ((ai-move size color) board)
  ;; for simplicity, all the pathing code calculates paths for the blue player.
  ;; Therefore we need a representation of the board as if we were playing blue,
  ;; and one as if they were playing blue.
  (let* ((my-board (my-blue-board color board))
         (their-board (hex/board-flip my-board))
         (bb (best-blue-candidates my-board))
         (wb (map hex/tile-flip (worst-red-candidates their-board)))
         (cb (lset-intersection hex/tile-equal? bb wb)))
    (hex/tile-coords
     (my-unblue-tile color
                     (choose (find (compose not null?)
                                   (list cb wb bb)))))))

(define (my-blue-board my-color board)
  (if (eq? my-color 'blue)
      board
      (hex/board-flip board)))

(define (my-unblue-tile my-color tile)
  (if (eq? my-color 'blue)
      tile
      (hex/tile-flip tile)))

(define (left-edge-tiles board)
  (map! (lambda (n) (hex/board-tile board n 0))
        (iota (hex/board-size board))))

(define (right-edge-tiles board)
  (let ((sz (hex/board-size board)))
    (map! (lambda (n) (hex/board-tile board n (sub1 sz))) (iota sz))))

(define (left-edge? tile)
  (receive (row col) (hex/tile-coords tile) (zero? col)))

(define (blue-distance t)
  (case (hex/tile-color-ref t)
    ((blue) 0)
    ((red) +inf.0)
    (else 1)))

(define (min+ key . lst)
  (reduce (lambda (a b) (if (< (key b) (key a)) b a)) #f lst))

(define (mins+ key . lst)
  (fold
   (lambda (e r)
     (cond ((or (null? r) (= (key (car r)) (key e))) (cons e r))
           ((< (key e) (key (car r))) (list e))
           (else r)))
   '() lst))

(define (maxs+ key . lst)
  (apply mins+ (compose - key) lst))

;; Returns a vector of path scores for the blue player
(define (score-blue-paths board)
  (let ((scores (make-vector (hex/board-index-count board) +inf.0)))

    (define (score-ref t)
      (vector-ref scores (hex/tile-index t)))
    (define (update-score! t s)
      (vector-set! scores (hex/tile-index t) (min s (score-ref t))))
    (define (min-score tiles)
      (apply min+ score-ref tiles))

    (for-each (lambda (t)
                (update-score! t (blue-distance t)))
              (left-edge-tiles board))

    (let next ((unvisited (hex/board-tiles board)))
      (if (not (null? unvisited))
          (begin
            (let ((current (min-score unvisited)))
              (for-each (lambda (n)
                          (update-score! n (+ (score-ref current) (blue-distance n))))
                        (hex/tile-neighbors current))
              (next (delete! current unvisited))))
          scores))))

;; Returns a list of the shortest paths through the board for the blue player
(define (shortest-blue-paths board scores)
  (define (score-ref t)
    (vector-ref scores (hex/tile-index t)))

  (define (best-scores ts)
    (filter (lambda (t) (< (score-ref t) +inf.0))
            (apply mins+ score-ref ts)))

  (let next ((paths (map! list (best-scores (right-edge-tiles board)))))
    (if (every left-edge? (map car paths))
        paths
        (next
         (append-map!
          (lambda (p)
            (if (left-edge? (car p))
                (list p)
                (map (cut cons <> p)
                     (lset-difference hex/tile-equal?
                                      (best-scores
                                       (hex/tile-neighbors (car p)))
                      p))))
          paths)))))

(define (path-cost scores path)
  (define (score-ref t)
    (vector-ref scores (hex/tile-index t)))
  (last (map score-ref path)))

(define (shortest-blue-path-cost board)
  (let* ((scores (score-blue-paths board))
         (paths (shortest-blue-paths board scores)))
    (if (null? paths) +inf.0 (path-cost scores (car paths)))))

(define (choose lst)
  (list-ref lst (random (length lst))))

(define (blue-candidates board)
  (filter
   (lambda (t) (not (hex/tile-color-ref t)))
   (apply lset-union hex/tile-equal?
          (shortest-blue-paths board (score-blue-paths board)))))

;; return a list of the best places for blue to play in order to minimise blue's
;; path costs
(define (best-blue-candidates board)
  (map car
       (apply mins+ cdr
              (map
               (cut hex/board-with-tile board <> 'blue
                    (lambda (b t)
                      (cons
                       (hex/tile-correlate t board)
                       (shortest-blue-path-cost b))))
               (blue-candidates board)))))

;; return a list of places red should play to maximise blue's path costs
(define (worst-red-candidates board)
  (map car
       (apply maxs+ cdr
              (map
               (cut hex/board-with-tile board <> 'red
                    (lambda (b t)
                      (cons
                       (hex/tile-correlate t board)
                       (shortest-blue-path-cost b))))
               (blue-candidates board)))))


;; Interface to hexmon
(use lolevel)

#>
#include <hex/hexmon.h>

C_word init_callback(int size, char color);
void move_callback(C_word data, char *board, int *row, int *col);
void destroy_callback(C_word data, char *board);

void move_callback_w(void *data, char *board, int *row, int *col)
{
  move_callback((C_word) data, board, row, col);
}

void destroy_callback_w(void *data, char *board)
{
  destroy_callback((C_word) data, board);
}

void disruptor_ai_init(hex_host_info *host, hex_ai_info *ai)
{
  C_word heap, stack, symbols;
  CHICKEN_parse_command_line(host->optc, host->optv, &heap, &stack, &symbols);
  if (!CHICKEN_initialize(heap, stack, symbols, C_toplevel)) { exit(1); }
  CHICKEN_run(NULL);
  ai->data = (void *) init_callback(host->size, host->color);
  ai->move_callback = &move_callback_w;
  ai->destroy_callback = &destroy_callback_w;
}
<#

(define-external (init_callback (int size) (char color)) scheme-object
  (object-evict (ai-move size (if (eqv? color #\b) 'blue 'red))))

(define-external (move_callback
                  (scheme-object move-callback)
                  (c-string board)
                  ((c-pointer int) row)
                  ((c-pointer int) col))
  void

  (receive (r c) (move-callback (hex/string->board board))
    (pointer-s32-set! row r)
    (pointer-s32-set! col c)))

(return-to-host)

(define-external (destroy_callback
                  (scheme-object move-callback)
                  (c-string board))
  void
  (object-release move-callback))
