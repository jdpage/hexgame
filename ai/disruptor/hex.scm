;; Chicken Scheme bindings for libhex
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

(declare (unit hex))
(use lolevel data-structures srfi-1 srfi-4)

(foreign-declare "#include <hex/hex.h>")

(define-record hex/board buffer)
(define-foreign-type hex/board scheme-pointer hex/board-buffer)
(define-record-printer (hex/board b out)
  (fprintf out "#,(hex/board ~s)" (hex/board->string b)))

(define-record hex/tile board index)
(define-record-printer (hex/tile t out)
  (fprintf out "#,(hex/tile ~s ~s)"
           (hex/tile-board t)
           (hex/tile-index t)))

(define-syntax define-foreign-enum
  (syntax-rules ()
    ((define-foreign-enum e n)
     (define n (foreign-value n (enum e))))
    ((define-foreign-enum e n p ...)
     (begin
       (define-foreign-enum e n)
       (define-foreign-enum e p ...)))))

(define-foreign-enum hex_err_e
  HEX_OK
  HEX_ENOMEM
  HEX_EBOUNDS
  HEX_ESIZEMISMATCH
  HEX_ESHORTBUFFER)

(define-foreign-enum hex_color_e
  HEX_COLOR_NONE
  HEX_COLOR_RED
  HEX_COLOR_BLUE)

(define hex/board-sizeof
  (foreign-lambda size_t "hex_board_sizeof" int))

(define (hex/make-board size)
  (let ((b (make-hex/board (make-blob (hex/board-sizeof size)))))
    ((foreign-lambda void "hex_board_initat" hex/board int)
     b size)
    b))

(define (hex/string->board data)
  (let ((b (hex/make-board
            (inexact->exact (truncate
                             (sqrt (string-length data)))))))
    (hex/board-scan b data)))

(define hex/board-size
  (foreign-lambda int "hex_board_size" hex/board))

(define hex/board-index-count
  (foreign-lambda int "hex_board_index_count" hex/board))

(define (hex/board->string board)
  (let* ((sz ((foreign-lambda size_t "hex_board_dumpsize" hex/board) board))
         (str (make-string (sub1 sz))))
    ((foreign-lambda int "hex_board_dump" hex/board scheme-pointer size_t)
     board str sz)
    str))

(define (hex/board-scan board data)
  (let ((err ((foreign-lambda (enum hex_err_e) "hex_board_scan" hex/board c-string)
              board data)))
    (cond ((eqv? err HEX_OK) board)
          ((eqv? err HEX_ESIZEMISMATCH) #f))))

(define hex/board-clear
  (foreign-lambda void "hex_board_clear" hex/board))

(define hex/board-empty?
  (foreign-lambda bool "hex_board_is_empty" hex/board))

(define (hex/board-copy dest src)
  (let ((err ((foreign-lambda (enum hex_err_e) "hex_board_copy" hex/board hex/board)
              dest src)))
    (select err
            ((HEX_OK) dest)
            ((HEX_ESIZEMISMATCH) #f)
            (else (abort 'exn)))))

(define (hex/board-dup board)
  (hex/board-copy (hex/make-board (hex/board-size board))
                  board))

(define (hex/board-flip board)
  (let ((b (hex/make-board (hex/board-size board))))
    ((foreign-lambda (enum hex_err_e) "hex_board_flipcopy" hex/board hex/board)
     b board)
    b))

(define (hex/color->object color)
  (select color
          ((HEX_COLOR_RED) 'red)
          ((HEX_COLOR_BLUE) 'blue)
          (else #f)))

(define (hex/object->color color)
  (case color
    ((#f) HEX_COLOR_NONE)
    ((red) HEX_COLOR_RED)
    ((blue) HEX_COLOR_BLUE)))

(define (hex/board-tile board r c)
  (let-location
   ((index int))
   (let ((err ((foreign-lambda (enum hex_err_e) "hex_board_index"
                               hex/board int int (c-pointer int))
               board r c (location index))))
     (select err
             ((HEX_EBOUNDS) (abort '(exn bounds)))
             ((HEX_OK) (make-hex/tile board index))))))

(define (hex/board-tiles board)
  (map (cut make-hex/tile board <>) (iota (hex/board-index-count board))))

(define (hex/board-with-tile board tile color cont)
  (let* ((b (hex/board-dup board))
         (t (hex/tile-correlate tile b)))
    (set! (hex/tile-color-ref t) color)
    (cont b t)))

(define (hex/tile-correlate tile board)
  (make-hex/tile board (hex/tile-index tile)))

(define (hex/tile-color-ref tile)
  (let-location
   ((color int))
   (let ((err ((foreign-lambda*
                (enum hex_err_e) ((hex/board board)
                                  (int index)
                                  ((c-pointer int) ptr))
                "hex_tile *tile;"
                "hex_err e = hex_board_itile(board, index, &tile);"
                "if (e == HEX_OK) *ptr = *hex_tile_color(tile);"
                "C_return(e);")
               (hex/tile-board tile)
               (hex/tile-index tile)
               (location color))))
     (select err
             ((HEX_EBOUNDS) (abort '(exn bounds)))
             ((HEX_OK) (hex/color->object color))
             (else (abort 'exn))))))

(define (hex/tile-color-set! tile color)
  (let ((err ((foreign-lambda*
               (enum hex_err_e) ((hex/board board)
                                 (int index)
                                 ((enum hex_color_e) c))
               "hex_tile *tile;"
               "hex_err e = hex_board_itile(board, index, &tile);"
               "if (e == HEX_OK) *hex_tile_color(tile) = c;"
               "C_return(e);")
              (hex/tile-board tile)
              (hex/tile-index tile)
              (hex/object->color color))))
    (select err
            ((HEX_EBOUNDS) (abort '(exn bounds)))
            ((HEX_OK) (hex/color->object color))
            (else (abort 'exn)))))

(set! (setter hex/tile-color-ref) hex/tile-color-set!)

(define (hex/tile-coords tile)
  (let-location
   ((row int) (col int))
   (let ((err ((foreign-lambda (enum hex_err_e) "hex_board_icoords"
                               hex/board int (c-pointer int) (c-pointer int))
               (hex/tile-board tile) (hex/tile-index tile)
               (location row) (location col))))
     (select err
             ((HEX_EBOUNDS) (abort '(exn bounds)))
             ((HEX_OK) (values row col))
             (else (abort 'exn))))))

(define (hex/tile-flip tile)
  (let-values (((board) (hex/board-flip (hex/tile-board tile)))
               ((r c) (hex/tile-coords tile)))
    (hex/board-tile board c r)))

(define (hex/tile-equal? a b)
  (= (hex/tile-index a) (hex/tile-index b)))

(define (hex/tile-neighbors tile)
  (let-location
   ((count int))
   (let* ((ns (make-s32vector 6))
          (err ((foreign-lambda (enum hex_err_e) "hex_board_ineighbors"
                                hex/board int s32vector (c-pointer int))
                (hex/tile-board tile)
                (hex/tile-index tile)
                ns
                (location count))))
     (select err
             ((HEX_EBOUNDS) (abort '(exn bounds)))
             ((HEX_OK)
              (map (lambda (index) (make-hex/tile (hex/tile-board tile) index))
                   (s32vector->list (subs32vector ns 0 count))))
             (else (abort 'exn))))))
