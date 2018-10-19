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

(module hex
    (make-hexboard
     hexboard?
     make-hextile
     hextile?
     hextile-board
     hextile-index
     string->hexboard
     hexboard-size
     hexboard-index-count
     hexboard->string
     hexboard-clear
     hexboard-empty?
     hexboard-dup
     hexboard-flip
     hexboard-tile-ref
     hexboard-tiles
     hexboard-with-tile
     hextile-correlate
     hextile-color-ref
     hextile-color-set!
     hextile-coords
     hextile-flip
     hextile-equal?
     hextile-neighbors
     )
  (import scheme chicken foreign)
  (use lolevel data-structures srfi-1 srfi-4)

  (foreign-declare "#include <hex/hex.h>")

  (define-type hexboard (struct hexboard))
  (: make-hexboard/s (blob -> hexboard))
  (: hexboard? (* -> boolean : hexboard))
  (: hexboard-buffer (hexboard -> blob))
  (define-record-type hexboard
    (make-hexboard/s buffer)
    hexboard?
    (buffer hexboard-buffer))
  (define-foreign-type hexboard scheme-pointer hexboard-buffer)
  (define-record-printer (hexboard b out)
    (fprintf out "#,(hexboard ~s)" (hexboard->string b)))

  (define-type hextile (struct hextile))
  (: make-hextile (hexboard fixnum -> hextile))
  (: hextile? (* -> boolean : hextile))
  (: hextile-board (hextile -> hexboard))
  (: hextile-index (hextile -> fixnum))
  (define-record-type hextile
    (make-hextile board index)
    hextile?
    (board hextile-board)
    (index hextile-index))
  (define-record-printer (hextile t out)
    (fprintf out "#,(hextile ~s ~s)"
             (hextile-board t)
             (hextile-index t)))

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

  (: hexboard-sizeof (fixnum -> fixnum))
  (define hexboard-sizeof
    (foreign-lambda size_t "hex_board_sizeof" int))

  (: make-hexboard (fixnum -> hexboard))
  (define (make-hexboard size)
    (let ((b (make-hexboard/s (make-blob (hexboard-sizeof size)))))
      ((foreign-lambda void "hex_board_initat" hexboard int)
       b size)
      b))

  (: string->hexboard (string -> (or hexboard false)))
  (define (string->hexboard data)
    (let ((b (make-hexboard
              (inexact->exact (truncate
                               (sqrt (string-length data)))))))
      (hexboard-scan b data)))

  (: hexboard-size (hexboard -> fixnum))
  (define hexboard-size
    (foreign-lambda int "hex_board_size" hexboard))

  (: hexboard-index-count (hexboard -> fixnum))
  (define hexboard-index-count
    (foreign-lambda int "hex_board_index_count" hexboard))

  (: hexboard->string (hexboard -> string))
  (define (hexboard->string board)
    (let* ((sz ((foreign-lambda size_t "hex_board_dumpsize" hexboard) board))
           (str (make-string (sub1 sz))))
      ((foreign-lambda int "hex_board_dump" hexboard scheme-pointer size_t)
       board str sz)
      str))

  (: hexboard-scan (hexboard string -> (or hexboard false)))
  (define (hexboard-scan board data)
    (let ((err ((foreign-lambda (enum hex_err_e) "hex_board_scan" hexboard c-string)
                board data)))
      (cond ((eqv? err HEX_OK) board)
            ((eqv? err HEX_ESIZEMISMATCH) #f))))

  (: hexboard-clear (hexboard -> undefined))
  (define hexboard-clear
    (foreign-lambda void "hex_board_clear" hexboard))

  (: hexboard-empty? (hexboard -> boolean))
  (define hexboard-empty?
    (foreign-lambda bool "hex_board_is_empty" hexboard))

  (: hexboard-copy (hexboard hexboard -> (or hexboard false)))
  (define (hexboard-copy dest src)
    (let ((err ((foreign-lambda (enum hex_err_e) "hex_board_copy" hexboard hexboard)
                dest src)))
      (select err
              ((HEX_OK) dest)
              ((HEX_ESIZEMISMATCH) #f)
              (else (abort 'exn)))))

  (: hexboard-dup (hexboard -> hexboard))
  (define (hexboard-dup board)
    (hexboard-copy (make-hexboard (hexboard-size board))
                   board))

  (: hexboard-flip (hexboard -> hexboard))
  (define (hexboard-flip board)
    (let ((b (make-hexboard (hexboard-size board))))
      ((foreign-lambda (enum hex_err_e) "hex_board_flipcopy" hexboard hexboard)
       b board)
      b))

  (: hexcolor->object (fixnum -> (or symbol false)))
  (define (hexcolor->object color)
    (select color
            ((HEX_COLOR_RED) 'red)
            ((HEX_COLOR_BLUE) 'blue)
            (else #f)))

  (: object->hexcolor ((or symbol false) -> fixnum))
  (define (object->hexcolor color)
    (case color
      ((#f) HEX_COLOR_NONE)
      ((red) HEX_COLOR_RED)
      ((blue) HEX_COLOR_BLUE)))

  (: hexboard-tile-ref (hexboard fixnum fixnum -> hextile))
  (define (hexboard-tile-ref board r c)
    (let-location
     ((index int))
     (let ((err ((foreign-lambda (enum hex_err_e) "hex_board_index"
                                 hexboard int int (c-pointer int))
                 board r c (location index))))
       (select err
               ((HEX_EBOUNDS) (abort '(exn bounds)))
               ((HEX_OK) (make-hextile board index))))))

  (: hexboard-tiles (hexboard -> (list-of hextile)))
  (define (hexboard-tiles board)
    (map (cut make-hextile board <>) (iota (hexboard-index-count board))))

  (: hexboard-with-tile (forall (t) (hexboard hextile (or symbol false) (hexboard hextile -> t) -> t)))
  (define (hexboard-with-tile board tile color cont)
    (let* ((b (hexboard-dup board))
           (t (hextile-correlate tile b)))
      (set! (hextile-color-ref t) color)
      (cont b t)))

  (: hextile-correlate (hextile hexboard -> hextile))
  (define (hextile-correlate tile board)
    (make-hextile board (hextile-index tile)))

  (: hextile-color-ref (hextile -> (or symbol false)))
  (define (hextile-color-ref tile)
    (let-location
     ((color int))
     (let ((err ((foreign-lambda*
                  (enum hex_err_e) ((hexboard board)
                                    (int index)
                                    ((c-pointer int) ptr))
                  "hex_tile *tile;"
                  "hex_err e = hex_board_itile(board, index, &tile);"
                  "if (e == HEX_OK) *ptr = *hex_tile_color(tile);"
                  "C_return(e);")
                 (hextile-board tile)
                 (hextile-index tile)
                 (location color))))
       (select err
               ((HEX_EBOUNDS) (abort '(exn bounds)))
               ((HEX_OK) (hexcolor->object color))
               (else (abort 'exn))))))

  (: hextile-color-set! (hextile (or symbol false) -> undefined))
  (define (hextile-color-set! tile color)
    (let ((err ((foreign-lambda*
                 (enum hex_err_e) ((hexboard board)
                                   (int index)
                                   ((enum hex_color_e) c))
                 "hex_tile *tile;"
                 "hex_err e = hex_board_itile(board, index, &tile);"
                 "if (e == HEX_OK) *hex_tile_color(tile) = c;"
                 "C_return(e);")
                (hextile-board tile)
                (hextile-index tile)
                (object->hexcolor color))))
      (select err
              ((HEX_EBOUNDS) (abort '(exn bounds)))
              ((HEX_OK))
              (else (abort 'exn)))))

  (set! (setter hextile-color-ref) hextile-color-set!)

  (: hextile-coords (hextile -> fixnum fixnum))
  (define (hextile-coords tile)
    (let-location
     ((row int) (col int))
     (let ((err ((foreign-lambda (enum hex_err_e) "hex_board_icoords"
                                 hexboard int (c-pointer int) (c-pointer int))
                 (hextile-board tile) (hextile-index tile)
                 (location row) (location col))))
       (select err
               ((HEX_EBOUNDS) (abort '(exn bounds)))
               ((HEX_OK) (values row col))
               (else (abort 'exn))))))

  (: hextile-flip (hextile -> hextile))
  (define (hextile-flip tile)
    (let-values (((board) (hexboard-flip (hextile-board tile)))
                 ((r c) (hextile-coords tile)))
      (hexboard-tile-ref board c r)))

  (: hextile-equal? (hextile hextile -> boolean))
  (define (hextile-equal? a b)
    (= (hextile-index a) (hextile-index b)))

  (: hextile-neighbors (hextile -> (list-of hextile)))
  (define (hextile-neighbors tile)
    (let-location
     ((count int))
     (let* ((ns (make-s32vector 6))
            (err ((foreign-lambda (enum hex_err_e) "hex_board_ineighbors"
                                  hexboard int s32vector (c-pointer int))
                  (hextile-board tile)
                  (hextile-index tile)
                  ns
                  (location count))))
       (select err
               ((HEX_EBOUNDS) (abort '(exn bounds)))
               ((HEX_OK)
                (map (lambda (index) (make-hextile (hextile-board tile) index))
                     (s32vector->list (subs32vector ns 0 count))))
               (else (abort 'exn))))))
  )
