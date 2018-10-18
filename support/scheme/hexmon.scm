;; Chicken Scheme hexmon stubs generator
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

(declare (unit hexmon))
(declare (uses hex))

(module hexmon
    (hexmon-info?
     hexmon-info-color
     hexmon-info-size
     define-hexmon-entry
     )
  (import scheme chicken foreign lolevel)

  (define-type hexmon-info (struct hexmon-info))
  (: make-hexmon-info (symbol fixnum -> hexmon-info))
  (: hexmon-info? (* -> boolean : hexmon-info))
  (: hexmon-info-color (hexmon-info -> symbol))
  (: hexmon-info-size (hexmon-info -> fixnum))
  (define-record-type hexmon-info
    (make-hexmon-info color size)
    hexmon-info?
    (color hexmon-info-color)
    (size hexmon-info-size))

  (define-syntax define-hexmon-entry
    (syntax-rules ()
      ((define-hexmon-entry name init destroy)
       (begin

         (foreign-declare "#include <hex/hexmon.h>")
         (foreign-declare "C_word init_callback(int size, char color)")

         ))))

  (define-external (move_callback
                    (scheme-object move-callback)
                    (c-string board)
                    ((c-pointer int) row)
                    ((c-pointer int) col))
    void

    (receive (r c) (move-callback (hex/string->board board))
      (pointer-s32-set! row r)
      (pointer-s32-set! col c)))

  (define-external ())

  (define-external (init_callback (int size) (char color)) scheme-object
    )

  )
