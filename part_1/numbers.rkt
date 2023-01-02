;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(+ 3 4)
;(+ 3 (* 2 3))
;(/ 12 (* 2 3))

(sqr 3)
(sqrt 16)

(sqrt (+ (sqr 3) (sqr 4)))

(sqrt 2)
(+ 2 (* 3 4 ) (- (+ 1 2) 3)) ;left to right, call to a primitve to podstawowe dzialanie np. (+ 1 2), gdzie + to operator, a 1 i 2 to operands
(+ 2 12 (- (+ 1 2) 3)) ;insided to outside
(+ 2 12 (- 3 3))
(+ 2 12 0 )
14

