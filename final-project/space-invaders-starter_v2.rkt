;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter_v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; !!!
;; Dołożyć funkcję, która sprawdza czy lokacja rakiety "pokrywa się" z lokacją statku - uwzglednic hit range
;; na zasadzie - wziąć środek rakiety i statku - jeśli są w odległosci -10 do 10 pixeli, znikają oba
;; sprawdzic każdą rakietę z kazdym statkiem



;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define LEFT-EDGE 0)
(define RIGHT-EDGE 300)

(define INVADER-X-SPEED 1)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.67)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ================================
;; Functions

;; Game -> Game
;; Start the world with (main (make-game empty empty (make-tank 100 1)))
;; 
(define (main game)
  (big-bang game                  ; game 
    (on-tick   advance-game)      ; game -> game 
    (to-draw   render)            ; game -> Image
    (on-key    control-tank)))    ; game KeyEvent -> game 


;; Game -> Game
;; move to the next state - change positions of invaders and missiles
;(check-expect (advance-game (make-game empty empty (make-tank 100 1))) (make-game empty empty (make-tank 102 1)))
;(check-expect (advance-game (make-game empty (list (make-missile 100 110)) (make-tank 100 1))) (make-game empty (list (make-missile 100 100)) (make-tank 102 1)))

(define (advance-game game)
  (if (under-screen? (game-invaders game))
      game
      (make-game (move-invaders (generate-invaders (destroy-collided (game-missiles game) (game-invaders game)))) (move-lom (destroy-collided-m (game-missiles game) (game-invaders game))) (move-tank (game-tank game)))))

;; ListOfMissiles ListOfInvaders -> ListOfMissiles - !!! poprawic opis
;; Delete missiles which collided with invaders from list and return list of left missiles
(check-expect (destroy-collided-m
               (list (make-missile 100 100) (make-missile 140 140))
               (list (make-invader 90 110 1) (make-invader 130 130 1)))
              empty)

(check-expect (destroy-collided-m
               (list (make-missile 120 100) (make-missile 150 140))
               (list (make-invader 90 110 1) (make-invader 130 130 1)))
              (list (make-missile 120 100) (make-missile 150 140)))


(define (destroy-collided-m lom loi)
  (cond [(empty? lom) lom]
        [(empty? loi) lom]    
        [else (if (empty? (check-impact-m (first loi) lom))
                  (destroy-collided-m lom (rest loi) )
                  (destroy-collided-m (cleaned-missiles (check-impact-m (first loi) lom) lom) (rest loi)))])) 

;; Missile ListOfInvaders -> Invader
;; Check LOI of invader which match coordinates (+- 10) of given missile, returns it

; (define (check-impact missile loi) empty)

(check-expect (check-impact-m (make-invader 100 120 1) (list (make-missile 100 100) (make-missile 100 120) (make-missile 200 120))) (make-missile 100 120))
(check-expect (check-impact-m (make-invader 100 120 1) empty) empty)
(check-expect (check-impact-m (make-invader 100 120 1) (list (make-missile 100 100) (make-missile 60 120) (make-missile 200 120))) empty)
(check-expect (check-impact-m (make-invader 100 120 1) (list (make-missile 100 100) (make-missile 100 110) (make-missile 100 120))) (make-missile 100 110))

;(define (check-impact missile loi)
;  (cond [(empty? loi) empty]
;        [else (if (close-enough? missile (first loi))
;                  (first loi)
;                  (check-impact missile (rest loi)))]))

(define (check-impact-m invader lom)
  (cond [(empty? lom) empty]
        [else (if (close-enough-m? (first lom) invader)
                  (first lom)
                  (check-impact-m invader (rest lom)))]))

;(define (close-enough-m? invader missile) 
;  (if (and (and (<= (- (missile-x missile) (invader-x invader)) 10) (>= (- (missile-x missile) (invader-x invader)) -10))
;           (and (<= (- (missile-y missile) (invader-y invader)) 10) (>= (- (missile-y missile) (invader-y invader)) -10)))
;      true
;      false)) 


;; Invader ListOfInvaders -> ListOfInvaders
;; Delete given invader from list and returns that list


; (define (cleaned-invaders invader loi) empty)

(check-expect (cleaned-missiles (make-missile 100 100) (list (make-missile 150 150) (make-missile 100 100) (make-missile 107 107)))
              (list (make-missile 150 150) (make-missile 107 107)))
(check-expect (cleaned-missiles (make-missile 100 100) (list (make-missile 105 105))) (list (make-missile 105 105)))
(check-expect (cleaned-missiles (make-missile 100 100) (list (make-missile 100 100))) empty)

(define (cleaned-missiles missile lom)
  (cond [(empty? lom) empty]
        [else (if (equal? missile (first lom))
                   (rest lom)
                   (cons (first lom) (cleaned-missiles missile (rest lom))))]))




;; ListOfInvaders ListOfMissiles -> ListOfInvaders - !!! poprawic opis
;; Delete invaders which collided with missiles, from list and return list of left invaders

;(define (destroy-collided lom loi) empty)

(check-expect (destroy-collided
               (list (make-missile 100 100) (make-missile 140 140))
               (list (make-invader 90 110 1) (make-invader 130 130 1)))
              empty)

(check-expect (destroy-collided
               (list (make-missile 120 100) (make-missile 150 140))
               (list (make-invader 90 110 1) (make-invader 130 130 1)))
              (list (make-invader 90 110 1) (make-invader 130 130 1)))

(check-expect (destroy-collided empty (list (make-invader 90 110 1) (make-invader 130 130 1))) (list (make-invader 90 110 1) (make-invader 130 130 1)))

(define (destroy-collided lom loi)
  (cond [(empty? loi) loi]
        [(empty? lom) loi]
        [else (if (empty? (check-impact (first lom) loi))
                  (destroy-collided (rest lom) loi)
                  (destroy-collided (rest lom) (cleaned-invaders (check-impact (first lom) loi) loi)))]))


;; Missile ListOfInvaders -> Invader
;; Check LOI of invader which match coordinates (+- 10) of given missile, returns it

(check-expect (check-impact (make-missile 100 100) (list (make-invader 100 120 1) (make-invader 100 100 1) (make-invader 200 120 1))) (make-invader 100 100 1))
(check-expect (check-impact (make-missile 100 100) empty) empty)
(check-expect (check-impact (make-missile 100 100) (list (make-invader 100 120 1) (make-invader 120 100 1) (make-invader 200 120 1))) empty)
(check-expect (check-impact (make-missile 100 100) (list (make-invader 100 120 1) (make-invader 100 110 1) (make-invader 100 100 1))) (make-invader 100 110 1))

; (define (check-impact missile loi) empty)

(define (check-impact missile loi)
  (cond [(empty? loi) empty]
        [else (if (close-enough? missile (first loi))
                  (first loi)
                  (check-impact missile (rest loi)))]))

;; Invader ListOfInvaders -> ListOfInvaders
;; Delete given invader from list and returns that list

(check-expect (cleaned-invaders (make-invader 100 100 1) (list (make-invader 105 105 1) (make-invader 100 100 1) (make-invader 107 107 1)))
              (list (make-invader 105 105 1) (make-invader 107 107 1)))
(check-expect (cleaned-invaders (make-invader 100 100 1) (list (make-invader 105 105 1))) (list (make-invader 105 105 1)))
(check-expect (cleaned-invaders (make-invader 100 100 1) (list (make-invader 100 100 1))) empty)


; (define (cleaned-invaders invader loi) empty)
  
(define (cleaned-invaders invader loi)
  (cond [(empty? loi) empty]
        [else (if (equal? invader (first loi))
                   (rest loi)
                   (cons (first loi) (cleaned-invaders invader (rest loi))))]))


;; Missile Invader -> Boolean
;; Compare both and if they are in impact range, return true
(check-expect (close-enough? (make-missile 100 100) (make-invader 100 110 1)) true)
(check-expect (close-enough? (make-missile 100 100) (make-invader 100 100 1)) true)
(check-expect (close-enough? (make-missile 100 100) (make-invader 100 90 1)) true)
(check-expect (close-enough? (make-missile 100 100) (make-invader 90 110 1)) true)
(check-expect (close-enough? (make-missile 100 100) (make-invader 90 111 1)) false)
(check-expect (close-enough? (make-missile 100 100) (make-invader 89 110 1)) false)

               

(define (close-enough? missile invader)  
  (if (and (and (<= (- (missile-x missile) (invader-x invader)) 10) (>= (- (missile-x missile) (invader-x invader)) -10))
           (and (<= (- (missile-y missile) (invader-y invader)) 10) (>= (- (missile-y missile) (invader-y invader)) -10)))
      true
      false))

;; Missile Invader -> Boolean
;; Compare both and if they are in impact range, return true
(check-expect (close-enough-m? (make-missile 100 100) (make-invader 100 110 1)) true)
(check-expect (close-enough-m? (make-missile 100 100) (make-invader 100 100 1)) true)
(check-expect (close-enough-m? (make-missile 100 100) (make-invader 100 90 1)) true)
(check-expect (close-enough-m? (make-missile 100 100) (make-invader 90 110 1)) true)
(check-expect (close-enough-m? (make-missile 100 100) (make-invader 90 111 1)) false)
(check-expect (close-enough-m? (make-missile 100 100) (make-invader 89 110 1)) false)


(define (close-enough-m? missile invader)
  (if (and (and (<= (- (missile-x missile) (invader-x invader)) 10) (>= (- (missile-x missile) (invader-x invader)) -10))
           (and (<= (- (missile-y missile) (invader-y invader)) 10) (>= (- (missile-y missile) (invader-y invader)) -10)))
      true
      false))    


;; ListOfInvaders ListOfMissiles -> ListOfMissiles 
;; simulate missile and invader destroyment

;(check-expect (destroy-missile empty empty) empty)
;(check-expect (destroy-missile empty (make-missile 100 100)) empty)
;(check-expect (destroy-missile (list (make-invader 90 90 1)) (list (make-missile 100 100))) empty)
;(check-expect (destroy-missile (list (make-invader 89 89 1)) (list (make-missile 100 100))) (list (make-missile 100 100)))
;(check-expect (destroy-missile (list (make-invader 110 110 1)) (list (make-missile 100 100))) empty)
;(check-expect (destroy-missile (list (make-invader 111 111 1)) (list (make-missile 100 100))) (list (make-missile 100 100)))

(define (destroy-missile loi lom) empty)



;(define (check-missile missile loi)
;  (define (under-screen? loi)
;  (cond [(empty? loi) empty]
;        [else (if (< 500 (invader-y (first loi)))
;                  true
;                  (under-screen? (rest loi)))]))

;; Missile ListOfInvaders -> ListOfInvaders
;; Delete invader which is in range of missile

(check-expect (destroy-invader empty empty) empty)
;(check-expect (destroy-invader (make-missile 100 100) (list (make-invader 100 100 1))) empty)
;(check-expect (destroy-invader (make-missile 100 100) (list (make-invader 150 100 1) (make-invader 100 100 1) (make-invader 200 100 1))) (list (make-invader 150 100 1) (make-invader 200 100 1)))


;(define (destroy-invader missile loi) empty)


(define (destroy-invader missile loi)
  (cond [(empty? loi) empty]
        [else (if (< 500 (invader-y (first loi)))
                  true
                  (under-screen? (rest loi)))]))



  

;; ListOfInvaders -> Boolean
;; Check if any invader got below the screen, if so, return true

(check-expect (under-screen? empty) false)
(check-expect (under-screen? (list (make-invader 100 490 1))) false)
(check-expect (under-screen? (list (make-invader 100 501 1))) true)
(check-expect (under-screen? (list (make-invader 100 100 1) (make-invader 100 501 1))) true)
(check-expect (under-screen? (list (make-invader 100 100 1) (make-invader 100 301 1))) false)

;(define (under-screen? loi) true)

(define (under-screen? loi)
  (cond [(empty? loi) false]
        [else (if (< 500 (invader-y (first loi)))
                  true
                  (under-screen? (rest loi)))]))



;; ListOfInvaders -> ListOfInvaders
;; Generate new invaders at random x position on top of the screen


;; (check-expect (generate-invaders empty) (list (make-invader (random HEIGHT) -10 -10)))
;; (check-expect (generate-invaders (list (make-invader 200 -10 -10))) (list (make-invader (random HEIGHT) -10 -10) (make-invader 200 -10 -10)))

; (define (generate-invaders loi) empty)



(define (generate-invaders loi)
  (if (> (random 101) 98)
      (cond [(empty? loi) (list (make-invader (random HEIGHT) -10 -1.5))]
            [else
             (append (list (make-invader (random HEIGHT) -10 -1.5)) loi)])
      loi))

;; ListOfInvaders -> ListOfInvaders
;; Advance invader position depending on direction it's moving

(check-expect (move-invaders (list (make-invader 100 100 1.5))) (list (make-invader 101.5 101.67 1.5)))
(check-expect (move-invaders (list (make-invader 100 100 1.5) (make-invader 101.5 101.5 -1.5))) (list (make-invader 101.5 101.67 1.5) (make-invader 100 103.17 -1.5)))
(check-expect (move-invaders (list (make-invader 299 100 1.5))) (list (make-invader 300 101.67 -1.5)))
(check-expect (move-invaders (list (make-invader 1 100 -1.5))) (list (make-invader 0 101.67 1.5)))
(check-expect (move-invaders empty) empty)
; (define (move-invaders loi) empty)

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cond              
                [(> (+ (invader-x (first loi)) (invader-dx (first loi))) RIGHT-EDGE)
                 (cons (make-invader RIGHT-EDGE (+ INVADER-Y-SPEED (invader-y (first loi))) (- (invader-dx (first loi)))) (move-invaders (rest loi)))]
                [(< (+ (invader-x (first loi)) (invader-dx (first loi))) LEFT-EDGE)
                 (cons (make-invader LEFT-EDGE (+ INVADER-Y-SPEED (invader-y (first loi))) (- (invader-dx (first loi)))) (move-invaders (rest loi))) ]
                [else
                 (cons (make-invader (+ (invader-x (first loi)) (invader-dx (first loi))) (+ INVADER-Y-SPEED (invader-y (first loi))) (invader-dx (first loi))) (move-invaders (rest loi)))])]))

;if (flying-right? (loi first))
;                  (cond [(> (+ (invader-x (loi first)) INVADER-X-SPEED) RIGHT-EDGE) (append (list (make-invader RIGHT-EDGE make-tank RIGHT-EDGE (tank-dir tank))]
;                        [else (make-tank (+ (tank-x tank) TANK-SPEED) (tank-dir tank))])
;                  (cond [(< (- (tank-x tank) TANK-SPEED) LEFT-EDGE) (make-tank LEFT-EDGE (tank-dir tank))]
;                        [else (make-tank (- (tank-x tank) TANK-SPEED) (tank-dir tank))])))

;; Invader -> Boolean
;; Check which way invader is moving
(check-expect (flying-right? (make-invader 100 100 1)) true)
(check-expect (flying-right? (make-invader 100 100 -1)) false)


; (define (flying-right? invader) false) ; stub

(define (flying-right? invader)
  (cond [(> (invader-dx invader) 0) true]
        [(< (invader-dx invader) 0) false]))

;; Tank -> Tank
;; Advance tank position depending on direction it's moving

(check-expect (move-tank (make-tank 100 1)) (make-tank (+ 100 TANK-SPEED) 1))
(check-expect (move-tank (make-tank 300 1)) (make-tank 300 1))
(check-expect (move-tank (make-tank 300 -1)) (make-tank (- 300 TANK-SPEED) -1))
(check-expect (move-tank (make-tank 0 -1)) (make-tank 0 -1))

; (define (move-tank tank) "")


(define (move-tank tank)
  (if (going-right? tank)
      (cond [(> (+ (tank-x tank) TANK-SPEED) RIGHT-EDGE) (make-tank RIGHT-EDGE (tank-dir tank))]
            [else (make-tank (+ (tank-x tank) TANK-SPEED) (tank-dir tank))])
      (cond [(< (- (tank-x tank) TANK-SPEED) LEFT-EDGE) (make-tank LEFT-EDGE (tank-dir tank))]
            [else (make-tank (- (tank-x tank) TANK-SPEED) (tank-dir tank))])))
            
;; ListofMissile -> ListofMissile
;; Advance position of all missiles by missile speed

(check-expect (move-lom empty) empty)
(check-expect (move-lom (list (make-missile 100 100))) (list (make-missile 100 90)))
(check-expect (move-lom (list (make-missile 100 100) (make-missile 200 200))) (list (make-missile 100 90) (make-missile 200 190)))
(check-expect (move-lom (list (make-missile 100 1))) empty)


;(define (move-lom lom) empty)

(define (move-lom lom)
  (cond [(empty? lom) empty]
        [else
         (if (> (- (missile-y (first lom)) MISSILE-SPEED) 0) ;; !!! upewnic sie czy nie psuje to przyjemności z gry
             (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
                   (move-lom (rest lom)))
             (move-lom (rest lom)))]))


;; Tank -> Boolean
;; Check which way tank is moving
(check-expect (going-right? (make-tank 100 1)) true)
(check-expect (going-right? (make-tank 100 -1)) false)


; (define (going-right? tank) false) ; stub

(define (going-right? tank)
  (cond [(> (tank-dir tank) 0) true]
        [(< (tank-dir tank) 0) false]))


;;++++++++++++++++++++++++++++++++++++++
;; RENDER ++++++++++++++++++++++++++++++
;;++++++++++++++++++++++++++++++++++++++

;; Game  -> Image
;; render current game details
(check-expect (render (make-game empty empty (make-tank 100 1))) (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render (make-game empty (list (make-missile 200 200)) (make-tank 100 1))) (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) (place-image MISSILE 200 200 BACKGROUND)))

;(define (render game) empty-image) ; stub
(define (render game)
  (render-tank-missiles (game-tank game) (game-missiles game) (game-invaders game)))


;; Tank ListOfMissiles ListOfMissiles -> Image
;; render tank missiles and invaders
(check-expect (render-tank-missiles (make-tank 100 1) empty empty) (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank-missiles (make-tank 100 1) (list (make-missile 200 200) (make-missile 150 150)) empty) (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) (place-image MISSILE 200 200 (place-image MISSILE 150 150 BACKGROUND))))


;(define (render-tank tank) empty-image) ;stub

(define (render-tank-missiles tank lom loi)
  (place-image TANK (tank-x tank) (- HEIGHT TANK-HEIGHT/2) (render-missiles lom loi)))

;; ListOfMissile -> Image
;; render missiles and invaders on screen

(check-expect (render-missiles empty empty) BACKGROUND)
(check-expect (render-missiles (list (make-missile 100 100)) empty) (place-image MISSILE 100 100 BACKGROUND))
(check-expect (render-missiles (list (make-missile 150 150) (make-missile 100 100)) empty) (place-image MISSILE 150 150 (place-image MISSILE 100 100 BACKGROUND)))

;(define (render-missiles lom) BACKGROUND) ; stub

(define (render-missiles lom loi)
  (cond [(empty? lom) (render-invaders loi)]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))                             
                      (render-missiles (rest lom) loi))]))

;; ListOfInvaders -> Image
;; render invaders

(check-expect (render-invaders empty) BACKGROUND)
(check-expect (render-invaders (list (make-invader 100 100 1.5))) (place-image INVADER 100 100 BACKGROUND))
(check-expect (render-invaders (list (make-invader 100 100 1.5) (make-invader 200 200 1.5))) (place-image INVADER 100 100 (place-image INVADER 200 200 BACKGROUND)))

(define (render-invaders loi)
  (cond [(empty? loi) BACKGROUND]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi))                             
                      (render-invaders (rest loi)))]))


;;++++++++++++++++++++++++++++++++++++++
;; ON-KEY ++++++++++++++++++++++++++++++
;;++++++++++++++++++++++++++++++++++++++


;; Tank KeyPress -> Tank
;; change direction tank is moving - with left and right arrow
(check-expect (control-tank (make-game empty empty (make-tank 50 1)) "right") (make-game empty empty (make-tank 50 1)))
(check-expect (control-tank (make-game empty empty (make-tank 50 1)) "left") (make-game empty empty (make-tank 50 -1)))
(check-expect (control-tank (make-game empty empty (make-tank 50 -1)) "left") (make-game empty empty (make-tank 50 -1)))
(check-expect (control-tank (make-game empty empty (make-tank 50 -1)) "right") (make-game empty empty (make-tank 50 1)))
(check-expect (control-tank (make-game empty empty (make-tank 50 1)) "a") (make-game empty empty (make-tank 50 1)))
(check-expect (control-tank (make-game empty empty (make-tank 50 1)) " ") (make-game empty (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2))) (make-tank 50 1)))


; (define (control-tank tank key) empty-image) ;stub

(define (control-tank game key)
  (cond [(key=? key "right")(make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) 1))]
        [(key=? key "left")(make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) -1))]
        [(key=? key " ") (make-game (game-invaders game) (launch-missile (game-missiles game) (tank-x (game-tank game))) (game-tank game))]
        [else (make-game (game-invaders game) (game-missiles game) (game-tank game))]))

;; liftof Missile -> liftof Missile
;; Add new missile to listof Missile in game

(check-expect (launch-missile empty 100) (list (make-missile 100 (- HEIGHT TANK-HEIGHT/2))))
(check-expect (launch-missile (list (make-missile 100 400)) 100) (list (make-missile 100 (- HEIGHT TANK-HEIGHT/2)) (make-missile 100 400)))

; (define (launch-missile lom tank-x) empty)

(define (launch-missile lom tank-x)
  (append (list (make-missile tank-x (- HEIGHT TANK-HEIGHT/2))) lom))
