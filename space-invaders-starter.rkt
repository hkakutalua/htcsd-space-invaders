;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT (image-height TANK))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-WIDTH/2 (/ (image-width MISSILE) 2))
(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank Integer)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles, tank position

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

(define NO-INVADERS empty)
(define NO-MISSILES empty)
(define CENTERED-R-MOVING-TANK (make-tank (/ WIDTH 2) 1))

;; Functions

; Start with (main G1)
(define (main game)
  (big-bang game
    (on-tick   update-game)           ; Game -> Game
    (on-key    update-on-key-pressed) ; Game KeyEvent -> Game
    (to-draw   render-game)           ; Game -> Image
    (stop-when end-game?)))           ; Game -> Boolean

;; Game -> Game
;; Updates the state of the given game
(check-random (update-game (make-game NO-INVADERS
                                      NO-MISSILES
                                      CENTERED-R-MOVING-TANK))
              (make-game (move-invaders (add-invader empty (random (add1 INVADE-RATE)))
                                        NO-MISSILES)
                         NO-MISSILES
                         (move-tank CENTERED-R-MOVING-TANK)))
(check-random (update-game (make-game (list I1)
                                      NO-MISSILES
                                      CENTERED-R-MOVING-TANK))
              (make-game (move-invaders (add-invader (list I1) (random (add1 INVADE-RATE)))
                                        NO-MISSILES)
                         NO-MISSILES
                         (move-tank CENTERED-R-MOVING-TANK)))

;(define (update-game game) game) ; stub

(define (update-game game)
  (make-game (move-invaders (add-invader (game-invaders game)
                                         (random (add1 INVADE-RATE)))
                            (game-missiles game))
             (move-missiles (game-missiles game)
                            (game-invaders game))
             (move-tank     (game-tank game))))

(define INVADER-ON-LWALL (make-invader INVADER-WIDTH/2 100 -12))
(define INVADER-ON-RWALL (make-invader (- WIDTH INVADER-WIDTH/2) 100 12))

;; (listof Invader) (listof Missile) -> (listof Invader)
;; Move the given invaders down the scene, detecting if they hit missiles
(check-expect (move-invaders empty empty) empty)
(check-expect (move-invaders (list (make-invader 150 100 12)
                                   (make-invader 120 134 -7))
                             empty)
              (list (make-invader (+ 150 12) (+ 100 INVADER-Y-SPEED) 12)
                    (make-invader (+ 120 -7) (+ 134 INVADER-Y-SPEED) -7)))
(check-expect (move-invaders (list INVADER-ON-RWALL I1) (list M1))
              (list (make-invader (+ (invader-x INVADER-ON-RWALL) -12)
                                  (+ (invader-y INVADER-ON-RWALL) INVADER-Y-SPEED)
                                  -12)
                    (move-invader I1)))
(check-expect (move-invaders (list INVADER-ON-LWALL I1) (list M1))
              (list (make-invader (+ (invader-x INVADER-ON-LWALL) 12)
                                  (+ (invader-y INVADER-ON-LWALL) INVADER-Y-SPEED)
                                  12)
                    (move-invader I1)))
(check-expect (move-invaders (list I1) (list M1-HITTING-I1-ON-LEFT)) empty)
(check-expect (move-invaders (list I1) (list M1-HITTING-I1-ON-RIGHT)) empty)

;(define (move-invaders invaders) invaders) ; stub

(define (move-invaders invaders missiles)
  (cond [(empty? invaders) empty]
        [else
         (if (invader-hit-missiles? (first invaders) missiles)
             (move-invaders (rest invaders) missiles)
             (cons (move-invader (first invaders))
                   (move-invaders (rest invaders) missiles)))]))

(define I1-LEFT-X-HIT (add1 (- (invader-x I1) INVADER-WIDTH/2)))
(define I1-RIGHT-X-HIT (sub1 (+ (invader-x I1) INVADER-WIDTH/2)))
(define I1-BOTTOM-Y-HIT (sub1 (+ (invader-y I1) INVADER-HEIGHT/2)))

(define M1-HITTING-I1-ON-LEFT (make-missile I1-LEFT-X-HIT I1-BOTTOM-Y-HIT))
(define M1-HITTING-I1-ON-RIGHT (make-missile I1-RIGHT-X-HIT I1-BOTTOM-Y-HIT))

;; Invader (listof Missile) -> Boolean
;; produce true when given invader is hit by a missile
(check-expect (invader-hit-missiles? I1 empty) false)
(check-expect (invader-hit-missiles? (make-invader (/ WIDTH 2) 30 -15)
                                     (list (make-missile (/ WIDTH 2) 90)))
              false)
(check-expect (invader-hit-missiles? (make-invader (/ WIDTH 2) 30 -15)
                                     (list (make-missile (/ WIDTH 2) 90)
                                           (make-missile MISSILE-WIDTH/2 30)))
              false)
(check-expect (invader-hit-missiles? I1 (list M1-HITTING-I1-ON-LEFT)) true)
(check-expect (invader-hit-missiles? I1 (list M1-HITTING-I1-ON-RIGHT)) true)
(check-expect (invader-hit-missiles? I1 (list(make-missile (/ WIDTH 2) 90)
                                             M1-HITTING-I1-ON-LEFT))
              true)
(check-expect (invader-hit-missiles? I1 (list(make-missile (/ WIDTH 2) 90)
                                             M1-HITTING-I1-ON-RIGHT))
              true)

;(define (invader-hit? invader missiles) false) ; stub

(define (invader-hit-missiles? invader missiles)
  (cond [(empty? missiles) false]
        [else
         (if (between-bounds? (first missiles) invader)
             true
             (invader-hit-missiles? invader (rest missiles)))]))

;; Missile Invader -> Boolean
;; produce true when given missile is within the invader bounds
(check-expect (between-bounds? (make-missile (/ WIDTH 2) 90)
                               (make-invader (/ WIDTH 2) 30 -15))
              false)
(check-expect (between-bounds? (make-missile MISSILE-WIDTH/2 30)
                               (make-invader (/ WIDTH 2) 30 -15))
              false)
(check-expect (between-bounds? M1-HITTING-I1-ON-LEFT I1) true)
(check-expect (between-bounds? M1-HITTING-I1-ON-RIGHT I1) true)

;(define (between-invader-bounds? missile invader) false) ; stub

(define (between-bounds? missile invader)
  (and (between-left-right-bounds? missile invader)
       (between-top-right-bounds?  missile invader)))

;; Missile Invader -> Boolean
;; produce true when given missile is between left-right bounds of invader
(check-expect (between-left-right-bounds? (make-missile (- 20 INVADER-WIDTH/2 MISSILE-WIDTH/2) 15)
                                          (make-invader 20 15 1))
              false)
(check-expect (between-left-right-bounds? (make-missile (+ 20 INVADER-WIDTH/2 MISSILE-WIDTH/2) 15)
                                          (make-invader 20 15 1))
              false)
(check-expect (between-left-right-bounds? M1-HITTING-I1-ON-LEFT I1) true)
(check-expect (between-left-right-bounds? M1-HITTING-I1-ON-RIGHT I1) true)
(define (between-left-right-bounds? missile invader)
  (or (and (>  (missile-right-x missile) (invader-left-x  invader))
           (<= (missile-right-x missile) (invader-right-x invader)))
      (and (<  (missile-left-x  missile) (invader-right-x invader))
           (>= (missile-left-x  missile) (invader-left-x  invader)))))

;; Missile -> Integer
;; produce missile left end x position
(check-expect (missile-left-x M1) (- (missile-x M1) MISSILE-WIDTH/2))
(check-expect (missile-left-x M2) (- (missile-x M2) MISSILE-WIDTH/2))
(define (missile-left-x missile)
  (- (missile-x missile) MISSILE-WIDTH/2))

;; Missile -> Integer
;; produce missile right end x position
(check-expect (missile-right-x M1) (+ (missile-x M1) MISSILE-WIDTH/2))
(check-expect (missile-right-x M2) (+ (missile-x M2) MISSILE-WIDTH/2))
(define (missile-right-x missile)
  (+ (missile-x missile) MISSILE-WIDTH/2))

;; Invader -> Integer
;; produce invader left end x position
(check-expect (invader-left-x I1) (- (invader-x I1) INVADER-WIDTH/2))
(check-expect (invader-left-x I2) (- (invader-x I2) INVADER-WIDTH/2))
(define (invader-left-x invader)
  (- (invader-x invader) INVADER-WIDTH/2))

;; Invader -> Integer
;; produce invader right end x position
(check-expect (invader-right-x I1) (+ (invader-x I1) INVADER-WIDTH/2))
(check-expect (invader-right-x I2) (+ (invader-x I2) INVADER-WIDTH/2))
(define (invader-right-x invader)
  (+ (invader-x invader) INVADER-WIDTH/2))

;; Missile Invader -> Boolean
;; produce true when given missile is within the top-bottom bounds of invader
(check-expect (between-top-right-bounds? (make-missile 100 (- 67 INVADER-HEIGHT/2 MISSILE-HEIGHT/2))
                                         (make-invader 100 67 1))
              false)
(check-expect (between-top-right-bounds? (make-missile 100 (+ 67 INVADER-HEIGHT/2 MISSILE-HEIGHT/2))
                                         (make-invader 100 67 1))
              false)
(check-expect (between-top-right-bounds? (make-missile 100 (add1 (- 67 INVADER-HEIGHT/2 MISSILE-HEIGHT/2)))
                                         (make-invader 100 67 -1))
              true)
(check-expect (between-top-right-bounds? (make-missile 100 (sub1 (+ 67 INVADER-HEIGHT/2 MISSILE-HEIGHT/2)))
                                         (make-invader 100 67 -1))
              true)
(define (between-top-right-bounds? missile invader)
  (or (and (>  (missile-bottom-y missile) (invader-top-y    invader))
           (<= (missile-bottom-y missile) (invader-bottom-y invader)))
      (and (<  (missile-top-y    missile) (invader-bottom-y invader))
           (>= (missile-top-y    missile) (invader-top-y    invader)))))

;; Missile -> Integer
;; produce given missile top y position
(check-expect (missile-top-y M1) (- (missile-y M1) MISSILE-HEIGHT/2))
(check-expect (missile-top-y M2) (- (missile-y M2) MISSILE-HEIGHT/2))
(define (missile-top-y missile)
  (- (missile-y missile) MISSILE-HEIGHT/2))

;; Missile -> Integer
;; produce given missile bottom y position
(check-expect (missile-bottom-y M1) (+ (missile-y M1) MISSILE-HEIGHT/2))
(check-expect (missile-bottom-y M2) (+ (missile-y M2) MISSILE-HEIGHT/2))
(define (missile-bottom-y missile)
  (+ (missile-y missile) MISSILE-HEIGHT/2))

;; Invader -> Integer
;; produce given invader top y position
(check-expect (invader-top-y I1) (- (invader-y I1) INVADER-HEIGHT/2))
(check-expect (invader-top-y I2) (- (invader-y I2) INVADER-HEIGHT/2))
(define (invader-top-y invader)
  (- (invader-y invader) INVADER-HEIGHT/2))

;; Invader -> Integer
;; produce given invader bottom y position
(check-expect (invader-bottom-y I1) (+ (invader-y I1) INVADER-HEIGHT/2))
(check-expect (invader-bottom-y I2) (+ (invader-y I2) INVADER-HEIGHT/2))
(define (invader-bottom-y invader)
  (+ (invader-y invader) INVADER-HEIGHT/2))

;; Invader -> Invader
;; Move the given invader down, switching its direction when it hits the wall
(check-expect (move-invader (make-invader 150 100 12))
              (make-invader (+ 150 12) (+ 100 INVADER-Y-SPEED) 12))
(check-expect (move-invader INVADER-ON-RWALL)
              (make-invader (+ (invader-x INVADER-ON-RWALL) -12)
                            (+ (invader-y INVADER-ON-RWALL) INVADER-Y-SPEED)
                            -12))
(check-expect (move-invader INVADER-ON-LWALL)
              (make-invader (+ (invader-x INVADER-ON-LWALL) 12)
                            (+ (invader-y INVADER-ON-LWALL) INVADER-Y-SPEED)
                            12))
              
;(define (move-invader invader) invader) ; stub

(define (move-invader invader)
  (if (hit-wall? invader)
      (make-invader (+ (invader-x invader) (- (invader-dx invader)))
                    (+ (invader-y invader) INVADER-Y-SPEED)
                    (- (invader-dx invader)))
      (make-invader (+ (invader-x invader) (invader-dx invader))
                    (+ (invader-y invader) INVADER-Y-SPEED)
                    (invader-dx invader))))

;; Invader -> Boolean
;; Produce true when invader hits the left or right screen wall
(check-expect (hit-wall? (make-invader 150 100 12)) false)
(check-expect (hit-wall? INVADER-ON-RWALL) true)
(check-expect (hit-wall? INVADER-ON-LWALL) true)

;(define (hit-wall? invader) false) ; stub

(define (hit-wall? invader)
  (or (>= (invader-x invader) (- WIDTH INVADER-WIDTH/2))
      (<= (invader-x invader) INVADER-WIDTH/2)))

;; (listof Invader) Natural[0, 100] -> (listof Invader)
;; Add a new invader when a number greater or equal than 98% of INVADER-RATE is generated
(check-expect (add-invader empty 69) empty)
(check-random (add-invader empty 98)
              (cons (make-invader (random WIDTH)
                                  (- INVADER-HEIGHT/2)
                                  (dx-for-invader (random 2)))
                    empty))
(check-expect (add-invader (list I1 I2) 94) (list I1 I2))
(check-random (add-invader (list I1 I2) 99)
              (list I1
                    I2
                    (make-invader (random WIDTH)
                                  (- INVADER-HEIGHT/2)
                                  (dx-for-invader (random 2)))))
(check-random (add-invader (list I1 I2) 100)
              (list I1
                    I2
                    (make-invader (random WIDTH)
                                  (- INVADER-HEIGHT/2)
                                  (dx-for-invader (random 2)))))

;(define (add-invader invaders probability) invaders) ; stub

(define (add-invader invaders probability)
  (cond [(< probability 98) invaders]
        [(empty? invaders) (cons (make-invader (random WIDTH)
                                               (- INVADER-HEIGHT/2)
                                               (dx-for-invader (random 2)))
                                 empty)]
        [else
         (cons (first invaders)
               (add-invader (rest invaders) probability))]))

;; Natural[0, 1] -> Integer
;; produce a negative direction when 0 or positive direction when 1
(check-expect (dx-for-invader 1) INVADER-X-SPEED)
(check-expect (dx-for-invader 0) (- INVADER-X-SPEED))

;(define (dx-for-invader d) 0) ; stub

(define (dx-for-invader d)
  (cond [(= d 0) (- INVADER-X-SPEED)]
        [(= d 1) INVADER-X-SPEED]))

;; (listof Missile) (listof Invader) -> (listof Missile)
;; Move missiles up in the screen
(check-expect (move-missiles empty empty) empty)
(check-expect (move-missiles (list M1 M2) empty)
              (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))
                    (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED))))
(check-expect (move-missiles (list M1 M2) (list I3))
              (list (move-missile M1) (move-missile M2)))
(check-expect (move-missiles (list M1-HITTING-I1-ON-LEFT)  (list I1 I2)) empty)
(check-expect (move-missiles (list M1-HITTING-I1-ON-RIGHT) (list I1 I2)) empty)

;(define (move-missiles missiles) missiles) ; stub

(define (move-missiles missiles invaders)
  (cond [(empty? missiles ) empty]
        [else
         (if (missile-hit-invaders? (first missiles) invaders)
             (move-missiles (rest missiles) invaders)
             (cons (move-missile  (first missiles))
                   (move-missiles (rest missiles) invaders)))]))

;; Missile (listof Invader) -> Boolean
;; produce true when given missile hits an invader
(check-expect (missile-hit-invaders? M1 empty) false)
(check-expect (missile-hit-invaders? (make-missile (/ WIDTH 2) 90)
                                     (list (make-invader (/ WIDTH 2) 30 -15)))
              false)
(check-expect (missile-hit-invaders? (make-missile (/ WIDTH 2) 30)
                                     (list (make-invader (/ WIDTH 2) 90 -15)
                                           (make-invader INVADER-WIDTH/2 30 -15)))
              false)
(check-expect (missile-hit-invaders? M1-HITTING-I1-ON-LEFT  (list I1)) true)
(check-expect (missile-hit-invaders? M1-HITTING-I1-ON-RIGHT (list I1)) true)
(check-expect (missile-hit-invaders? M1-HITTING-I1-ON-LEFT
                                     (list (make-invader (/ WIDTH 2) 90 -1) I1))
              true)
(check-expect (missile-hit-invaders? M1-HITTING-I1-ON-RIGHT
                                     (list (make-invader (/ WIDTH 2) 90 -1) I1))
              true)

;(define (missile-hit-invaders? missile invaders) false) ; stub

(define (missile-hit-invaders? missile invaders)
  (cond [(empty? invaders) false]
        [else
         (if (between-bounds? missile (first invaders))
             true
             (missile-hit-invaders? missile (rest invaders)))]))

;; Missile -> Missile
;; Move missile up in the screen
(check-expect (move-missile M1)
              (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
(check-expect (move-missile M2)
              (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED)))

;(define (move-missile missile) missile) ; stub

(define (move-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; Move given tank to its current direction, stopping it when it reaches the start/end of the scene
(check-expect (move-tank (make-tank (/ WIDTH 2) -1))
              (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
(check-expect (move-tank (make-tank (/ WIDTH 2) 1))
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (move-tank (make-tank TANK-WIDTH/2 -1))
              (make-tank TANK-WIDTH/2 -1))
(check-expect (move-tank (make-tank (- WIDTH TANK-WIDTH/2) 1))
              (make-tank (- WIDTH TANK-WIDTH/2) 1))
(check-expect (move-tank (make-tank TANK-WIDTH/2 1))
              (make-tank (+ TANK-WIDTH/2 TANK-SPEED) 1))
(check-expect (move-tank (make-tank (- WIDTH TANK-WIDTH/2) -1))
              (make-tank (- (- WIDTH TANK-WIDTH/2) TANK-SPEED) -1))

;(define (move-tank tank) tank) ; stub

(define (move-tank tank)
  (cond [(and (not (on-screen-left-end? tank))
              (moving-left? tank))
         (make-tank (- (tank-x tank) TANK-SPEED)
                    (tank-dir tank))]
        [(and (not (on-screen-right-end? tank))
              (moving-right? tank))
         (make-tank (+ (tank-x tank) TANK-SPEED)
                    (tank-dir tank))]
        [else
         (make-tank (tank-x tank)
                    (tank-dir tank))]))

;; Tank -> Boolean
;; produce true when tank x coordinate it's on the left end of the screen
(check-expect (on-screen-left-end? (make-tank (/ WIDTH 2) 1)) false)
(check-expect (on-screen-left-end? (make-tank (add1 TANK-WIDTH/2) -1)) false)
(check-expect (on-screen-left-end? (make-tank TANK-WIDTH/2 -1)) true)
(define (on-screen-left-end? tank)
  (<= (tank-x tank) TANK-WIDTH/2))

;; Tank -> Boolean
;; produce true when tank x coordinate it's on the right end of the screen
(check-expect (on-screen-right-end? (make-tank (/ WIDTH 2) 1)) false)
(check-expect (on-screen-right-end? (make-tank (sub1 (- WIDTH TANK-WIDTH/2)) -1)) false)
(check-expect (on-screen-right-end? (make-tank (- WIDTH TANK-WIDTH/2) -1)) true)
(define (on-screen-right-end? tank)
  (>= (tank-x tank) (- WIDTH TANK-WIDTH/2)))

;; Tank -> Boolean
;; produce true if given tank is moving left
(check-expect (moving-left? (make-tank (/ WIDTH 2) -1)) true)
(check-expect (moving-left? (make-tank (/ WIDTH 2) 1)) false)

;(define (moving-left? tank) false) ; stub

(define (moving-left? tank)
  (cond [(= (tank-dir tank) -1) true]
        [(= (tank-dir tank) 1) false]))

;; Tank -> Boolean
;; produce true if given tank is moving right
(check-expect (moving-right? (make-tank (/ WIDTH 2) -1)) false)
(check-expect (moving-right? (make-tank (/ WIDTH 2) 1)) true)

;(define (moving-right? tank) false) ; stub

(define (moving-right? tank)
  (cond [(= (tank-dir tank) -1) false]
        [(= (tank-dir tank) 1) true]))

;; KeyEvent Game -> Game
;; Updates the state of game when game control keys are pressed
(check-expect (update-on-key-pressed G1 "up") G1)
(check-expect (update-on-key-pressed G1 "a") G1)
(check-expect (update-on-key-pressed G2 "left")
              (make-game (game-invaders G2)
                         (game-missiles G2)
                         (move-tank-on-key (game-tank G2) "left")))
(check-expect (update-on-key-pressed G2 "right")
              (make-game (game-invaders G2)
                         (game-missiles G2)
                         (move-tank-on-key (game-tank G2) "right")))
(check-expect (update-on-key-pressed G3 " ")
              (make-game (game-invaders G3)
                         (add-missile-on-spacebar (game-missiles G3)
                                                  (game-tank G3)
                                                  " ")
                         (game-tank G3)))

;(define (update-on-key-pressed game ke) game) ; stub

(define (update-on-key-pressed game ke)
  (make-game (game-invaders game)
             (add-missile-on-spacebar (game-missiles game)
                                      (game-tank game)
                                      ke)
             (move-tank-on-key (game-tank game) ke)))

;; (listof Missile) Tank KeyEvent -> (listof Missile)
;; Launch a missile when spacebar is pressed
(check-expect (add-missile-on-spacebar empty T1 "left") empty)
(check-expect (add-missile-on-spacebar (list M1) T1 "right") (list M1))
(check-expect (add-missile-on-spacebar empty T1 " ")
              (list (make-missile (tank-x T1)
                                  (- HEIGHT MISSILE-HEIGHT/2 TANK-HEIGHT))))
(check-expect (add-missile-on-spacebar (list M1) T2 " ")
              (list M1
                    (make-missile (tank-x T2)
                                  (- HEIGHT MISSILE-HEIGHT/2 TANK-HEIGHT))))

;(define (add-missile-on-spacebar missiles tank ke) missiles) ; stub

(define (add-missile-on-spacebar missiles tank ke)
  (cond [(not (string=? " " ke)) missiles]
        [(empty? missiles)
         (cons (make-missile (tank-x tank)
                             (- HEIGHT MISSILE-HEIGHT/2 TANK-HEIGHT)) empty)]
        [else
         (cons (first missiles)
               (add-missile-on-spacebar (rest missiles) tank ke))]))

;; Tank KeyEvent -> Tank
;; Moves given tank to left or right when left/right arrow is pressed
(check-expect (move-tank-on-key T1 " ") T1)
(check-expect (move-tank-on-key T1 "up") T1)
(check-expect (move-tank-on-key (make-tank (/ WIDTH 2) -1) "right")
              (make-tank (/ WIDTH 2) 1))
(check-expect (move-tank-on-key (make-tank (/ WIDTH 2) 1) "left")
              (make-tank (/ WIDTH 2) -1))
(check-expect (move-tank-on-key (make-tank (/ WIDTH 2) -1) "left")
              (make-tank (/ WIDTH 2) -1))
(check-expect (move-tank-on-key (make-tank (/ WIDTH 2) 1) "right")
              (make-tank (/ WIDTH 2) 1))

;(define (move-tank-on-key tank ke) tank) ; stub

(define (move-tank-on-key tank ke)
  (cond [(string=? ke "left")
         (make-tank (tank-x tank) -1)]
        [(string=? ke "right")
         (make-tank (tank-x tank) 1)]
        [else
         (make-tank (tank-x tank)
                    (tank-dir tank))]))

;; Game -> Image
;; Renders the given game to the background screen
(check-expect (render-game (make-game empty empty T1))
              (place-image TANK
                           (tank-x T1)
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (render-game (make-game (list I1) empty T1))
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           (place-image TANK
                                        (tank-x T1)
                                        (- HEIGHT TANK-HEIGHT/2)
                                        BACKGROUND)))
(check-expect (render-game (make-game (list I1) (list M1) T1))
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           (place-image MISSILE
                                        (missile-x M1)
                                        (missile-y M1)
                                        (place-image TANK
                                                     (tank-x T1)
                                                     (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))
(check-expect (render-game (make-game (list I1 I2)
                                      (list M1 M2)
                                      T2))
              (render-invaders (list I1 I2)
                               (render-missiles (list M1 M2)
                                                (render-tank T1 BACKGROUND))))

;(define (render-game game) game) ; stub

(define (render-game game)
  (render-invaders (game-invaders game)
                   (render-missiles (game-missiles game)
                                    (render-tank (game-tank game) BACKGROUND))))

;; (listof Invader) -> Image
;; Render the given invaders into the background
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list I1) BACKGROUND)
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           BACKGROUND))
(check-expect (render-invaders (list I1 I2) BACKGROUND)
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           (render-invaders (list I2) BACKGROUND)))

;(define (render-invaders invaders background) background) ; stub

(define (render-invaders invaders background)
  (cond [(empty? invaders) background]
        [else
         (place-image INVADER
                      (invader-x (first invaders))
                      (invader-y (first invaders))
                      (render-invaders (rest invaders) background))]))

;; (listof Invader) -> Image
;; Render the given missiles into the background
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list M1) BACKGROUND)
              (place-image MISSILE
                           (missile-x M1)
                           (missile-y M1)
                           BACKGROUND))
(check-expect (render-missiles (list M2 M1) BACKGROUND)
              (place-image MISSILE
                           (missile-x M2)
                           (missile-y M2)
                           (render-missiles (list M1) BACKGROUND)))

;(define (render-missiles missiles background) background) ; stub

(define (render-missiles missiles background)
  (cond [(empty? missiles) background]
        [else
         (place-image MISSILE
                      (missile-x (first missiles))
                      (missile-y (first missiles))
                      (render-missiles (rest missiles) background))]))

;; Tank -> Image
;; Render the given tank into the background
(check-expect (render-tank T1 BACKGROUND)
              (place-image TANK
                           (tank-x T1)
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (render-tank T2 (square 10 "solid" "cyan"))
              (place-image TANK
                           (tank-x T2)
                           (- HEIGHT TANK-HEIGHT/2)
                           (square 10 "solid" "cyan")))

;(define (render-tank tank background) background) ; stub

(define (render-tank tank background)
  (place-image TANK
               (tank-x tank)
               (- HEIGHT TANK-HEIGHT/2)
               background))

;; Game -> Boolean
;; Produce true when the game ends, happening when an invader lands
(check-expect (end-game? (make-game (list I1) (list M1) T0)) false)
(check-expect (end-game? (make-game (list (make-invader 10 10 -1)
                                          (make-invader 150 100 1))
                                    (list M1 M2 M3)
                                    T1))
              false)
(check-expect (end-game? (make-game (list I1 (make-invader 30 (- HEIGHT INVADER-HEIGHT/2) 1))
                                    (list M1 M2)
                                    T1))
              true)
(define (end-game? game)
  (landed-invaders? (game-invaders game)))

;; (listof Invader) -> Boolean
;; produce true when any of the given invaders land at the bottom of the scene
(check-expect (landed-invaders? empty) false)
(check-expect (landed-invaders? (list (make-invader 10 10 -1)
                                      (make-invader 150 100 1)))
              false)
(check-expect (landed-invaders? (list (make-invader 10 10 -1)
                                      (make-invader 150 100 1)
                                      (make-invader 30 HEIGHT -1)))
              true)
(define (landed-invaders? invaders)
  (cond [(empty? invaders) false]
        [else
         (if (>= (invader-bottom-y (first invaders)) HEIGHT)
             true
             (landed-invaders? (rest invaders)))]))






