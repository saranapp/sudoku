;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Game_Squash) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; Goal: Create a simple squash game
;;; that allows expert squash players to challenge themselves by
;;; trying to keep multiple balls in play at once.

;;; Use Arrows to move the racket
;;; Press 'b' to spawn balls
;;; Press '' to pause and resume the game


(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
      
(provide simulation
         initial-world
         world-ready-to-serve?
         world-after-tick 
         world-after-key-event 
         world-balls
         world-racket
         ball-x
         ball-y
         racket-x
         racket-y
         ball-vx
         ball-vy
         racket-vx
         racket-vy
         world-after-mouse-event
         racket-after-mouse-event
         racket-selected?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; MAIN FUNCTION.
;;; simulation : PosReal -> World
;;; GIVEN   : the speed of the simulation, in seconds per tick
;;;     (so larger numbers run slower)
;;; EFFECT  : runs the simulation, starting with the initial world
;;; RETURNS : the final state of the world
;;; EXAMPLES:
;;;     (simulation 1) runs in super slow motion
;;;     (simulation 1/24) runs at a more realistic speed
;;; STRATEGY: Combine simpler functions.

(define (simulation simulation-speed)
  (big-bang (initial-world simulation-speed)
            (on-tick world-after-tick simulation-speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; initial-world : PosReal -> World
;;; GIVEN   : the speed of the simulation, in seconds per tick
;;;     (so larger numbers run slower)
;;; RETURNS : the ready-to-serve state of the world
;;; EXAMPLE : (initial-world 1)
;;; STRATEGY: Use constructor template for World.
(define (initial-world s)
  (make-world
   (list (make-ball INIT-X INIT-Y INIT-VELOCITY INIT-VELOCITY))
   (make-racket INIT-X INIT-Y INIT-VELOCITY INIT-VELOCITY
                INIT-RACKET-SELECTABLE OUT-OF-BOUND OUT-OF-BOUND)
   READY-STATE
   INIT-TICKS-PASSED
   s))

;;; TESTS:
(begin-for-test
  (check-equal? (initial-world (/ 1 24))
                (make-world (list ball-at-330-384)
                            unselect-racket-at-330-384
                            "ready"
                            0
                            (/ 1 24))
                "Initial world returns all components initialized"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

;;; standard colors used in the simulation
(define COLOR-BLACK "black")
(define COLOR-WHITE "white")
(define COLOR-YELLOW "yellow")
(define COLOR-GREEN "green")
(define COLOR-BLUE "blue")

;;; dimensions and color of the court
(define COURT-WIDTH 425)
(define COURT-HEIGHT 649)
(define HALF-COURT-WIDTH (/ COURT-WIDTH 2))
(define HALF-COURT-HEIGHT (/ COURT-HEIGHT 2))
(define COURT-LEFT-WALL-COORDINATE 0)
(define COURT-FRONT-WALL-COORDINATE 0)
(define EMPTY-COURT-CANVAS (empty-scene (add1 COURT-WIDTH)
                                        (add1 COURT-HEIGHT)))

;;; The court walls are represented as a black outlined rectangle
(define COURT-WALLS (rectangle (add1 COURT-WIDTH)
                               (add1 COURT-HEIGHT)
                               "outline"
                               COLOR-BLACK))

;;; Court walls painted on the court
(define COURT-CANVAS-WITH-WALLS (place-image COURT-WALLS
                                             HALF-COURT-WIDTH
                                             HALF-COURT-HEIGHT
                                             EMPTY-COURT-CANVAS))

;;; An-unpaused court floor is represented as a solid white rectangle
(define UNPAUSED-COURT-FLOOR (rectangle COURT-WIDTH
                                        COURT-HEIGHT
                                        "outline"
                                        COLOR-BLACK))

;;; A paused court is represented as a solid yellow rectangle
(define PAUSED-COURT-FLOOR (rectangle COURT-WIDTH
                                      COURT-HEIGHT
                                      "solid"
                                      COLOR-YELLOW))

;;; The ball is represented as a black circle of radius 3 pixels
(define BALL-CIRCLE (circle 3 "solid" COLOR-BLACK))

;;; The racket is represented as a green rectangle of 47x7 pixels
(define RACKET-LENGTH 47)
(define HALF-RACKET-LENGTH (/ RACKET-LENGTH 2))
(define RACKET-HEIGHT 7)
(define HALF-RACKET-HEIGHT (/ RACKET-HEIGHT 2))
(define RACKET-RECTANGLE (rectangle RACKET-LENGTH
                                    RACKET-HEIGHT
                                    "solid"
                                    COLOR-GREEN))

;;; Initial ready-to-serve coordinates of the ball and racket
(define INIT-X 330)
(define INIT-Y 384)

;;; Initial ready-to-serve velocity of the ball and racket
(define INIT-VELOCITY 0)

;;; Initial ready-to-serve color of the court's floor
(define INIT-COURT-COLOR COLOR-WHITE)

;;; Initial rally-state-velocity of the ball
(define INIT-RALLY-VELOCITY-X 3)
(define INIT-RALLY-VELOCITY-Y -9)

;;; Initial number of ticks passed
(define INIT-TICKS-PASSED 0)

;;; Initial racket selectable states
(define INIT-RACKET-SELECTABLE false)
(define OUT-OF-BOUND -30)

;;; Adjust Racket's coordinates on collision with the walls of the court.
(define RACKET-X-WITH-RIGHT-WALL
  (- COURT-WIDTH HALF-RACKET-LENGTH))
(define RACKET-X-WITH-LEFT-WALL
  (+ COURT-LEFT-WALL-COORDINATE HALF-RACKET-LENGTH))
(define RACKET-Y-WITH-BOTTOM-WALL
  (- COURT-HEIGHT HALF-RACKET-HEIGHT))

;;; Time of pause in seconds
(define PAUSE-TIME 3)

;;; Circle to follow mouse pointer during mouse-drag.
(define MOUSE-POINTER-CIRCLE (circle 4 "solid" COLOR-BLUE))

;;; Radius around the racket's center where it is selectable
(define RACKET-SELECTION-RADIUS 25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; DATA DEFINITIONS

;;; StateName:
;;; A StateName is one of the following strings
;;; -- "ready"
;;; -- "rally"
;;; -- "pause"
;;; INTERPRETATION: represents the current state of the World.

;;; CONSTRUCTOR TEMPLATE: Not needed.

;;; OBSERVER TEMPLATE:
;;; state-name-fn : StateName -> ??
;;; (define (state-name-fn s)
;;; (cond
;;;     [(string=? s "ready") ...]
;;;     [(string=? s "rally")  ...]
;;;     [(string=? s "pause") ...]))
;;
;;; EXAMPLES:
(define READY-STATE "ready")
(define RALLY-STATE "rally")
(define PAUSE-STATE "pause")


;;; Velocity : Integer  Number of pixels per tick along the x or y direction
;;;                     inside the graphical coordinates of the court.


;;; Ball:
;;; REPRESENTATION:
;;; A Ball is represented as a struct (make-ball x y vx vy)
;;; INTERPRETATION:
;;; x        : Integer    x coordinate of the ball in graphics coordinates,
;;;                       measured in pixels.
;;; y        : Integer    y coordinate of the ball in graphics coordinates,
;;;                       measured in pixels.
;;; vx       : Velocity   of the ball along x direction.
;;; vy       : Velocity   of the ball along y direction.
;;
;;; IMPLEMENTATION:
(define-struct ball (x y vx vy))

;;; CONSTRUCTOR TEMPLATE:
;;; (make-ball Integer Integer Velocity Velocity)
;;
;;; OBSERVER TEMPLATE:
;;; ball-fn : Ball -> ??
;;; (define (ball-fn w)
;;;   (... (ball-x w)
;;;        (ball-y w)
;;;        (ball-vx w)
;;;        (ball-vy w)))
;;;
;;; EXAMPLE:
(define BALL-AT-INIT-POSITION (make-ball 330 384 3 -9))



;;; BallList:
;;; REPRESENTATION:
;;; A BallList is represented as a list of Ball.
;;;
;;; CONSTRUCTOR TEMPLATE AND INTERPRETATION:
;;; empty                         -- the empty list.
;;; (cons b bs)
;;; 
;;;  WHERE:
;;;      b   is of type Ball      -- the first ball in the list.
;;;      bs  is of type BallList  -- the rest of the balls in the list.
;;;
;;; OBSERVER TEMPLATE:
;;; (bl-fn lst) : BallList -> ??
;;; (define (bl-fn lst)
;;; (cond [(empty? lst) ...]
;;;       [else (... (first lst)
;;;                  (bl-fn (rest lst)))]))



;;; Racket:
;;; REPRESENTATION:
;;; A Racket is represented as a struct
;;; (make-racket x y vx vy selected? mouse-x mouse-y)
;;; INTERPRETATION:
;;; x         : Integer    x coordinate of the racket in graphics coordinates
;;;                        measured in pixels.
;;; y         : Integer    y coordinate of the racket in graphics coordinates
;;;                        measured in pixels.
;;; vx        : Velocity   of the Racket along x direction.
;;; vy        : Velocity   of the Racket along y direction.
;;; selected? : Boolean    describes whether or not the racket is selected.
;;; mouse-x   : Integer    x coordinate of the drag-handle for the mouse,
;;;                        if racket is selected. (measured in pixels)
;;; mouse-y   : Integer    y coordinate of the drag-handle for the mouse,
;;;                        if racket is selected. (measured in pixels)
;;;
;;; IMPLEMENTATION:
(define-struct racket (x y vx vy selected? mouse-x mouse-y))
;;;
;;; CONSTRUCTOR TEMPLATE:
;;; (make-racket Integer Integer Velocity Velocity Boolean Integer Integer)
;;;
;;; OBSERVER TEMPLATE:
;;; racket-fn : Racket -> ??
;;; (define (racket-fn r)
;;;  (... (racket-x r)
;;;       (racket-y r)
;;;       (racket-vx r)
;;;       (racket-vy r)
;;;       (racket-selected? r)
;;;       (racket-mouse-x r)
;;;       (racket-mouse-y r)))
;;;
;;; Examples of rackets for testing
(define selected-racket-at-50-50 (make-racket 50 50 1 1 true 70 70))
(define unselected-racket-at-50-50 (make-racket 50 50 1 1 false 70 70))
(define selected-racket-at-100-100 (make-racket 100 100 1 1 true 110 110))
(define unselected-racket-at-100-100 (make-racket 100 100 1 1 false 110 110))



;;; World:
;;; REPRESENTATION:
;;; A World is represented as a struct
;;; (make-world balls racket state ticks-since-state speed))
;;; INTERPRETATION:
;;; balls             : BallList   the list of balls in the world.
;;; racket            : Racket     the racket in the world.
;;; state             : StateName  the state name of the world's state.
;;; ticks-since-state : Integer    number of ticks that passed since the
;;;                                current state started.
;;; speed             : PosReal    the speed of the simulation,
;;;                                in seconds per tick.
;;; IMPLEMENTATION:
(define-struct world (balls racket state ticks-since-state speed))
;;;
;;; CONSTRUCTOR TEMPLATE:
;;; (make-world BallList Racket State Integer PosReal)
;;;
;;; OBSERVER TEMPLATE:
;;; world-fn : World -> ??
;;; (define (world-fn w)
;;;   (... (world-balls w)
;;;        (world-racket w)
;;;        (world-state w)
;;;        (world-ticks-since-state w)
;;;        (world-speed w)))



;;; SimulationKeyEvent:
;;; A SimulationKeyEvent is one of the following strings
;;; -- "left"
;;; -- "right"
;;; -- "up"
;;; -- "down"
;;; -- " "
;;; -- "b"
;;; INTERPRETATION: represents the possible KeyEvents in the simulation.
;;;                 All KeyEvents are represented as strings.
;;;                 But not all strings are KeyEvents.
;;;
;;; CONSTRUCTOR TEMPLATE: Not needed.
;;;
;;; OBSERVER TEMPLATE:
;;; simulation-key-event-fn : SimulationKeyEvent -> ??
;;; (define (simulation-key-event-fn kev)
;;; (cond
;;;     [(key=? kev "left") ...]
;;;     [(key=? kev "right")  ...]
;;;     [(key=? kev "up")  ...]
;;;     [(key=? kev "down")  ...]
;;;     [(key=? kev " ") ...]
;;;     [(key=? kev "b") ...]))
;;;
;;; EXAMPLES:
(define LEFT-KEY "left")
(define RIGHT-KEY "right")
(define UP-KEY "up")
(define DOWN-KEY "down")
(define SPACE-KEY " ")
(define B-KEY "b")


;;; END OF DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; world-ready-to-serve? : World -> Boolean
;;; GIVEN   : a world
;;; RETURNS : true iff the world is in its ready-to-serve state
;;; EXAMPLES: (world-ready-to-serve? INITIAL-WORLD-24) => true
;;; STRATEGY: Combine simpler functions
(define (world-ready-to-serve? w)
  (string=? (world-state w) READY-STATE))

;;; TESTS:
(define INITIAL-WORLD-24 (initial-world (/ 1 24)))
(begin-for-test
  (check-equal? (world-ready-to-serve? INITIAL-WORLD-24)
                true
                "Expecting true since its ready state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; world-rally-state? : World -> Boolean
;;; GIVEN   : a world
;;; RETURNS : true iff the world is in its rally state
;;; EXAMPLES: (world-rally-state? (make-world (list ball-at-330)
;;;                                           unselected-racket-at-330
;;;                                           "rally"
;;;                                           0
;;;                                           (/ 1 24)))
;;;                =>      true
;;; STRATEGY: Combine simpler functions
(define (world-rally-state? w)
  (string=? (world-state w) RALLY-STATE))


;;; TESTS:
(define unselected-racket-at-330 (make-racket 330 384 0 0 false -30 -30))
(define ball-at-330 (make-ball 330 384 3 -9))
(define WORLD-AT-INIT-RALLY-STATE (make-world (list ball-at-330)
                                              unselected-racket-at-330
                                              "rally"
                                              0
                                              (/ 1 24)))
(begin-for-test
  (check-equal? (world-rally-state? WORLD-AT-INIT-RALLY-STATE)
                true
                "Expecting true for rally state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; pause-world : World -> World
;;; GIVEN  : a world thats in rally state,
;;; RETURNS : the world that should follow the given world
;;;          after the space(pause) key event.
;;; EXAMPLE : (pause-world (make-world (list (make-ball ??))
;;;                                    (make-racket ??) "rally" 10 1)
;;;              =>   (pause-world (make-world (list (make-ball ??))
;;;                                    (make-racket ??) "pause" 10 1)
;;; STRATEGY: Use constructor template for World.
(define (pause-world w)
  (make-world (world-balls w)
              (world-racket w)
              PAUSE-STATE
              INIT-TICKS-PASSED
              (world-speed w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; world-pause-state? : World -> Boolean
;;; GIVEN   : a world
;;; RETURNS : true iff the world is in its pause state
;;; EXAMPLES: (world-pause-state? (make-world (list ball-all-at-30)
;;;                                   unselected-racket-all-at-30
;;;                                   PAUSE-STATE 10 1)
;;;             => true
;;; STRATEGY: Combine simpler functions
(define (world-pause-state? w)
  (string=? (world-state w) PAUSE-STATE))

;;; TESTS:
(define unselected-racket-all-at-30 (make-racket 30 30 0 0 false -30 -30))
(define ball-all-at-30 (make-ball 330 384 3 -9))
(define PAUSE-STATE-WORLD (make-world (list ball-all-at-30)
                                      unselected-racket-all-at-30
                                      PAUSE-STATE 10 1))
(define PAUSED-WORLD-AT-INIT (pause-world WORLD-AT-INIT-RALLY-STATE))
(begin-for-test
  (check-equal? (world-pause-state? PAUSE-STATE-WORLD)
                true
                "Expecting true since given world with pause state"))
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; WORLD AFTER TICK :
;;; world-after-tick : World -> World
;;; GIVEN   : any world that's possible for the simulation
;;; RETURNS : the world that should follow the given world
;;;           after a tick
;;; EXAMPLES: (world-after-tick INITIAL-WORLD-24)
;;;            => (make-world (list (make-ball 330 384 0 0))
;;;                         unselected-racket-at-330
;;;                         "ready" 1 1/24)
;;; STRATEGY: Use observer template for StateName
(define (world-after-tick w)
  (cond [(world-ready-to-serve? w) (ready-after-tick w)]
        [(world-pause-state? w) (pause-after-tick w)]
        [(world-rally-state? w) (rally-after-tick w)]))


;;; TESTS:
(begin-for-test
  (check-equal? (world-racket (world-after-tick PAUSED-WORLD-AT-INIT))
                (world-racket PAUSED-WORLD-AT-INIT)
                "Expecting no change from paused world after tick")
  (check-equal? (world-after-tick WORLD-AT-INIT-RALLY-STATE)
                (make-world (list (make-ball 333 375 3 -9))
                            unselected-racket-at-330
                            RALLY-STATE
                            1
                            (/ 1 24))
                "Expecting world with changes to ball and racket states")
  (check-equal? (world-after-tick INITIAL-WORLD-24)
                (make-world (list (make-ball 330 384 0 0))
                            unselected-racket-at-330
                            "ready" 1 1/24)
                "Expecting no change but one tick recorded"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ready-after-tick : World -> World
;;; GIVEN   : a world that's in ready state of the simulation,
;;; RETURNS : the world that should follow the given world after a tick.
;;; EXAMPLES:
;;;      (ready-after-tick (make-world (list ball1) racket READY-STATE 12 1))
;;;           =>  (make-world (list ball1) racket READY-STATE 13 1)
;;; STRATEGY: User Constructor template for World.
(define (ready-after-tick w)
  (make-world
   (world-balls w)
   (world-racket w)
   (world-state w)
   (add1 (world-ticks-since-state w))
   (world-speed w)))


;;; TESTS:
(define unselect-racket-at-330-384 (make-racket 330 384 0 0 false -30 -30))
(define ball-at-330-384 (make-ball 330 384 0 0))
(begin-for-test
  (check-equal? (ready-after-tick INITIAL-WORLD-24)
                (make-world (list ball-at-330-384)
                            unselect-racket-at-330-384
                            READY-STATE 1 (/ 1 24))
                "Expecting no change but a tick recorded"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rally-after-tick : World -> World
;;; GIVEN   : a world that's in rally state of the simulation,
;;; RETURNS : the world that should follow the given world after a tick.
;;; EXAMPLES:
;;;      (rally-after-tick (make-world (list (make-ball 330 384 3 -9))
;;;                           (make-racket 330 384 1 2 false -30 -30)
;;;                            "rally" 12 1)
;;;           =>  (make-world (list (make-ball 333 375 3 -9))
;;;                           (make-racket 331 386 0 0 false -30 -30)
;;;                            "rally" 13 1)
;;; STRATEGY: Cases on possibility of a pause state.
(define (rally-after-tick w)
  (cond [(racket-collision-with-front-wall? (world-racket w))
         (pause-world w)]
        [(balls-collision-with-back-wall? (world-balls w))
         (world-after-balls-back-wall-collision w (world-balls w))]
        [else (rally-after-tick-without-pause w)]))

;;; TESTS:
(define ball-at-333-375 (make-ball 333 375 3 -9))
(define ball-at-333-645 (make-ball 333 645 3 9))
(define ball-at-333-333 (make-ball 333 333 3 -9))
(define unselect-racket-up-7 (make-racket 330 1 0 -7 false -30 -30))
(begin-for-test
  (check-equal? (world-after-tick WORLD-AT-INIT-RALLY-STATE)
                (make-world (list ball-at-333-375)
                            unselect-racket-at-330-384
                            RALLY-STATE 1 (/ 1 24))
                "Expecting world with changes to ball and racket states")
  (check-equal? (world-pause-state? (world-after-tick
                                     (make-world
                                      (list ball-at-333-645)
                                      unselect-racket-at-330-384
                                      RALLY-STATE 0 (/ 1 24))))
                true
                "Ball collides back wall - Expecting a paused world")
  (check-equal? (world-pause-state? (world-after-tick
                                     (make-world
                                      (list ball-at-333-333)
                                      unselect-racket-up-7
                                      RALLY-STATE 0 (/ 1 24))))
                true
                "Racket collides front wall - Expecting a paused world"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; world-after-balls-back-wall-collision : World BallList -> World
;;; GIVEN   : a world and a list of balls where one or more balls are about to
;;;           collide with the back wall,
;;; RETURNS : the world that should follow the given world after a tick.
;;; EXAMPLES: 
;;;  (world-after-balls-back-wall-collision
;;;     (make-world (list colliding-ball1 colliding-ball2 non-colliding-ball3)
;;;                   unselect-racket-at-330-384
;;;                   RALLY-STATE 1 (/ 1 24)) collision-ball-list)))
;;;
;;;   => (make-world (list non-colliding-ball3)
;;;                   unselect-racket-at-330-384
;;;                   RALLY-STATE 1 (/ 1 24))
;;;
;;; OLD STRATEGY: User observer template for BallList
#;(define (world-after-balls-back-wall-collision w bl)
    (cond [(empty? bl) w]
          [(ball-collision-with-back-wall? (first bl))
           (world-after-balls-back-wall-collision
            (world-after-ball-back-wall-collision w (first bl)) (rest bl))]
          [else (world-after-balls-back-wall-collision w (rest bl))]))

;;; NEW STRATEGY: Use HOF filter on bl, followed by HOF foldr.
(define (world-after-balls-back-wall-collision w bl)
  (foldr
   ;; Ball World -> World
   ;; GIVEN  : a ball b and the world,
   ;; RETURNS: the world with the given ball removed.
   (lambda (b world) (world-after-ball-back-wall-collision world b))
   w
   (filter ball-collision-with-back-wall? bl)))



;;; TESTS:
(define ball-crossed-back-wall-1 (make-ball 100 700 1 1))
(define ball-crossed-back-wall-2 (make-ball 20 680 1 -11))
(define collision-ball-list (list ball-crossed-back-wall-1
                                  ball-crossed-back-wall-2
                                  ball-at-333-375
                                  ball-at-333-333))
(begin-for-test
  (check-equal? (length (world-balls
                         (world-after-balls-back-wall-collision
                          (make-world collision-ball-list
                                      unselect-racket-at-330-384
                                      RALLY-STATE 1 (/ 1 24))
                          collision-ball-list)))
                2
                "Expecting only 2 balls to be remaining at the end")
  (check-equal? (world-after-balls-back-wall-collision
                 (make-world ball-at-333-375 unselect-racket-at-330-384
                             RALLY-STATE 1 (/ 1 24)) (list))
                (make-world ball-at-333-375 unselect-racket-at-330-384
                            RALLY-STATE 1 (/ 1 24))
                "Expecting no change in a world without back-wall-collision"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; world-after-ball-back-wall-collision : World Ball -> World
;;; GIVEN   : a world and a ball where the ball is about to
;;;           collide with the back wall,
;;; RETURNS : the world that should follow the given world after a tick.
;;; EXAMPLES:
;;;  (world-after-ball-back-wall-collision
;;;     (make-world (list colliding-ball1 non-colliding-ball2)
;;;                   unselect-racket-at-330-384
;;;                   RALLY-STATE 11 (/ 1 24)) collision-ball-list)))
;;;
;;;   => (make-world (list non-colliding-ball2)
;;;                   unselect-racket-at-330-384
;;;                   PAUSE-STATE 0 (/ 1 24))
;;; STRATEGY: Cases on possibility of collision with racket before the wall.
(define (world-after-ball-back-wall-collision w b)
  (cond [(balls-collision-with-racket? (world-balls w) (world-racket w))
         (rally-after-tick-without-pause w)]
        [(last-ball-remaining? w) (pause-world
                                   (world-after-loosing-a-ball w b))]
        [else (world-after-loosing-a-ball w b)]))

;;; TESTS:
(define ball-at-330-640 (make-ball 330 640 0 19))
(define unselect-racket-at-330-645 (make-racket 330 645 0 0 false -30 -30))
(begin-for-test
  (check-equal? (world-rally-state? (world-after-ball-back-wall-collision
                                     (make-world
                                      (list ball-at-330-640)
                                      unselect-racket-at-330-645
                                      RALLY-STATE 0 (/ 1 24))
                                     ball-at-330-640))
                true
                "Rally after tick without pause - ball-racket collision"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; last-ball-remaining? : World -> Boolean
;;; GIVEN   : a world in its rally state
;;; RETURNS : true iff the world has only one ball remaining in it
;;; EXAMPLES:
;;;      (last-ball-remaining? (make-world (list ball1) ??) => true
;;;      (last-ball-remaining? (make-world (list b1 b2) ??) => false
;;; STRATEGY: Combine simpler functions.
(define (last-ball-remaining? w)
  (<= (length (world-balls w)) 1))


;;; list-length : BallList -> Integer
;;; GIVEN       : a list of balls,
;;; RETURNS     : size of the list
;;; EXAMPLES    :
;;;     (list-length (list ball1 ball2 ball3)) => 3
;;;     (list-length (list)) => 0
;;; OLD STRATEGY: Use observer template for BallList.
#; (define (list-length lst)
     (cond
       [(empty? lst) 0]
       [else (+ 1 (list-length (rest lst)))]))

(begin-for-test
  (check-equal? (last-ball-remaining? (world-after-balls-back-wall-collision
                                       (make-world collision-ball-list
                                                   unselect-racket-at-330-384
                                                   RALLY-STATE 1 (/ 1 24))
                                       collision-ball-list))
                false
                "Expecting false since 2 balls remain"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; world-after-loosing-a-ball : World Ball -> World
;;; GIVEN   : the world and the ball that is about to collide with
;;;           the back wall,
;;; RETURNS : the given world after a tick, but with the given ball removed.
;;; EXAMPLES: (world-after-loosing-a-ball (make-world (list b1 b2 b3) ??) b1)
;;;                     => (make-world (list b2 b3) ??)
;;; STRATEGY: Use Constructor template for World.
(define (world-after-loosing-a-ball w b)
  (make-world (ball-removed-from-list (world-balls w) b)
              (world-racket w)
              (world-state w)
              (world-ticks-since-state w)
              (world-speed w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-removed-from-list : BallList Ball -> BallList
;;; GIVEN   : the list of balls and a ball,
;;; RETURNS : the list of balls after a tick, but with the given ball removed.
;;; EXAMPLES: (ball-removed-from-list (list b1 b2 b3) b1) => (list b2 b3)
;;; OLD STRATEGY: Use observer template for BallList.
#;(define (ball-removed-from-list bl b)
    (cond [(empty? bl) empty]
          [(equal? (first bl) b) (rest bl)]
          [else (cons (first bl) (ball-removed-from-list (rest bl) b))]))


;;; NEW STRATEGY: Use HOF filter on bl.
(define (ball-removed-from-list bl b)
  (filter
   ;; Ball -> Boolean
   ;; RETURNS: true iff the ball in its argument is not equal to the ball b.
   ;; EXAMPLE: if b = (make-ball 330 640 0 19),
   ;;          (lambda (make-ball 10 10 10 10)) => true
   (lambda (e) (not (equal? e b)))
   bl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rally-after-tick-without-pause : World -> World
;;; GIVEN   : a world which is in rally state,
;;; RETURNS : the world that should follow the given world after a tick.
;;; EXAMPLES: (rally-after-tick-without-pause (make-world
;;;                                            (list (make-ball 10 10 1 -1))
;;;                                            (make-racket 10 10 1 -1)
;;;                                            RALLY-STATE ??))
;;;                        =>            (make-world
;;;                                        (list (make-ball 11 9 1 -1))
;;;                                        (make-racket 11 9 1 -1)
;;;                                        RALLY-STATE ??)
;;; STRATEGY: Use constructor template for World. 
(define (rally-after-tick-without-pause w)
  (make-world
   (balls-after-tick (world-balls w) (world-racket w))
   (racket-after-tick w)
   (world-state w)
   (add1 (world-ticks-since-state w))
   (world-speed w)))

;;; TESTS:
(define ball-at-330-640-19 (make-ball 330 640 0 -19))
(define ball-at-330-645-19 (make-ball 330 645 0 -19))
(define BALL-ON-ROUTE-TO-RACKET (make-world
                                 (list ball-at-330-640)
                                 unselect-racket-at-330-645
                                 RALLY-STATE 0 (/ 1 24)))
(define BALL-POST-RACKET-COLLISION (make-world
                                    (list ball-at-330-645-19)
                                    unselect-racket-at-330-645
                                    RALLY-STATE 1 (/ 1 24)))
(begin-for-test
  (check-equal? (rally-after-tick-without-pause BALL-ON-ROUTE-TO-RACKET)
                BALL-POST-RACKET-COLLISION
                "Rally after tick without pause - expecting collision"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; pause-after-tick : World -> World
;;; GIVEN   : a world which is in pause state,
;;; RETURNS : the world that should follow the given world after a tick.
;;; EXAMPLES: (pause-after-tick (make-world ?? ?? PAUSE-STATE 72 (/ 1 24))
;;;                        =>   (make-world ?? ?? READY-STATE 0 (/ 1 24))
;;; STRATEGY: Cases on whether seconds since pause crossed pause limit. 
(define (pause-after-tick w)
  (if (>= (secs-since-pause w)
          PAUSE-TIME)
      (initial-world (world-speed w))
      (increment-ticks-since-pause w)))


;;; TESTS:
(define WORLD-PAUSED-24 (make-world
                         (list ball-at-330-640)
                         unselect-racket-at-330-645
                         PAUSE-STATE 75 (/ 1 24)))
(begin-for-test
  (check-equal? (pause-after-tick WORLD-PAUSED-24)
                INITIAL-WORLD-24
                "Initial world returns all components initialized"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; secs-since-pause : World -> PosReal
;;; GIVEN   : a world which is in pause state,
;;; RETURNS : the time in seconds since the world is in paused state.
;;; EXAMPLE : 
;;;          (secs-since-pause (make-world (list (make-ball ??))
;;;                            (make-racket ??)
;;;                            "pause" 48 (/ 1 24))
;;;          => 2
;;; STRATEGY: Calculate time from ticks and simulation speed.
(define (secs-since-pause w)
  (* (world-ticks-since-state w)
     (world-speed w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; increment-ticks-since-pause : World -> World
;;; GIVEN   : a world which is in pause state,
;;; RETURNS : an updated world with ticks passed count incremented by one.
;;; EXAMPLE : 
;;;          (increment-ticks-since-pause (make-world
;;;                                        (list (make-ball ??))
;;;                                        (make-racket ??)
;;;                                        "pause" 48 (/ 1 24)))
;;;          => (make-world (list (make-ball ??))
;;;                         (make-racket ??)
;;;                         "pause" 49 (/ 1 24))
;;; STRATEGY: Use constructor template for world.
(define (increment-ticks-since-pause w)
  (make-world (world-balls w)
              (world-racket w)
              (world-state w)
              (add1 (world-ticks-since-state w))
              (world-speed w)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-tentative-x : Ball -> Integer
;;; GIVEN   : a state of the ball in any state of the world,
;;; RETURNS : the new x coordinate of the ball calculated from its vx.
;;; EXAMPLES:
;;;          (ball-tentative-x (make-ball 30 30 10 10))  => 40
;;;          (ball-tentative-x (make-ball 30 30 -10 10)) => 20
;;; STRATEGY: Transcribe formula.
(define (ball-tentative-x b)
  (+ (ball-x b) (ball-vx b)))


;;; ball-tentative-y : Ball -> Integer
;;; GIVEN   : a state of the ball in any state of the world,
;;; RETURNS : the new y coordinate of the ball calculated from its vy.
;;; EXAMPLES:
;;;          (ball-tentative-y (make-ball 30 13 10 10))   => 23
;;;          (ball-tentative-y (make-ball 30 10 -10 -20)) => -10
;;; STRATEGY: Transcribe formula.
(define (ball-tentative-y b)
  (+ (ball-y b) (ball-vy b)))


;;; racket-tentative-x : Racket -> Integer
;;; GIVEN   : a state of the racket in any state of the world,
;;; RETURNS : the new x coordinate of the ball calculated from its vx.
;;; EXAMPLE : 
;;;          (racket-tentative-x (make-racket 30 13 -10 10 ??)) => 20
;;; STRATEGY: Transcribe formula.
(define (racket-tentative-x r)
  (+ (racket-x r) (racket-vx r)))


;;; racket-tentative-y : Racket -> Integer
;;; GIVEN   : a state of the racket in any state of the world,
;;; RETURNS : the new y coordinate of the ball calculated from its vy.
;;; EXAMPLE : 
;;;          (racket-tentative-y (make-racket 30 13 -10 10 ??)) => 23
;;; STRATEGY: Transcribe formula.
(define (racket-tentative-y r)
  (+ (racket-y r) (racket-vy r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; balls-after-tick : BallList Racket -> BallList
;;; GIVEN   : a list of balls and the racket in the rally state of the world,
;;; RETURNS : the list of given balls after a tick.
;;; EXAMPLES:
;;;    (balls-after-tick (list (make-ball COURT-WIDTH 30 2 19)
;;;                            (make-ball 30 0 2 -19))
;;;                      unselected-racket-all-at-30)
;;;      =>              (list (make-ball 423 49 -2 19)
;;;                            (make-ball 32 19 2 19))
;;; OLD STRATEGY: Use observer template for BallList.
#;(define (balls-after-tick bl r)
    (cond [(empty? bl) empty]
          [else (cons (ball-after-tick (first bl) r)
                      (balls-after-tick (rest bl) r))]))
;;; NEW STRATEGY: Use HOF map on bl.
(define (balls-after-tick bl r)
  (map
   ;; Ball Racket -> Ball
   ;; GIVEN  : a ball and racket in the world,
   ;; RETURNS: the state of the ball after a tick.
   (lambda (b) (ball-after-tick b r))
   bl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-after-tick : Ball Racket -> Ball
;;; GIVEN   : a ball and the racket in rally state of the world,
;;; RETURNS : the given ball after a tick.
;;; EXAMPLES:
;;;    (ball-after-tick (make-ball 30 0 2 -19) unselected-racket-all-at-30)
;;;      =>             (make-ball 32 19 2 19)
;;; STRATEGY: Cases on whether ball collides with the racket or any of
;;;           the walls of the court.
(define (ball-after-tick b r)
  (cond [(ball-collision-with-racket? b r)
         (ball-post-racket-collision b r)]
        [(ball-collision-with-right-wall? b)
         (ball-post-right-wall-collision b)]
        [(ball-collision-with-left-wall? b)
         (ball-post-left-wall-collision b)]
        [(ball-collision-with-front-wall? b)
         (ball-post-front-wall-collision b)]
        [else (ball-without-collision b)]))


;;; TESTS:
(define BALL-COLLIDING-RIGHT-WALL (make-world
                                   (list (make-ball COURT-WIDTH 30 2 19))
                                   unselected-racket-all-at-30
                                   RALLY-STATE 0 (/ 1 24)))
(define BALL-COLLIDING-LEFT-WALL (make-world
                                  (list (make-ball 0 30 -2 19))
                                  unselected-racket-all-at-30
                                  RALLY-STATE 0 (/ 1 24)))
(define BALL-COLLIDING-FRONT-WALL (make-world
                                   (list (make-ball 30 0 2 -19))
                                   unselected-racket-all-at-30
                                   RALLY-STATE 0 (/ 1 24)))

(begin-for-test
  (check-equal? (ball-after-tick
                 (make-ball COURT-WIDTH 30 2 19) unselected-racket-all-at-30)
                (make-ball 423 49 -2 19)
                "Ball on right wall - expecting negated vx and x")
  (check-equal? (ball-after-tick
                 (make-ball 0 30 -2 19) unselected-racket-all-at-30)
                (make-ball 2 49 2 19)
                "Ball on left wall - expecting negated vx and x")
  (check-equal?  (ball-after-tick
                  (make-ball 30 0 2 -19) unselected-racket-all-at-30)
                 (make-ball 32 19 2 19)
                 "Ball on front wall - expecting negated y and vy"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; balls-collision-with-racket? : BallList Racket -> Boolean
;;; GIVEN   : a list of balls and a racket in rally state of the world,
;;; RETURNS : true iff atleast one of the balls tentatively
;;;           collides with the racket.
;;; EXAMPLES:
;;;     (balls-collision-with-racket? (list (make-ball 265 381 -3 9)
;;;                                         (make-ball 346 117 -3 -12))
;;;                                   (make-racket 255 384 0 0 false -30 -30))
;;;                => true
;;; OLD STRATEGY: Use observer template for BallList.
#;(define (balls-collision-with-racket? bl r)
    (cond [(empty? bl) false]
          [else (if (ball-collision-with-racket? (first bl) r)
                    true
                    (balls-collision-with-racket? (rest bl) r))]))

;;; NEW STRATEGY: Use HOF ormap on bl.
(define (balls-collision-with-racket? bl r)
  (ormap
   ;; Ball Racket -> Boolean
   ;; GIVEN  : a ball b and a racket r in the world,
   ;; RETURNS: true iff the ball tentatively collides with the racket.
   (lambda (b) (ball-collision-with-racket? b r))
   bl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-collision-with-racket? : Ball Racket -> Boolean
;;; GIVEN   : the states of the ball and racket in rally state of the world,
;;; RETURNS : true iff the ball collides with the racket.
;;; EXAMPLES:
;;;     (ball-collision-with-racket? (make-ball 265 381 -3 9)
;;;                                  (make-racket 255 384 0 0 false -30 -30))
;;;                => true
;;; STRATEGY: Combine simpler functions.
(define (ball-collision-with-racket? b r)
  (and (ball-moving-towards-racket? b r)
       (line-segments-intersect? (ball-x b)
                                 (ball-y b)
                                 (ball-tentative-x b)
                                 (ball-tentative-y b)
                                 (racket-tentative-left-x r)
                                 (racket-tentative-right-x r)
                                 (racket-tentative-y r))))

;;; TESTS
(define ball-rack-coll-true (make-ball 265 381 -3 9))
(define unselect-rack-ball-col-true (make-racket 255 384 0 0 false -30 -30))
(define ball-rack-coll-false (make-ball 346 117 -3 -12))
(define unselect-rack-ball-col-fal (make-racket 339 124 0 0 false -30 -30))
(begin-for-test
  (check-equal? (ball-collision-with-racket? ball-rack-coll-true
                                             unselect-rack-ball-col-true)
                true
                "Ball contact from above with racket below returns true")
  (check-equal? (ball-collision-with-racket? ball-rack-coll-false
                                             unselect-rack-ball-col-fal)
                false
                "Ball moving away with racket below it returns false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-moving-towards-racket? : Ball Racket -> Boolean
;;; GIVEN   : the states of the ball and racket in rally state of the world,
;;; RETURNS : true iff the ball's motion is towards the racket.
;;; EXAMPLES:
;;;      (ball-moving-towards-racket?
;;;              (make-ball 100 35 3 4)
;;;              (make-racket 100 40 1 1 false -30 -30))
;;;      =>  true
;;; STRATEGY: Combine simpler functions.
(define (ball-moving-towards-racket? b r)
  (and (racket-below-ball? b r) (> (ball-vy b) 0)))

;;; TESTS
(begin-for-test
  (check-equal? (ball-moving-towards-racket?
                 (make-ball 100 35 3 4)
                 (make-racket 100 40 1 1 false -30 -30))
                true
                "Ball moving towards racket from above returns true")
  (check-equal? (ball-moving-towards-racket?
                 (make-ball 10 20 3 -4)
                 (make-racket 8 20 7 8 false -30 -30))
                false
                "Ball moving towards racket from above returns false")
  (check-equal? (ball-collision-with-racket?
                 (make-ball 100 37 3 4)
                 (make-racket 100 40 1 1 false -30 -30))
                true
                "Ball collision with racket"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-below-ball? : Ball Racket -> Boolean
;;; GIVEN   : the states of the ball and racket in rally state of the world,
;;; RETURNS : true iff the racket's position is below that of the ball.
;;; EXAMPLES:
;;;   (racket-below-ball? (make-ball 100 35 3 4)
;;;                       (make-racket 100 40 1 1 ??)) => true
;;;   (racket-below-ball? (make-ball 100 35 3 4)
;;;                       (make-racket 100 33 1 1 ??)) => false
;;; STRATEGY: Combine simpler functions.
(define (racket-below-ball? b r)
  (<= (ball-y b) (racket-y r)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Check whether the path of the ball collides with the racket.
;;; line-segments-intersect? : Integer Integer Integer Integer Integer
;;;                            Integer Integer -> Boolean
;;; GIVEN   : the old and the tentative coordinates of the ball and racket,
;;; RETURNS : true iff the path of the ball intersects the line-segment
;;;           that represents the racket.
;;; EXAMPLES:
;;;    (line-segments-intersect? 5 2 5 7 3 7 5) => true
;;;    (line-segments-intersect? 5 2 5 7 3 4 5) => false
;;; STRATEGY: Combine simpler functions.
(define (line-segments-intersect? x1 y1 x2 y2 x3 x4 y3)
  (and (<= 0 (x-intercept x1 y1 x2 y2 x3 x4 y3) 1)
       (<= 0 (y-intercept x1 y1 x2 y2 x3 x4 y3) 1)))



;;; x-intercept : Integer Integer Integer Integer Integer
;;;               Integer Integer -> Real     
;;; GIVEN   : the old and the tentative coordinates of the ball and racket,
;;; RETURNS : the x-intercept of the line-segment intersection formula.
;;; EXAMPLES: (x-intercept 5 2 5 7 3 7 5) => 0.6
;;; STRATEGY: Transcribe formula.
(define (x-intercept x1 y1 x2 y2 x3 x4 y3)
  (/ (- y1 y3) (- y1 y2)))


;;; y-intercept : Integer Integer Integer Integer Integer
;;;               Integer Integer -> Real   
;;; GIVEN   : the old and the tentative coordinates of the ball and racket,
;;; RETURNS : the y-intercept of the line-segment intersection formula.
;;; EXAMPLES: (y-intercept 5 2 5 7 3 7 5) => 0.5
;;; STRATEGY: Transcribe formula.
(define (y-intercept x1 y1 x2 y2 x3 x4 y3)
  (/ (- (* (- x1 x2) (- y1 y3))
        (* (- y2 y1) (- x1 x3)))
     (* (- y2 y1) (- x3 x4))))


;;; TESTS:
(begin-for-test
  (check-equal? (line-segments-intersect? 100 35 103 39 78 124 41)
                false
                "Ball and racket move towards each other, but miss by 2 px")
  (check-equal? (line-segments-intersect? 100 39 103 43 78 124 41)
                true
                "Ball and racket move towards each other, and intersect"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-tentative-left-x : Racket -> Real
;;; GIVEN   : a racket in any state of the world,
;;; RETURNS : the new x component of the racket's left edge.
;;; EXAMPLES:
;;;     (racket-tentative-left-x (make-racket 30 30 10 10)) => 16.5
;;; STRATEGY: Transcribe formula.
(define (racket-tentative-left-x r)
  (- (racket-tentative-x r) HALF-RACKET-LENGTH))


;;; racket-tentative-right-x : Racket -> Real
;;; GIVEN   : a racket in any state of the world,
;;; RETURNS : the new x component of the racket's right edge.
;;; EXAMPLES:
;;;     (racket-tentative-right-x (make-racket 30 30 10 10 ??)) => 63.5
;;; STRATEGY: Transcribe formula.
(define (racket-tentative-right-x r)
  (+ (racket-tentative-x r) HALF-RACKET-LENGTH))


;;; racket-tentative-top-y : Racket -> Real
;;; GIVEN   : a racket in any state of the world,
;;; RETURNS : the new y component of the racket's top edge.
;;; EXAMPLES:
;;;     (racket-tentative-top-y (make-racket 30 30 10 10 ??)) => 36.5
;;; STRATEGY: Transcribe formula.
(define (racket-tentative-top-y r)
  (- (racket-tentative-y r) HALF-RACKET-HEIGHT))


;;; racket-tentative-bottom-y : Racket -> Real
;;; GIVEN   : a racket in any state of the world,
;;; RETURNS : the new y component of the racket's bottom edge.
;;; EXAMPLES:
;;;     (racket-tentative-bottom-y (make-racket 30 30 10 10 ??)) => 43.5
;;; STRATEGY: Transcribe formula.
(define (racket-tentative-bottom-y r)
  (+ (racket-tentative-y r) HALF-RACKET-HEIGHT))


;;; ball-collision-with-right-wall? : Ball -> Boolean
;;; GIVEN   : a ball in the rally state of the world,
;;; RETURNS : true iff the tentative position of the ball collides with the
;;;          right wall of the court.
;;; EXAMPLES:
;;;    (ball-collision-with-right-wall? (make-ball 420 30 10 10)) => true
;;; STRATEGY: Transcribe formula.
(define (ball-collision-with-right-wall? b)
  (>= (ball-tentative-x b) COURT-WIDTH))

;;; TESTS:
(begin-for-test
  (check-equal? (ball-collision-with-right-wall? (make-ball 420 30 10 10))
                true "Ball collides with right wall => Expecting true")
  (check-equal? (ball-collision-with-right-wall? (make-ball 30 30 10 10))
                false "Ball doesn't collide with right wall => false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-collision-with-left-wall? : Ball -> Boolean
;;; GIVEN   : a ball in the rally state of the world,
;;; RETURNS : true iff the tentative position of the ball collides with the
;;;          left wall of the court.
;;; EXAMPLES:
;;;     (ball-collision-with-left-wall? (make-ball 9 30 -10 10)) => true
;;;     (ball-collision-with-left-wall? (make-ball 30 30 10 10)) => false
;;; STRATEGY: Transcribe formula.
(define (ball-collision-with-left-wall? b)
  (<= (ball-tentative-x b) COURT-LEFT-WALL-COORDINATE))

;;; TESTS:
(begin-for-test
  (check-equal? (ball-collision-with-left-wall? (make-ball 9 30 -10 10))
                true "Ball collides with left wall => Expecting true")
  (check-equal? (ball-collision-with-left-wall? (make-ball 30 30 10 10))
                false "Ball doesn't collide with left wall => false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-collision-with-front-wall? : Ball -> Boolean
;;; GIVEN   : a ball in the rally state of the world,
;;; RETURNS : true iff the tentative position of the ball collides with the
;;;          front wall of the court.
;;; EXAMPLES:
;;;     (ball-collision-with-front-wall? (make-ball 5 5 10 -10)) => true
;;;     (ball-collision-with-front-wall? (make-ball 30 30 10 10)) => false
;;; STRATEGY: Transcribe formula.
(define (ball-collision-with-front-wall? b)
  (<= (ball-tentative-y b) COURT-FRONT-WALL-COORDINATE))

;;; TESTS:
(begin-for-test
  (check-equal? (ball-collision-with-front-wall? (make-ball 5 5 10 -10))
                true "Ball collides with front wall => Expecting true")
  (check-equal? (ball-collision-with-front-wall? (make-ball 30 30 10 10))
                false "Ball doesn't collide with front wall => false"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; balls-collision-with-back-wall? : BallList -> Boolean
;;; GIVEN   : a list of balls in the rally state of the world,
;;; RETURNS : true iff atleast one of the balls tentatively collides
;;;           with the back wall of the court.
;;; EXAMPLES:
;;;     (balls-collision-with-back-wall? (list (make-ball 9 30 -10 10)
;;;                                            (make-ball 50 648 10 2))
;;;                   => true
;;; OLD STRATEGY: Use observer template on BallList.
#;(define (balls-collision-with-back-wall? bl)
    (cond [(empty? bl) false]
          [else (if (ball-collision-with-back-wall? (first bl))
                    true
                    (balls-collision-with-back-wall? (rest bl)))]))

;;; NEW STRATEGY: Use HOF ormap on bl.
(define (balls-collision-with-back-wall? bl)
  (ormap ball-collision-with-back-wall? bl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-collision-with-back-wall? : Ball -> Boolean
;;; GIVEN   : a ball in the rally state of the world,
;;; RETURNS : true iff the tentative position of the ball collides with the
;;;          back wall of the court.
;;; EXAMPLES:
;;;     (ball-collision-with-back-wall? (make-ball 50 648 10 2)) => true
;;;     (ball-collision-with-back-wall? (make-ball 30 30 10 10)) => false
;;; STRATEGY: Transcribe formula.
(define (ball-collision-with-back-wall? b)
  (>= (ball-tentative-y b) COURT-HEIGHT))

;;; TESTS:
(begin-for-test
  (check-equal? (ball-collision-with-back-wall? (make-ball 50 648 10 2))
                true "Ball collides with back wall => Expecting true")
  (check-equal? (ball-collision-with-back-wall? (make-ball 30 30 10 10))
                false "Ball doesn't collide with back wall => false"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-post-racket-collision : Ball Racket -> Ball
;;; GIVEN   : a ball and a racket that are about to collide in the
;;;        rally state of the world,
;;; RETURNS : the new state of the ball post collision.
;;; EXAMPLES: (ball-post-racket-collision (make-ball 370 66 -3 9)
;;;                                  (make-racket 370 80 -3 9 false -30 -30))
;;;             =>    (make-ball 370 80 -3 -12)
;;; STRATEGY: Use constructor template for Ball.
(define (ball-post-racket-collision b r)
  (make-ball (ball-x b)
             (racket-y r)
             (ball-vx b)
             (- (ball-vx b) (ball-vy b))))

;;; TESTS:
(begin-for-test
  (check-equal? (ball-post-racket-collision
                 (make-ball 370 66 -3 9)
                 (make-racket 370 80 -3 9 false -30 -30))
                (make-ball 370 80 -3 -12)
                "Post collision racket-vy component should be -12")
  (check-equal? (ball-post-racket-collision
                 (make-ball 220 474 -3 9)
                 (make-racket 220 474 -3 9 false -30 -30))
                (make-ball 220 474 -3 -12)
                "Moving away post collision"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; additive-inverse : Real -> Real
;;; GIVEN   : any real number,
;;; RETURNS : the additive inverse of the number.
;;; EXAMPLES:
;;;          (additive-inverse 89) => -89
;;;          (additive-inverse -2) => 2
;;; STRATEGY: Transcribe formula.
(define (additive-inverse n)
  (- 0 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-post-right-wall-collision : Ball -> Ball
;;; GIVEN   : a ball thats about to collide with the right wall of the court,
;;; RETURNS : the new state of the ball post collision.
;;; EXAMPLES:
;;;      (ball-post-right-wall-collision (make-ball 420 30 10 10))
;;;       =>    (make-ball 420 40 -10 10)
;;; STRATEGY: Use constructor template for Ball.
(define (ball-post-right-wall-collision b)
  (make-ball (adjusted-ball-x-with-right-wall b)
             (ball-tentative-y b)
             (additive-inverse (ball-vx b))
             (ball-vy b)))
;;; TESTS:
(begin-for-test
  (check-equal? (ball-post-right-wall-collision (make-ball 420 30 10 10))
                (make-ball 420 40 -10 10)
                "Ball collides with right wall => Expecting new state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; adjusted-ball-x-with-right-wall : Ball -> Integer
;;; GIVEN   : a ball thats about to collide with the right wall of the court,
;;; RETURNS : the new x coordinate of the ball post collision.
;;; EXAMPLES:
;;;    (adjusted-ball-x-with-right-wall (make-ball 420 30 7 10)) => 423
;;; STRATEGY: Transcribe formula.
(define (adjusted-ball-x-with-right-wall b)
  (- COURT-WIDTH (- (ball-tentative-x b) COURT-WIDTH)))

;;; TESTS:
(begin-for-test
  (check-equal? (adjusted-ball-x-with-right-wall (make-ball 420 30 7 10))
                423 "Offset post collision with right wall is 423"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-post-left-wall-collision : Ball -> Ball
;;; GIVEN   : a ball thats about to collide with the left wall of the court,
;;; RETURNS : the new state of the ball post collision.
;;; EXAMPLES:
;;;    (ball-post-left-wall-collision (make-ball 9 30 -10 10))
;;;        =>      (make-ball 1 40 10 10)
;;; STRATEGY: Use constructor template for Ball.
(define (ball-post-left-wall-collision b)
  (make-ball (additive-inverse (ball-tentative-x b))
             (ball-tentative-y b)
             (additive-inverse (ball-vx b))
             (ball-vy b)))

;;; TESTS:
(begin-for-test
  (check-equal? (ball-post-left-wall-collision (make-ball 9 30 -10 10))
                (make-ball 1 40 10 10)
                "Ball collides with left wall => Expecting new state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-post-front-wall-collision : Ball -> Ball
;;; GIVEN   : a ball thats about to collide with the front wall of the court,
;;; RETURNS : the new state of the ball post collision.
;;; EXAMPLES:
;;;       (ball-post-front-wall-collision (make-ball 5 5 10 -7))
;;;          =>      (make-ball 15 2 10 7)
;;; STRATEGY: Use constructor template for Ball.
(define (ball-post-front-wall-collision b)
  (make-ball (ball-tentative-x b)
             (additive-inverse (ball-tentative-y b))
             (ball-vx b)
             (additive-inverse (ball-vy b))))

;;; TESTS:
(begin-for-test
  (check-equal? (ball-post-front-wall-collision (make-ball 5 5 10 -7))
                (make-ball 15 2 10 7)
                "Ball collides with left wall => Expecting new state"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ball-without-collision : Ball -> Ball
;;; GIVEN   : a ball in the rally state of the world whose tentative position
;;;           does not collide with the racket of the walls of the court,
;;; RETURNS : the new state of the ball.
;;; EXAMPLES:
;;;   (ball-without-collision (make-ball 15 2 10 7)) => (make-ball 25 9 10 7)
;;; STRATEGY: Use constructor template for Ball.
(define (ball-without-collision b)
  (make-ball (ball-tentative-x b)
             (ball-tentative-y b)
             (ball-vx b)
             (ball-vy b)))

;;; TESTS:
(begin-for-test
  (check-equal? (ball-without-collision (make-ball 15 2 10 7))
                (make-ball 25 9 10 7)
                "ball Without collision => Expecting new state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-after-tick : World -> Racket
;;; GIVEN   : the state of the world,
;;; RETURNS : the state of the racket in the world after a tick.
;;; EXAMPLES:
;;;      (racket-after-tick (make-world
;;;                                (list (make-ball 30 30 2 2 ))
;;;                                (make-racket 10 50 2 2 false -30 -30)
;;;                                RALLY-STATE 10 (/ 1 24))))
;;;            =>    (make-racket 23.5 52 2 2 false -30 -30)
;;; STRATEGY: Cases on whether the racket collides with the ball or
;;;           any of the court's walls or if it is selected.
(define (racket-after-tick w)
  (cond [(balls-collision-with-racket? (world-balls w) (world-racket w))
         (racket-post-ball-collision (world-racket w))]
        [(racket-selected? (world-racket w)) (world-racket w)]
        [(racket-collision-with-back-wall? (world-racket w))
         (racket-post-bottom-walls-collision (world-racket w))]
        [(racket-collision-with-right-wall? (world-racket w))
         (racket-post-right-wall-collision (world-racket w))]
        [(racket-collision-with-left-wall? (world-racket w))
         (racket-post-left-wall-collision (world-racket w))]
        [else (racket-without-collision (world-racket w))]))

;;; TESTS:
(define WORLD-RACKET-BACK-WALL (make-world
                                (list (make-ball 30 30 2 2 ))
                                (make-racket 50 648 2 2 false -30 -30)
                                RALLY-STATE 10 (/ 1 24)))
(define WORLD-RACKET-RIGHT-WALL (make-world
                                 (list (make-ball 30 30 2 2 ))
                                 (make-racket 420 30 2 2 false -30 -30)
                                 RALLY-STATE 10 (/ 1 24)))
(define WORLD-RACKET-LEFT-WALL (make-world
                                (list (make-ball 30 30 2 2 ))
                                (make-racket 10 50 2 2 false -30 -30)
                                RALLY-STATE 10 (/ 1 24)))

(begin-for-test
  (check-equal? (racket-x (racket-after-tick
                           WORLD-RACKET-LEFT-WALL))
                23.5
                "Racket x gets adjusted on left wall impact")
  (check-equal? (racket-y (racket-after-tick
                           WORLD-RACKET-BACK-WALL))
                (- COURT-HEIGHT HALF-RACKET-HEIGHT)
                "Racket y gets adjusted on back wall impact")
  (check-equal? (racket-x (racket-after-tick
                           WORLD-RACKET-RIGHT-WALL))
                401.5
                "Racket x gets adjusted on right wall impact")
  (check-equal? (racket-after-tick
                 (make-world (list ball-at-330)
                             selected-racket-at-100-100
                             "rally"
                             0
                             (/ 1 24)))
                selected-racket-at-100-100
                "Selected Racket returns unchanged"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-collision-with-right-wall? : Racket -> Boolean
;;; GIVEN   : a racket in the rally state of world,
;;; RETURNS : true iff the racket collides with the right wall of the court.
;;; EXAMPLES: (racket-collision-with-right-wall?
;;;                 (make-racket 420 12 10 7 false -30 -30))  => true
;;; STRATEGY: Combine simpler functions
(define (racket-collision-with-right-wall? r)
  (>= (racket-tentative-right-x r) COURT-WIDTH))

;;; TESTS:
(begin-for-test
  (check-equal? (racket-collision-with-right-wall?
                 (make-racket 420 12 10 7 false -30 -30))
                true "Racket collides with right wall => Expecting true")
  (check-equal? (racket-collision-with-right-wall?
                 (make-racket 20 12 10 7 false -30 -30))
                false "Racket collides with right wall => Expecting false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-collision-with-left-wall? : Racket -> Boolean
;;; GIVEN   : a racket in the rally state of world,
;;; RETURNS : true iff the racket collides with the left wall of the court.
;;; EXAMPLES: (racket-collision-with-left-wall?
;;;               (make-racket 20 12 -10 7 false -30 -30))  => true
;;; STRATEGY: Combine simpler functions
(define (racket-collision-with-left-wall? r)
  (<= (racket-tentative-left-x r) COURT-LEFT-WALL-COORDINATE))

;;; TESTS:
(begin-for-test
  (check-equal? (racket-collision-with-left-wall?
                 (make-racket 20 12 -10 7 false -30 -30))
                true "Racket collides with left wall => Expecting true")
  (check-equal? (racket-collision-with-left-wall?
                 (make-racket 200 12 -10 7 false -30 -30))
                false "Racket collides with left wall => Expecting false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-collision-with-front-wall? : Racket -> Boolean
;;; GIVEN   : a racket in the rally state of world,
;;; RETURNS : true iff the racket collides with the front wall of the court.
;;; EXAMPLES: (racket-collision-with-front-wall?
;;;               (make-racket 40 8 -10 -7 false -30 -30)) => true
;;; STRATEGY: Combine simpler functions
(define (racket-collision-with-front-wall? r)
  (<= (racket-tentative-top-y r) COURT-FRONT-WALL-COORDINATE))

;;; TESTS:
(begin-for-test
  (check-equal? (racket-collision-with-front-wall?
                 (make-racket 40 8 -10 -7 false -30 -30))
                true "Racket collides with front wall => Expecting true")
  (check-equal? (racket-collision-with-front-wall?
                 (make-racket 20 12 -10 7 false -30 -30))
                false "Racket collides with front wall => Expecting false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-collision-with-back-wall? : Racket -> Boolean
;;; GIVEN   : a racket in the rally state of world,
;;; RETURNS : true iff the racket collides with the back wall of the court.
;;; EXAMPLES: (racket-collision-with-back-wall?
;;;                 (make-racket 50 648 10 7 false -30 -30))  => true
;;; STRATEGY: Combine simpler functions
(define (racket-collision-with-back-wall? r)
  (>= (racket-tentative-bottom-y r) (- COURT-HEIGHT HALF-RACKET-HEIGHT)))

;;; TESTS:
(begin-for-test
  (check-equal? (racket-collision-with-back-wall?
                 (make-racket 50 648 10 7 false -30 -30))
                true "Racket collides with back wall => Expecting true")
  (check-equal? (racket-collision-with-back-wall?
                 (make-racket 50 12 -10 7 false -30 -30))
                false "Racket collides with back wall => Expecting false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-post-ball-collision : Racket -> Racket
;;; GIVEN   : a racket in the rally state of world thats about to collide
;;;        with a ball in the world,
;;; RETURNS : the given racket's state post collision with the ball.
;;; EXAMPLES:
;;;  (racket-post-ball-collision (make-racket 50 648 10 -7 false -30 -30))
;;;         => (make-racket 60 641 10 0 #false -30 -30)
;;; STRATEGY: Use constructor template for Racket.
(define (racket-post-ball-collision r)
  (make-racket (racket-tentative-x r)
               (racket-tentative-y r)
               (racket-vx r)
               (adjust-racket-vy-on-ball-collision r)
               (racket-selected? r)
               (racket-mouse-x r)
               (racket-mouse-y r)))

;;; TESTS:
(begin-for-test
  (check-equal? (racket-vy (racket-post-ball-collision
                            (make-racket 50 648 10 -7 false -30 -30)))
                0 "Racket's vy component becomes 0 if -ve on ball impact"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; adjust-racket-vy-on-ball-collision : Racket -> Integer
;;; GIVEN   : a racket in the rally state of world thats about to collide
;;;        with a ball in the world,
;;; RETURNS : the tentative vy component of the racket post tick.
;;; EXAMPLES:
;;;      (adjust-racket-vy-on-ball-collision
;;;          (make-racket 50 10 10 -7 false -30 -30)) => 0
;;; STRATEGY: Cases on whether the vy component is negative.
(define (adjust-racket-vy-on-ball-collision r)
  (if (< (racket-vy r) 0)
      0
      (racket-vy r)))

;;; TESTS:
(define racket-approaching-ball (make-racket 330 645 0 -4 false -30 -30))
(begin-for-test
  (check-equal? (adjust-racket-vy-on-ball-collision
                 (make-racket 50 648 10 7 false -30 -30))
                7 "Racket's vy component - No impact")
  (check-equal? (adjust-racket-vy-on-ball-collision
                 (make-racket 50 10 10 -7 false -30 -30))
                0 "Racket's vy component - impact")
  (check-equal? (adjust-racket-vy-on-ball-collision
                 racket-approaching-ball)
                0 "Racket's vy on collision with ball becomes 0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-post-right-wall-collision : Racket -> Racket
;;; GIVEN   : a racket in the rally state of world thats about to collide
;;;        with the right wall of the court,
;;; RETURNS : the given racket's state post collision with the given wall.
;;; EXAMPLES: (racket-post-right-wall-collision
;;;                 (make-racket 420 12 10 7 false -30 -30))
;;;                => (make-racket 401.5 19 10 7 false -30 -30)
;;; STRATEGY: Use constructor template for Racket.
(define (racket-post-right-wall-collision r)
  (make-racket RACKET-X-WITH-RIGHT-WALL
               (racket-tentative-y r)
               (racket-vx r)
               (racket-vy r)
               (racket-selected? r)
               (racket-mouse-x r)
               (racket-mouse-y r)))

;;; TESTS:
(begin-for-test
  (check-equal? (racket-post-right-wall-collision
                 (make-racket 420 12 10 7 false -30 -30))
                (make-racket 401.5 19 10 7 false -30 -30)
                "Racket collides with right wall => Expecting state change"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-post-left-wall-collision : Racket -> Racket
;;; GIVEN   : a racket in the rally state of world thats about to collide
;;;           with the left wall of the court,
;;; RETURNS : the given racket's state post collision with the given wall.
;;; EXAMPLES:
;;;        (racket-post-left-wall-collision
;;;                 (make-racket 20 12 -10 7 false -30 -30))
;;;          =>        (make-racket 23.5 19 -10 7 false -30 -30)
;;; STRATEGY: Use constructor template for Racket.
(define (racket-post-left-wall-collision r)
  (make-racket RACKET-X-WITH-LEFT-WALL
               (racket-tentative-y r)
               (racket-vx r)
               (racket-vy r)
               (racket-selected? r)
               (racket-mouse-x r)
               (racket-mouse-y r)))

;;; TESTS:
(begin-for-test
  (check-equal? (racket-post-left-wall-collision
                 (make-racket 20 12 -10 7 false -30 -30))
                (make-racket 23.5 19 -10 7 false -30 -30)
                "Racket collides with left wall => Expecting state change"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-post-bottom-walls-collision : Racket -> Racket
;;; GIVEN   : a racket in the rally state of world thats about to collide
;;;           with the back or side walls of the court,
;;; RETURNS : the given racket's state post collision with the given walls.
;;; EXAMPLES: (racket-x (racket-post-bottom-walls-collision
;;;                    RACKET-RIGHT-BACK-WALL))  => Racket center adjusted
;;; STRATEGY: Cases on whether racket collides with left and right walls
;;;           of the court along with the back wall.
(define (racket-post-bottom-walls-collision r)
  (cond  [(racket-collision-with-left-wall? r)
          (racket-post-left-wall-collision
           (racket-post-back-wall-collision r))]
         [(racket-collision-with-right-wall? r)
          (racket-post-right-wall-collision
           (racket-post-back-wall-collision r))]
         [else (racket-post-back-wall-collision r)]))

;;; TESTS:
(define RACKET-LEFT-BACK-WALL (make-racket 22 645 -3 4 false -30 -30))
(define RACKET-RIGHT-BACK-WALL (make-racket 410 645 12 4 false -30 -30))
                                        
(begin-for-test
  (check-equal? (racket-x (racket-post-bottom-walls-collision
                           RACKET-LEFT-BACK-WALL))
                23.5
                "Racket's vx gets adjusted to fit left wall")
  (check-equal? (racket-x (racket-post-bottom-walls-collision
                           RACKET-RIGHT-BACK-WALL))
                401.5
                "Racket's vx gets adjusted to fit right wall"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-post-back-wall-collision : Racket -> Racket
;;; GIVEN   : a racket in the rally state of world thats about to collide
;;;        with the back wall of the court,
;;; RETURNS : the given racket's state post collision with the given wall.
;;; EXAMPLES:
;;;  (racket-post-back-wall-collision (make-racket 50 648 10 7 false -30 -30))
;;;            =>    (make-racket 60 645.5 10 0 false -30 -30)
;;; STRATEGY: Use constructor template for Racket.
(define (racket-post-back-wall-collision r)
  (make-racket (racket-tentative-x r)
               RACKET-Y-WITH-BOTTOM-WALL
               (racket-vx r)
               INIT-VELOCITY
               (racket-selected? r)
               (racket-mouse-x r)
               (racket-mouse-y r)))

;;; TESTS:
(begin-for-test
  (check-equal? (racket-post-back-wall-collision
                 (make-racket 50 648 10 7 false -30 -30))
                (make-racket 60 645.5 10 0 false -30 -30)
                "Racket collides with back wall => Expecting state change"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-without-collision : Racket -> Racket
;;; GIVEN   : a racket in the rally state of the world whose tentative new
;;;        position does not collide with the ball or the walls
;;;        of the court,
;;; RETURNS : the new state of the racket.
;;; EXAMPLES:
;;;  (racket-without-collision (make-racket 50 50 10 7 false -30 -30))
;;;       =>      (make-racket 60 57 10 7 false -30 -30)
;;; STRATEGY: Use constructor template for Racket.
(define (racket-without-collision r)
  (make-racket (racket-tentative-x r)
               (racket-tentative-y r)
               (racket-vx r)
               (racket-vy r)
               (racket-selected? r)
               (racket-mouse-x r)
               (racket-mouse-y r)))
;;; TESTS:
(begin-for-test
  (check-equal? (racket-without-collision
                 (make-racket 50 50 10 7 false -30 -30))
                (make-racket 60 57 10 7 false -30 -30)
                "Expecting state change with tentative x and y components"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; WORLD AFTER KEY:
;;; world-after-key-event : World SimulationKeyEvent -> World
;;; GIVEN   : a world and a key event
;;; RETURNS : the world that should follow the given world
;;;     after the given key event.
;;; EXAMPLES: (world-rally-state? (world-after-key-event
;;;                                  RALLY-STATE-WORLD SPACE-KEY))
;;;                    => PAUSE-STATE-WORLD
;;; STRATEGY: Use observer template for StateName.
(define (world-after-key-event w kev)
  (cond [(world-ready-to-serve? w) (ready-state-after-key-event w kev)]
        [(world-pause-state? w) w]
        [(world-rally-state? w) (rally-state-after-key-event w kev)]))

;;; TESTS:
(define RALLY-STATE-WORLD (make-world (list (make-ball 30 30 30 30))
                                      (make-racket 30 30 30 30 false -30 -30)
                                      RALLY-STATE 10 1))
(define READY-STATE-WORLD (make-world (list (make-ball 30 30 30 30))
                                      (make-racket 30 30 30 30 false -30 -30)
                                      READY-STATE 10 1))
(begin-for-test
  (check-equal? (world-rally-state? (world-after-key-event
                                     PAUSE-STATE-WORLD " "))
                false
                "Expecting pause state even after pause")
  (check-equal? (world-rally-state? (world-after-key-event
                                     RALLY-STATE-WORLD RIGHT-KEY))
                true
                "Expecting rally state event during right key event")
  (check-equal? (world-rally-state? (world-after-key-event
                                     INITIAL-WORLD-24 SPACE-KEY))
                true
                "Expecting rally state event after space key event"))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rally-state-after-key-event : World SimulationKeyEvent -> World
;;; GIVEN   : a world thats in rally state and a key event
;;; RETURNS : the world that should follow the given world
;;;     after the given key event.
;;; EXAMPLES: (length (world-balls
;;;               (world-after-key-event RALLY-STATE-WORLD B-KEY))) => 2
;;; STRATEGY: Use observer template for SimulationKeyEvent.
(define (rally-state-after-key-event w kev)
  (cond [(key=? kev LEFT-KEY)
         (rally-after-left-key-event w)]
        [(key=? kev RIGHT-KEY)
         (rally-after-right-key-event w)]
        [(key=? kev UP-KEY)
         (rally-after-up-key-event w)]
        [(key=? kev DOWN-KEY)
         (rally-after-down-key-event w)]
        [(key=? kev B-KEY)
         (rally-after-b-key-event w)]
        [(key=? kev SPACE-KEY)
         (pause-world w)]
        [else w]))

;;; TESTS:
(define NON-KEY "q")

(begin-for-test
  (check-equal? (- (racket-vx (world-racket RALLY-STATE-WORLD))
                   (racket-vx (world-racket (world-after-key-event
                                             RALLY-STATE-WORLD LEFT-KEY))))
                1
                "Expecting 1 shift to left")
  (check-equal? (- (racket-vy (world-racket RALLY-STATE-WORLD))
                   (racket-vy (world-racket (world-after-key-event
                                             RALLY-STATE-WORLD UP-KEY))))
                1
                "Expecting 1 shift to up")
  (check-equal? (- (racket-vy (world-racket RALLY-STATE-WORLD))
                   (racket-vy (world-racket (world-after-key-event
                                             RALLY-STATE-WORLD DOWN-KEY))))
                -1
                "Expecting 1 shift to down")
  (check-equal? (world-pause-state? (world-after-key-event
                                     RALLY-STATE-WORLD SPACE-KEY))
                true
                "Expecting pause state on space key event")
  (check-equal? (length (world-balls (world-after-key-event
                                      RALLY-STATE-WORLD B-KEY)))
                2
                "Expecting number of balls to be 2 after b-key event")
  (check-equal? (- (racket-vx (world-racket RALLY-STATE-WORLD))
                   (racket-vx (world-racket (world-after-key-event
                                             RALLY-STATE-WORLD NON-KEY))))
                0
                "Expecting 0 shift to any direction"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rally-after-left-key-event : World -> World
;;; GIVEN   : a world thats in rally state,
;;; RETURNS : the world that should follow the given world
;;;          after the left key event.
;;; EXAMPLE : 
;;;    (rally-after-left-key-event (make-world (list (make-ball 50 50 50 50))
;;;                                            (make-racket 50 50 50 50)
;;;                                            RALLY-STATE 10 1))
;;;              =>                (make-world (list (make-ball 50 50 50 50))
;;;                                            (make-racket 50 50 49 50)
;;;                                            RALLY-STATE 10 1))
;;; STRATEGY: Use constructor template for World.
(define (rally-after-left-key-event w)
  (make-world (world-balls w)
              (racket-after-left-key-event (world-racket w))
              (world-state w)
              (world-ticks-since-state w)
              (world-speed w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-after-left-key-event : Racket -> Racket
;;; GIVEN   : a racket in the world thats in rally state,
;;; RETURNS : the new state of the racket post the left key event.
;;; EXAMPLE : 
;;;    (racket-after-left-key-event (make-racket 50 50 50 50))
;;;              =>  (make-racket 50 50 49 50)
;;; STRATEGY: Use constructor template for Racket.
(define (racket-after-left-key-event r)
  (make-racket (racket-x r)
               (racket-y r)
               (sub1 (racket-vx r))
               (racket-vy r)
               (racket-selected? r)
               (racket-mouse-x r)
               (racket-mouse-y r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rally-after-right-key-event : World -> World
;;; GIVEN   : a world thats in rally state,
;;; RETURNS : the world that should follow the given world
;;;          after the right key event.
;;; EXAMPLE : 
;;;      (rally-after-right-key-event
;;;       (make-world (list (make-ball 50 50 50 50))
;;;                   (make-racket 50 50 50 50)
;;;                   RALLY-STATE 10 1))
;;;          =>  (make-world (list (make-ball 50 50 50 50))
;;;                          (make-racket 50 50 51 50)
;;;                          RALLY-STATE 10 1)
;;; STRATEGY: Use constructor template for World.
(define (rally-after-right-key-event w)
  (make-world (world-balls w)
              (racket-after-right-key-event (world-racket w))
              (world-state w)
              (world-ticks-since-state w)
              (world-speed w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-after-right-key-event : Racket -> Racket
;;; GIVEN   : a racket in the world thats in rally state,
;;; RETURNS : the new state of the racket post the right key event.
;;; EXAMPLE :
;;;    (racket-after-right-key-event (make-racket 50 50 50 50))
;;;              =>  (make-racket 50 50 51 50)
;;; STRATEGY: Use constructor template for Racket.
(define (racket-after-right-key-event r)
  (make-racket (racket-x r)
               (racket-y r)
               (add1 (racket-vx r))
               (racket-vy r)
               (racket-selected? r)
               (racket-mouse-x r)
               (racket-mouse-y r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rally-after-up-key-event : World -> World
;;; GIVEN   : a world thats in rally state,
;;; RETURNS : the world that should follow the given world
;;;           after the up key event.
;;; EXAMPLE : 
;;;    (rally-after-up-key-event
;;;     (make-world (list (make-ball 50 50 50 50))
;;;                 (make-racket 50 50 50 50)
;;;                 RALLY-STATE 10 1))
;;;     =>   (make-world (list (make-ball 50 50 50 50))
;;;                      (make-racket 50 50 50 49)
;;;                      RALLY-STATE 10 1)
;;; STRATEGY: Use constructor template for World.
(define (rally-after-up-key-event w)
  (make-world (world-balls w)
              (racket-after-up-key-event (world-racket w))
              (world-state w)
              (world-ticks-since-state w)
              (world-speed w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-after-up-key-event : Racket -> Racket
;;; GIVEN   : a racket in the world thats in rally state,
;;; RETURNS : the new state of the racket post the up key event.
;;; EXAMPLE : 
;;;    (racket-after-up-key-event (make-racket 50 50 50 50))
;;;              =>  (make-racket 50 50 50 49)
;;; STRATEGY: Use constructor template for Racket.
(define (racket-after-up-key-event r)
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               (sub1 (racket-vy r))
               (racket-selected? r)
               (racket-mouse-x r)
               (racket-mouse-y r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rally-after-down-key-event : World -> World
;;; GIVEN   : a world thats in rally state,
;;; RETURNS : the world that should follow the given world
;;;          after the down key event.
;;; EXAMPLE : 
;;;     (rally-after-down-key-event
;;;      (make-world (list (make-ball 50 50 50 50))
;;;                  (make-racket 50 50 50 50)
;;;                  RALLY-STATE 10 1))
;;;       =>  (make-world (list (make-ball 50 50 50 50))
;;;                       (make-racket 50 50 50 51)
;;;                       RALLY-STATE 10 1)
;;; STRATEGY: Use constructor template for World.
(define (rally-after-down-key-event w)
  (make-world (world-balls w)
              (racket-after-down-key-event (world-racket w))
              (world-state w)
              (world-ticks-since-state w)
              (world-speed w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-after-down-key-event : Racket -> Racket
;;; GIVEN   : a racket in the world thats in rally state,
;;; RETURNS : the new state of the racket post the down key event.
;;; EXAMPLE : 
;;;    (racket-after-down-key-event (make-racket 50 50 50 50))
;;;              =>  (make-racket 50 50 50 51)
;;; STRATEGY: Use constructor template for Racket.
(define (racket-after-down-key-event r)
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               (add1 (racket-vy r))
               (racket-selected? r)
               (racket-mouse-x r)
               (racket-mouse-y r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rally-after-b-key-event : World -> World
;;; GIVEN   : a world thats in rally state,
;;; RETURNS : the world that should follow the given world
;;;           after the b key event.
;;; EXAMPLE : 
;;;     (rally-after-b-key-event
;;;      (make-world (list (make-ball 50 50 50 50))
;;;                  (make-racket 50 50 50 50)
;;;                  RALLY-STATE 10 1))
;;;       =>  (make-world (list (make-ball 50 50 50 50)
;;;                             (make-ball 330 384 3 -9))
;;;                       (make-racket 50 50 50 50)
;;;                       RALLY-STATE 10 1)
;;; STRATEGY: Use constructor template for World.
(define (rally-after-b-key-event w)
  (make-world (balls-after-b-key-event (world-balls w))
              (world-racket w)
              (world-state w)
              (world-ticks-since-state w)
              (world-speed w)))

;;; TESTS:
(begin-for-test
  (check-equal? (rally-after-b-key-event
                 (make-world (list ball-all-at-30)
                             unselected-racket-at-100-100
                             RALLY-STATE 1 1))
                (make-world (list ball-all-at-30 BALL-AT-INIT-POSITION)
                            unselected-racket-at-100-100
                            RALLY-STATE 1 1)
                "Spawning of a new ball at initial position expected")
  (check-equal? (rally-after-b-key-event
                 (rally-after-b-key-event
                  (make-world (list ball-all-at-30)
                              unselected-racket-at-100-100
                              RALLY-STATE 1 1)))
                (make-world (list ball-all-at-30
                                  BALL-AT-INIT-POSITION
                                  BALL-AT-INIT-POSITION)
                            unselected-racket-at-100-100
                            RALLY-STATE 1 1)
                "Spawning of multiple new balls at initial position"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; balls-after-b-key-event : BallList -> BallList
;;; GIVEN   : a list of balls that are in the rally state of the world,
;;; RETURNS : a list with a new ball at initial position added to it.
;;; EXAMPLE : 
;;;     (balls-after-b-key-event (list (make-ball 50 50 50 50)))
;;;       =>  (list (make-ball 50 50 50 50) (make-ball 330 384 3 -9))
;;; STRATEGY: Use constructor template for BallList.
(define (balls-after-b-key-event bl)
  (cons BALL-AT-INIT-POSITION bl))

  
;;; TESTS:
(begin-for-test
  (check-equal? (balls-after-b-key-event (list ball-all-at-30))
                (list ball-all-at-30 BALL-AT-INIT-POSITION)
                "Expects a new ball at initial position added")
  (check-equal? (balls-after-b-key-event empty)
                (list BALL-AT-INIT-POSITION)
                "Expects a new ball at initial position added to empty list"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ready-state-after-key-event : World SimulationKeyEvent -> World
;;; GIVEN   : a world thats in ready state and a key event,
;;; RETURNS : the new state of the world post the key event.
;;; EXAMPLES:
;;;     (world-rally-state?(ready-state-after-key-event
;;;                         READY-STATE-WORLD SPACE-KEY))  => true
;;; STRATEGY: Cases on whether the given key event is space key event and
;;;           then use constructor template for World.
(define (ready-state-after-key-event w kev)
  (if (key=? kev " ") 
      (make-world (ready-balls-after-space-key-event (world-balls w))
                  (world-racket w)
                  RALLY-STATE
                  (world-ticks-since-state w)
                  (world-speed w))
      w))

;;; TESTS:
(begin-for-test
  (check-equal? (world-rally-state?(ready-state-after-key-event
                                    READY-STATE-WORLD NON-KEY))
                false
                "Expecting Ready state to respond only on Space Key"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ready-balls-after-space-key-event : BallList -> BallList
;;; GIVEN   : a list of balls in the world thats in ready state,
;;; RETURNS : the list of balls post the space key event.
;;; EXAMPLES: (ready-ball-after-space-key-event (list (make-ball 10 10 0 0)))
;;;               => (list (make-ball 10 10 3 -9))
;;; OLD STRATEGY: Use observer template for BallList.
#;(define (ready-balls-after-space-key-event bl)
    (cond [(empty? bl) empty]
          [else (cons (ready-ball-after-space-key-event (first bl))
                      (ready-balls-after-space-key-event (rest bl)))]))

;;; NEW STRATEGY: Use HOF map on bl.
(define (ready-balls-after-space-key-event bl)
  (map ready-ball-after-space-key-event bl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ready-ball-after-space-key-event : Ball -> Ball
;;; GIVEN   : a ball in the world thats in ready state,
;;; RETURNS : the new state of the ball post the space key event.
;;; EXAMPLES: (ready-ball-after-space-key-event (make-ball 10 10 0 0))
;;;               => (make-ball 10 10 3 -9)
;;; STRATEGY: Use constructor template for Ball.
(define (ready-ball-after-space-key-event b)
  (make-ball (ball-x b)
             (ball-y b)
             INIT-RALLY-VELOCITY-X
             INIT-RALLY-VELOCITY-Y))

;;; TESTS:
(begin-for-test
  (check-equal? (ready-ball-after-space-key-event (make-ball 10 10 0 0))
                (make-ball 10 10 3 -9)
                "First coordinates of the ball after ready-rally"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; WORLD AFTER MOUSE EVENT:
;;; world-after-mouse-event : World Int Int MouseEvent -> World
;;; GIVEN   : a world, the x and y coordinates of a mouse event,
;;;           and the mouse event
;;; RETURNS : the world that should follow the given world after
;;;           the given mouse event
;;; EXAMPLES:
;;;   (world-after-mouse-event (make-world
;;;                           (list ball-all-at-30)
;;;                           selected-racket-at-100-100
;;;                           RALLY-STATE 1 1)
;;;                          400 400
;;;                          "button-up")
;;; => (make-world (list ball-all-at-30)
;;;             unselected-racket-at-100-100
;;;             RALLY-STATE 1 1)
;;; STRATEGY: Use observer template for StateName.
(define (world-after-mouse-event w mx my mev)
  (cond [(world-ready-to-serve? w) w]
        [(world-pause-state? w) w]
        [(world-rally-state? w) (rally-state-after-mouse-event w mx my mev)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rally-state-after-mouse-event : World Integer Integer MouseEvent -> World
;;; GIVEN   : a world in rally state and a description of a mouse event,
;;; RETURNS : the world that should follow the given mouse event
;;; EXAMPLES:
;;;   (rally-state-after-mouse-event (make-world
;;;                           (list ball-all-at-30)
;;;                           selected-racket-at-100-100
;;;                           RALLY-STATE 1 1)
;;;                          400 400
;;;                          "button-up")
;;; => (make-world (list ball-all-at-30)
;;;             unselected-racket-at-100-100
;;;             RALLY-STATE 1 1)
;;; STRATEGY: Use constructor template for World.
(define (rally-state-after-mouse-event w mx my mev)
  (make-world (world-balls w)
              (racket-after-mouse-event (world-racket w) mx my mev)
              (world-state w)
              (world-ticks-since-state w)
              (world-speed w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-after-mouse-event : Racket Int Int MouseEvent -> Racket
;;; GIVEN   : a racket, the x and y coordinates of a mouse event,
;;;           and the mouse event
;;; RETURNS : the racket as it should be after the given mouse event
;;; EXAMPLES:
;;;     (racket-after-mouse-event (make-racket 30 30 31 30
;;;                                            true -30 -30) 10 10 "drag")
;;;      => (make-racket 70 70 31 30 #true 10 10)
;;; STRATEGY: Cases on mouse event mev
(define (racket-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (racket-after-button-down r mx my)]
    [(mouse=? mev "drag") (racket-after-drag r mx my)]
    [(mouse=? mev "button-up") (racket-after-button-up r mx my)]
    [else r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-after-button-down : Racket Integer Integer -> Racket
;;; GIVEN   : the racket and coordinates of the mouse event,
;;; RETURNS : the racket following a button-down at the given location.
;;; EXAMPLES:
;;;   (racket-after-mouse-event (make-racket 30 30 31 30
;;;                                      false -30 -30) 10 10 "button-down")
;;;      => (make-racket 30 30 31 30 #true -30 -30)
;;; STRATEGY: Cases on selection within selectable area and then
;;;           Use constructor template for Racket on r.
(define (racket-after-button-down r x y)
  (if (in-racket? r x y)
      (make-racket (racket-x r)
                   (racket-y r)
                   (racket-vx r)
                   (racket-vy r)
                   true
                   x
                   y)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-after-drag : Racket Integer Integer -> Racket
;;; GIVEN   : a racket and the mouse coordinates for drag destination
;;; RETURNS : the racket following a drag at the given location
;;; EXAMPLES:
;;;     (racket-after-mouse-event (make-racket 30 30 31 30
;;;                                            true -30 -30) 10 10 "drag")
;;;      => (make-racket 70 70 31 30 #true 10 10)
;;; STRATEGY: Cases on whether racket is selected and then
;;;           Use constructor template for Racket on r.
(define (racket-after-drag r x y)
  (if (racket-selected? r)
      (make-racket (drag-relative-x r x)
                   (drag-relative-y r y)
                   (racket-vx r)
                   (racket-vy r)
                   true
                   x
                   y)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; drag-relative-x : Racket Integer -> Integer
;;; GIVEN   : a racket and the x coordinate of the drag handle's position
;;; RETURNS : a new x position for the racket from the given handle
;;; EXAMPLE :
;;;       (drag-relative-x (make-racket 40 42 1 1 true 47 47) 50) => 43
;;; STRATEGY: Transcribe formula.
(define (drag-relative-x r x)
  (- x (click-x-offet-from-racket-center r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; drag-relative-y : Racket Integer -> Integer
;;; GIVEN   : a racket and the y coordinate of the drag handle's position
;;; RETURNS : a new y position for the racket from the given handle.
;;; EXAMPLE :
;;;       (drag-relative-y (make-racket 40 42 1 1 true 47 47) 50) => 45
;;; STRATEGY: Transcribe formula.
(define (drag-relative-y r y)
  (- y (click-y-offet-from-racket-center r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; click-x-offet-from-racket-center : Racket -> Integer
;;; GIVEN   : a racket in the rally state of the world
;;; RETURNS : the difference between the racket's x position and the mouse-x.
;;; EXAMPLE :
;;;    (click-x-offet-from-racket-center (make-racket 40 42 1 1 true 47 47))
;;;              => 7
;;; STRATEGY: Transcribe formula.
(define (click-x-offet-from-racket-center r)
  (- (racket-mouse-x r) (racket-x r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; click-y-offet-from-racket-center : Racket -> Integer
;;; GIVEN   : a racket in the rally state of the world
;;; RETURNS : the difference between the racket's x position and the mouse-x.
;;; EXAMPLE :
;;;    (click-y-offet-from-racket-center (make-racket 40 42 1 1 true 47 47))
;;;              => 5
;;; STRATEGY: Transcribe formula.
(define (click-y-offet-from-racket-center r)
  (- (racket-mouse-y r) (racket-y r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-after-button-up : Racket Integer Integer -> Racket
;;; RETURNS : the racket following a button-up at the given location
;;; STRATEGY: Cases on whether racket is selected and then
;;;           Use constructor template for Racket on r.
(define (racket-after-button-up r x y)
  (if (racket-selected? r)
      (make-racket (racket-x r)
                   (racket-y r)
                   (racket-vx r)
                   (racket-vy r)
                   false
                   (racket-mouse-x r)
                   (racket-mouse-y r))
      r))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; in-racket? : Racket Integer Integer -> Boolean
;;; RETURNS : true iff the given coordinate is inside the bounding box of
;;;           the given racket.
;;; EXAMPLES:
;;;        (in-racket? racket-at-50-50 70 70) => true
;;;        (in-racket? racket-at-50-50 100 70) => false
;;; STRATEGY: Transcribe formula.
(define (in-racket? r x y)
  (<= (sqrt (+ (sqr (- x (racket-x r))) (sqr (- y (racket-y r)))))
      RACKET-SELECTION-RADIUS))


;;; TESTS:
(begin-for-test
  ;;; button-down:
  ;;; button-down inside racket's selectable zone
  (check-equal?
   (world-after-mouse-event 
    (make-world
     (list ball-all-at-30)
     unselected-racket-at-100-100
     RALLY-STATE 1 1)
    110 110    ;;; a coordinate inside selectable zone
    "button-down")
   (make-world
    (list ball-all-at-30)
    selected-racket-at-100-100
    RALLY-STATE 1 1)
   "button down inside racket should select it")


  ;;; button-down outside racket
  (check-equal?
   (world-after-mouse-event 
    (make-world
     (list ball-all-at-30)
     unselected-racket-at-100-100
     RALLY-STATE 1 1)
    126 126    ;;; a coordinate outside selectable zone
    "button-down")
   (make-world
    (list ball-all-at-30)
    unselected-racket-at-100-100
    RALLY-STATE 1 1)
   "button down inside racket should not affect it")

  
  ;;; tests for drag
  ;;; racket not selected: drag should not change anything
  (check-equal?
   (world-after-mouse-event 
    (make-world
     (list ball-all-at-30)
     unselected-racket-at-100-100
     RALLY-STATE 1 1)
    150 150    ;;; a coordinate outside selectable zone
    "drag")
   (make-world
    (list ball-all-at-30)
    unselected-racket-at-100-100
    RALLY-STATE 1 1)
   "drag with racket unslected leaves world unchanged")
    
  ;;; racket selected
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list ball-all-at-30)
     selected-racket-at-100-100
     RALLY-STATE 1 1)
    250 250 ;;; drag for larger pixels distance
    "drag")
   (make-world
    (list ball-all-at-30)
    (make-racket 240 240 1 1 true 250 250)
    RALLY-STATE 1 1)
   "drag with racket selected performs displacement")

   
  ;;; tests for button-up
  ;;; unselect racket
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list ball-all-at-30)
     selected-racket-at-100-100
     RALLY-STATE 1 1)
    400 400   ;;; arbitrary location
    "button-up")
   (make-world
    (list ball-all-at-30)
    unselected-racket-at-100-100
    RALLY-STATE 1 1)
   "button-up expected to unselect the racket")


  ;;; make no change racket
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list ball-all-at-30)
     unselected-racket-at-100-100
     RALLY-STATE 1 1)
    400 400   ;;; arbitrary location
    "button-up")
   (make-world
    (list ball-all-at-30)
    unselected-racket-at-100-100
    RALLY-STATE 1 1)
   "button-up expected to make no change to already unselected racket")

  ;;; unselect racket
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list ball-all-at-30)
     selected-racket-at-100-100
     RALLY-STATE 1 1)
    400 400   ;;; arbitrary location
    "button-up")
   (make-world
    (list ball-all-at-30)
    unselected-racket-at-100-100
    RALLY-STATE 1 1)
   "button-up expected to unselect the racket")

  
  ;;; tests for other mouse events
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list ball-all-at-30)
     selected-racket-at-100-100
     RALLY-STATE 1 1)
    100 100 ;;; arbitrary coordinate
    "move")
   (make-world
    (list ball-all-at-30)
    selected-racket-at-100-100
    RALLY-STATE 1 1)
   "other mouse events should leave the world unchanged")


  ;;; tests for other mouse events
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list ball-all-at-30)
     selected-racket-at-100-100
     RALLY-STATE 1 1)
    100 100 ;;; arbitrary coordinate
    "move")
   (make-world
    (list ball-all-at-30)
    selected-racket-at-100-100
    RALLY-STATE 1 1)
   "other mouse events should leave the world unchanged")


  ;;; tests for world in paused and ready states
  ;;; mouse event at pause state
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list ball-all-at-30)
     selected-racket-at-100-100
     PAUSE-STATE 1 1)
    250 250 ;;; drag for larger pixels distance
    "drag")
   (make-world
    (list ball-all-at-30)
    selected-racket-at-100-100
    PAUSE-STATE 1 1)
   "world in pause state has no response to mouse events")

  ;;; mouse event at pause state
  (check-equal?
   (world-after-mouse-event
    (make-world
     (list ball-all-at-30)
     selected-racket-at-100-100
     READY-STATE 1 1)
    250 250 ;;; drag for larger pixels distance
    "drag")
   (make-world
    (list ball-all-at-30)
    selected-racket-at-100-100
    READY-STATE 1 1)
   "world in ready state has no response to mouse events"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; WORLD TO SCENE:
;;; world-to-scene : World -> Scene
;;; RETURNS : a Scene that portrays the given world.
;;; EXAMPLE : See tests below.
;;; STRATEGY: Combine simpler functions.
(define (world-to-scene w)
  (scene-with-balls
   (world-balls w)
   (scene-with-racket
    (world-racket w)
    (scene-with-court
     w
     COURT-CANVAS-WITH-WALLS))))

;;; TESTS:
;;; Initial State
(begin-for-test
  (check-equal? (world-to-scene INITIAL-WORLD-24)
                (place-image
                 BALL-CIRCLE
                 330 384
                 (place-image
                  RACKET-RECTANGLE
                  330 384
                  (place-image
                   UNPAUSED-COURT-FLOOR
                   HALF-COURT-WIDTH HALF-COURT-HEIGHT
                   COURT-CANVAS-WITH-WALLS)))
                "Testing the rendered image for initial state."))

;;; Draggable State
(begin-for-test
  (check-equal? (world-to-scene (make-world
                                 (list ball-all-at-30)
                                 selected-racket-at-100-100
                                 READY-STATE 1 1))
                (place-image
                 BALL-CIRCLE
                 330 384
                 (place-image
                  MOUSE-POINTER-CIRCLE
                  110 110
                  (place-image
                   RACKET-RECTANGLE
                   100 100
                   (place-image
                    UNPAUSED-COURT-FLOOR
                    HALF-COURT-WIDTH HALF-COURT-HEIGHT
                    COURT-CANVAS-WITH-WALLS))))
                "Testing the rendered image during mouse drag."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helper functions:
;;; scene-with-balls : BallList Scene -> Scene
;;; GIVEN   : a list of balls and a scene
;;; RETURNS : a scene like the given one, but with the given balls painted
;;;           on it.
;;; EXAMPES : see tests above
;;; OLD STRATEGY: Use observer template for BallList
#;(define (scene-with-balls bl s)
    (cond [(empty? bl) s]
          [else (place-image BALL-CIRCLE
                             (ball-x (first bl)) (ball-y (first bl))
                             (scene-with-balls (rest bl) s))]))

;;; NEW STRATEGY: Use HOF foldr on bl.
(define (scene-with-balls bl s)
  (foldr
   ;; Ball -> Scene
   ;; GIVEN  : a ball b,
   ;; RETURNS: a scene with given ball painted on it
   (lambda (b scene) (place-image BALL-CIRCLE
                                  (ball-x b) (ball-y b) scene))
   s
   bl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; scene-with-racket : Racket Scene -> Scene
;;; GIVEN   : a racket and a scene
;;; RETURNS : a scene like the given one, but with the given racket painted
;;; on it.
;;; EXAMPES: see tests above
;;; STRATEGY: Cases on whether the given racket is selected for dragging.
(define (scene-with-racket r s)
  (if (racket-selected? r)
      (scene-with-draggable-racket r s)
      (scene-with-racket-unselected r s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; scene-with-draggable-racket : Racket Scene -> Scene
;;; GIVEN   : a selected racket and a scene
;;; RETURNS : a scene like the given one, but with the given racket painted
;;; on it.
;;; EXAMPES: see tests above
;;; STRATEGY: Place image of racket on given scene at the racket's coordinates
(define (scene-with-draggable-racket r s)
  (place-image
   MOUSE-POINTER-CIRCLE
   (racket-mouse-x r) (racket-mouse-y r)
   (scene-with-racket-unselected r s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; scene-with-racket-unselected : Racket Scene -> Scene
;;; GIVEN   : the state of a racket and a scene
;;; RETURNS : a scene like the given one, but with the given racket painted
;;; on it.
;;; EXAMPES: see tests above
;;; STRATEGY: Place image of racket on given scene at the racket's coordinates
(define (scene-with-racket-unselected r s)
  (place-image
   RACKET-RECTANGLE
   (racket-x r) (racket-y r)
   s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; scene-with-court : World Scene -> Scene
;;; GIVEN   : a state of the world and a scene
;;; RETURNS : a scene like the given one, but with the corresponding 
;;;           background painted on it.
;;; EXAMPES: see tests below.
;;; STRATEGY: Use observer template for StateName.
(define (scene-with-court w s)
  (cond
    [(string=? (world-state w) PAUSE-STATE)
     (scene-with-paused-court s)]
    [(string=? (world-state w) RALLY-STATE)
     (scene-with-unpaused-court s)]
    [(string=? (world-state w) READY-STATE)
     (scene-with-unpaused-court s)]
    ))

;;; TESTS:
(define EMPTY-SQUARE-1000 (empty-scene 1000 1000))
(begin-for-test
  (check-equal? (scene-with-court PAUSE-STATE-WORLD EMPTY-SQUARE-1000)
                (place-image
                 PAUSED-COURT-FLOOR
                 HALF-COURT-WIDTH HALF-COURT-HEIGHT
                 EMPTY-SQUARE-1000)
                "Expecting paused scenes to match")
  (check-equal? (scene-with-court RALLY-STATE-WORLD EMPTY-SQUARE-1000)
                (place-image
                 UNPAUSED-COURT-FLOOR
                 HALF-COURT-WIDTH HALF-COURT-HEIGHT
                 EMPTY-SQUARE-1000)
                "Expecting unpaused scenes to match"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; scene-with-paused-court : Scene -> Scene
;;; GIVEN   : a scene,
;;; RETURNS : a scene like the given one, but with the paused court painted
;;; on it.
;;; EXAMPLE : see above tests.
;;; STRATEGY: Place the image of a paused court floor on given scene centered.
(define (scene-with-paused-court s)
  (place-image
   PAUSED-COURT-FLOOR
   HALF-COURT-WIDTH HALF-COURT-HEIGHT
   s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; scene-with-unpaused-court : Scene -> Scene
;;; GIVEN   : a scene,
;;; RETURNS : a scene like the given one, but with the paused court painted
;;; on it.
;;; EXAMPLE : see above tests.
;;; STRATEGY: Place the image of an unpaused floor on given scene centered.
(define (scene-with-unpaused-court s)
  (place-image
   UNPAUSED-COURT-FLOOR
   HALF-COURT-WIDTH HALF-COURT-HEIGHT
   s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Implemented implicitly via creation of Ball and Racket structs

;;; world-balls : World -> BallList
;;; GIVEN   : a world
;;; RETURNS : a list of the balls that are present in the world
;;;           (but does not include any balls that have disappeared
;;;           by colliding with the back wall)
;;; STRATEGY: Implemented implicitly via creation of the Ball struct

          
;;; world-racket : World -> Racket
;;; GIVEN   : a world
;;; RETURNS : the racket that's present in the world
;;; STRATEGY: Implemented implicitly via creation of the Racket struct

         
;;; ball-x : Ball -> Integer
;;; ball-y : Ball -> Integer
;;; racket-x : Racket -> Integer
;;; racket-y : Racket -> Integer
;;; GIVEN   : a racket or ball
;;; RETURNS : the x or y coordinate of that item's position,
;;;     in graphics coordinates
;;; STRATEGY: Implemented implicitly via creation of the Ball and the
;;;           Racket structs.


;;; ball-vx : Ball -> Integer
;;; ball-vy : Ball -> Integer
;;; racket-vx : Racket -> Integer
;;; racket-vy : Racket -> Integer
;;; GIVEN   : a racket or ball
;;; RETURNS : the vx or vy component of that item's velocity,
;;;     in pixels per tick
;;; STRATEGY: Implemented implicitly via creation of the Ball and the
;;;           Racket structs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BEGIN SIMULATION:
(simulation 1/24)