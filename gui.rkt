#lang racket
(require 2htdp/image 2htdp/universe "button.rkt" "posn.rkt" "input.rkt")

;; GLOBAL VALIRABLES
(define WIDTH 1200) ;; The width of the scene
(define HEIGHT 600) ;; The height of the scene
(define TOP (/ HEIGHT 10))
(define RIGHT (/ WIDTH 5))
(define BOTTOM(/ HEIGHT 8))
(define CONTROL-BOX-H (/ HEIGHT 5)) ;; The height of each left side conrol box
(define E-SCENE (empty-scene WIDTH HEIGHT "white")) ;; Create the initial scene

;; CIRCLE VARIABLES
(define X0  (/ (-  WIDTH 200) 2))
(define Y0 (/  HEIGHT 2))
(define R 175)
(define inner-R (- R 25))
(define the-circle (circle R "outline" "transparent"))

(define SCALE 1)

;; WORLD GLOBAL VARIABLES
(define STATE-LIST '()) ;; The list of states for the machine 
(define SYMBOL-LIST '()) ;; The list of symbols for the machine
(define START-STATE null) ;; The starting state of the machine
(define FINAL-STATE-LIST '()) ;; The list of final states that the machine has
(define RULE-LIST (list "(j j j)" "(i i i)" "(h h h)" "(g g g)"  "(f f f)" "(e e e)" "(d d d)" "(c c c)" "(b b b)" "(a a a)")) ;; The list of rules that the machine must follow
(define SIGMA-LIST '()) ;; The list of sigma for the mahcine
(define TAPE-POSITION 0) ;; The current position on the tape
(define CURRENT-RULE null) ;; The current rule that the machine is following
(define CURRENT-STATE null) ;; The current state that the machine is in
(define PROCESSED-CONFIG-LIST '()) ;; TODO
(define UNPROCESSED-CONFIG-LIST '()) ;; TODO

;; world: The world for the GUI
;; - state-list:
;; - symbol-list:
;; - start-state:
;; - final-state-list:
;; - rule-list:
;; - sigma-list:
;; - tape-position:
;; - cur-rule:
;; - cur-state:
;; - button-list:
;; - input-list:
;; - processed-config-list:
;; - unprocessed-config-list:
(struct world (state-list symbol-list start-state final-state-list rule-list sigma-list tape-position cur-rule cur-state button-list input-list processed-config-list unporcessed-config-list) #:transparent)

;; ***** BUTTON FUCTIONS BELOW *****

;; THIS FUNCTION IS JUST A PLACEHOLDER
(define NULL-FUNCTION (lambda (w)
                        (redraw-world w)))

(define addState (lambda (w)
                   (let ((state (string-trim (textbox-text (car (world-input-list w)))))
                         (new-input-list (list-set (world-input-list w) 0 (remove-text (car (world-input-list w)) 100))))
                     
                     (world (cons state (world-state-list w)) (world-symbol-list w)
                            (world-start-state w) (world-final-state-list w) (world-rule-list w)
                            (world-sigma-list w) (world-tape-position w) (world-cur-rule w)
                            (world-cur-state w) (world-button-list w) new-input-list
                            (world-processed-config-list w) (world-unporcessed-config-list w)))))

(define removeState (lambda(w)
                      (let ((state (string-trim (textbox-text (car (world-input-list w)))))
                            (new-input-list (list-set (world-input-list w) 0 (remove-text (car (world-input-list w)) 100))))
                        
                        (world(remove state (world-state-list w))
                              (world-symbol-list w)  (world-start-state w) (world-final-state-list w) (world-rule-list w)
                              (world-sigma-list w) (world-tape-position w) (world-cur-rule w)
                              (world-cur-state w) (world-button-list w) new-input-list
                              (world-processed-config-list w) (world-unporcessed-config-list w)))))

(define addRule (lambda (w)
                  (let ((input-list (world-input-list w))
                        (r1 (string-trim (textbox-text (list-ref (world-input-list w) 4))))
                        (r2 (string-trim (textbox-text (list-ref (world-input-list w) 5))))
                        (r3 (string-trim (textbox-text (list-ref (world-input-list w) 6))))
                        (new-input-list (list-set (list-set (list-set (world-input-list w) 6 (remove-text (list-ref (world-input-list w) 6) 100)) 5 (remove-text (list-ref (world-input-list w) 5) 100)) 4 (remove-text (list-ref (world-input-list w) 4) 100))))
                    (cond
                      [(or (equal? r1 "") (equal? r2 "") (equal? r3 "")) (redraw-world w)]
                      [else
                       (world (world-state-list w) (world-symbol-list w)
                              (world-start-state w) (world-final-state-list w) (cons (string-append "(" r1 " " r2 " " r3 ")") (world-rule-list w))
                              (world-sigma-list w) (world-tape-position w) (world-cur-rule w)
                              (world-cur-state w) (world-button-list w) new-input-list
                              (world-processed-config-list w) (world-unporcessed-config-list w))]))))
                        
    

                  
;; **** BUTTONS BELOW ***
(define BTN-ADD-STATE (button 70 25 "Add" "solid" (make-color 230 142 174) (make-color 230 142 174) 24 #f #f (posn (- WIDTH 150) (- CONTROL-BOX-H 25)) addState))
(define BTN-REMOVE-STATE (button 70 25 "Remove" "solid" (make-color 230 142 174) (make-color 230 142 174) 24 #f #f (posn (- WIDTH 50) (- CONTROL-BOX-H 25)) removeState))

(define BTN-ADD-ALPHA (button 70 25 "Add" "solid" (make-color 230 142 174) (make-color 230 142 174) 24 #f #f (posn (- WIDTH 150) (- (* 2 CONTROL-BOX-H) 25)) NULL-FUNCTION))
(define BTN-REMOVE-ALPHA (button 70 25 "Remove" "solid" (make-color 230 142 174) (make-color 230 142 174) 24 #f #f (posn (- WIDTH 50) (- (* 2 CONTROL-BOX-H ) 25)) NULL-FUNCTION))

(define BTN-ADD-START (button 50 25 "Add" "solid" (make-color 230 142 174) (make-color 230 142 174) 18 #f #f (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 71)) NULL-FUNCTION))
(define BTN-REMOVE-START (button 50 25 "Remove" "solid" (make-color 230 142 174) (make-color 230 142 174) 18 #f #f (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 25)) NULL-FUNCTION))

(define BTN-ADD-END (button 50 25 "Add" "solid" (make-color 230 142 174) (make-color 230 142 174) 18 #f #f (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 71)) NULL-FUNCTION))
(define BTN-REMOVE-END (button 50 25 "Remove" "solid" (make-color 230 142 174) (make-color 230 142 174) 18 #f #f (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 25)) NULL-FUNCTION))

(define BTN-ADD-RULES (button 70 25 "Add" "solid" (make-color 230 142 174) (make-color 230 142 174) 24 #f #f (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 25)) addRule))
(define BTN-REMOVE-RULES (button 70 25 "Remove" "solid" (make-color 230 142 174) (make-color 230 142 174) 24 #f #f (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 25)) NULL-FUNCTION))


(define BTN-NEXT (button 100 50 "NEXT" "solid" (make-color 230 142 174) (make-color 230 142 174) 36 #f #f (posn (- WIDTH 275) 100) NULL-FUNCTION))
(define BTN-PREV (button 100 50 "PREV" "solid" (make-color 230 142 174) (make-color 230 142 174) 36 #f #f (posn 75 100) NULL-FUNCTION))
(define BTN-RUN (button 100 50 "RUN" "solid" (make-color 230 142 174) (make-color 230 142 174) 36 #f #f (posn 75 (- HEIGHT 115)) NULL-FUNCTION))

;; BUTTON-LIST: A List containing all buttons that are displayed on the scene.
(define BUTTON-LIST (list BTN-ADD-STATE BTN-REMOVE-STATE
                          BTN-ADD-ALPHA BTN-REMOVE-ALPHA
                          BTN-ADD-START BTN-REMOVE-START
                          BTN-ADD-END BTN-REMOVE-END
                          BTN-ADD-RULES BTN-REMOVE-RULES
                          BTN-RUN BTN-NEXT BTN-PREV))



;; **** INPUT FIELDS BELOW ****
(define IPF-STATE (textbox 150 25 (make-color 110 162 245) (make-color 110 162 245) "" 5 (posn (- WIDTH 100) (- CONTROL-BOX-H 70)) #f))
(define IPF-ALPHA (textbox 150 25 (make-color 110 162 245) (make-color 110 162 245) "" 10 (posn (- WIDTH 100) (- (* 2 CONTROL-BOX-H) 70)) #f))
(define IPF-START (textbox 75 25 (make-color 110 162 245) (make-color 110 162 245) "" 10 (posn (- WIDTH 150) (- (* 3 CONTROL-BOX-H) 50)) #f))
(define IPF-END (textbox 75 25 (make-color 110 162 245) (make-color 110 162 245) "" 10 (posn (- WIDTH 150) (- (* 4 CONTROL-BOX-H) 50)) #f))
(define IPF-RULE1 (textbox 40 25 (make-color 110 162 245) (make-color 110 162 245) "" 1 (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 70)) #f))
(define IPF-RULE2 (textbox 40 25 (make-color 110 162 245) (make-color 110 162 245) "" 1 (posn (- WIDTH 100) (- (* 5 CONTROL-BOX-H) 70)) #f))
(define IPF-RULE3 (textbox 40 25 (make-color 110 162 245) (make-color 110 162 245) "" 1 (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 70)) #f))

;; INPUT-LIST: A list containing all input fields that are displayed on the scene.
(define INPUT-LIST (list IPF-STATE IPF-ALPHA IPF-START IPF-END IPF-RULE1 IPF-RULE2 IPF-RULE3))


;; Initialize the world
(define INIT-WORLD (world STATE-LIST SYMBOL-LIST START-STATE FINAL-STATE-LIST RULE-LIST SIGMA-LIST TAPE-POSITION
                          CURRENT-RULE CURRENT-STATE BUTTON-LIST INPUT-LIST PROCESSED-CONFIG-LIST UNPROCESSED-CONFIG-LIST))

(define (draw-world w)
  (letrec((draw-input-list (lambda (loi scn)
                             (cond
                               [(empty? loi) scn]
                               [else (draw-textbox (car loi) (draw-input-list (cdr loi) scn))])))
          (draw-button-list (lambda (lob scn)
                              (cond
                                [(empty? lob) scn]
                                [else (draw-button (car lob) (draw-button-list (cdr lob) scn))])))
          (deg-shift (if (empty? (world-state-list w)) 0 (/ 360 (length (world-state-list w)))))
          (get-x (lambda (theta rad) (truncate (+ (* rad (cos (degrees->radians theta))) X0))))
                
          (get-y(lambda (theta rad)
                  (truncate (+ (* rad (sin (degrees->radians theta))) Y0))))
          
          (draw-states
           (lambda (l i s)
             (cond[(empty? l) s]
                  [else (place-image (text  (car l) 25 "red")
                                     (get-x (* deg-shift i) R)
                                     (get-y (* deg-shift i) R)
                                     (draw-states (cdr l) (add1 i) s))]))))
          
    
    (place-image the-circle X0 Y0 (draw-states (world-state-list w) 0 
                                               (place-image (create-gui-left) (- WIDTH 100) (/ HEIGHT 2)
                                                            (place-image (create-gui-top) (/ WIDTH 2) (/ TOP 2)
                                                                         (place-image (create-gui-bottom (world-rule-list w)) (/ WIDTH 2) (- HEIGHT (/ BOTTOM 2))
                                                                                      (draw-button-list (world-button-list w) (draw-input-list (world-input-list w) E-SCENE)))))))))


;; top-input-label: null -> image
;; Purpose: Creates the top left input lable
(define (top-input-label)
  (overlay
   (text (string-upcase "Input") 24 "Black")
   (rectangle (/ WIDTH 11) TOP "outline" "blue")))


;; los-top-label: null -> Image
;; Purpose: Creates the top list of sigmas lable
(define (los-top-label)
  (overlay
   (text (string-upcase "List of sigmas goes here") 24 "Black")
   (rectangle (- (- WIDTH (/ WIDTH 11)) 200) TOP "outline" "blue")))


;; create-gui-bottom: list-of-rules -> image
;; Purpose: Creates the bottom of the gui layout
(define (create-gui-bottom lor)
  (overlay/align "left" "middle"
                 (align-items
                  (rules-bottom-label)
                  (lor-bottom-label lor))
                 (rectangle WIDTH BOTTOM "outline" "transparent")))


;; rules-bottom-label: null -> image
;; Purpose: Creates the left bottom label in the gui
(define (rules-bottom-label)
  (overlay
   (text (string-upcase "Rules") 24 "Black")
   (rectangle (/ WIDTH 11) BOTTOM "outline" "blue")))

; create-gui-top: null -> image
;; Creates the top of the gui layout
(define (create-gui-top)
  (overlay/align "left" "middle"
                 (align-items
                  (top-input-label)
                  (los-top-label))
                 (rectangle WIDTH TOP "outline" "transparent")))

;; align-items image image -> image
;; Purpose: Aligns 2 images next to each other
(define (align-items item1 item2)
  (beside
   item1
   item2))

;; lor-bottom-label: list-of-rules -> image
;; Purpose: The label for the list of rules
(define (lor-bottom-label lor)
  (letrec (
           ;; list-to-string: list-of-rules -> list-of-strings
           ;; Purpose: formates a list of rules to be displayed on the gui
           (list-to-string (lambda (lor)
                             (cond
                               [(empty? lor) ""]
                               [else (string-append (car lor) " , " (list-to-string (cdr lor)))])))
           (text-str (list-to-string (reverse lor))))
    (println SCALE)
    (scale-text-to-image (text (substring text-str 0 (- (string-length text-str) 2)) 24 "Black") (rectangle (- (- WIDTH (/ WIDTH 11)) 200) BOTTOM "outline" "blue") SCALE)))


   
;; create-gui-left: null -> image
;; Purpose: creates the left conrol panel for the 
(define (create-gui-left)
  (overlay/align "left" "top"
                 (above/align "left"
                              (state-left-control)
                              (alpha-left-control)
                              (start-left-control)
                              (end-left-control)
                              (rule-left-control))
                 (rectangle 200 HEIGHT "outline" "gray")))

;; state-left-control: null -> image
;; Purpose: Creates the state control panel
(define (state-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")
                 (control-header "State Options")))

                 
;; alpha-left-control: null -> image
;; Purpose: Creates the alpha control panel
(define (alpha-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")
                 (control-header "Alpha Options")))


;; start-left-control: null -> image
;; Purpose: Creates the start control panel
(define (start-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")
                 (control-header "Start State")))


;; end-left-control: null -> image
;; Purpose: Creates the end control panel
(define (end-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")
                 (control-header "End State")))

;; rule-left-control: null -> image
;; Purpose: Creates the rule control panel
(define (rule-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")
                 (control-header "Add Rules")))
  
;; control-header: string -> image
;; Purpose: Creates a header label for right control panel
(define (control-header msg)
  (overlay
   (text (string-upcase msg) 18 "Black")
   (rectangle 200 25 "outline" "transparent")))


;; scale-text-to-image image image integer (between 0 and 1) -> image
;; Purpose: Scales the text of an image to not be larger then the image it is overlayed on
(define (scale-text-to-image text img sc)
  (let ((newScale (- sc .2)))
    (cond
      [(> (image-width text) (image-width img))
       (scale-text-to-image (scale newScale text) img SCALE)]
      [else (overlay (scale sc text) img)])))

;; process-mouse-event: world integer integer string --> world
;; Purpose: processes a users mouse event
(define (process-mouse-event w x y me)
  (letrec
      ;; Check-and-set: list-of-input-fields mouse-x mouse-y -> list-of-input-fields
      ;; Purpose: sets the input fields to active or inactive depending on where the mouse click happens
      ((check-and-set (lambda (loi x y)
                        (cond
                          [(empty? loi) '()]
                          [(textbox-pressed? x y (car loi))
                           (cond
                             [(equal? (textbox-active (car loi)) #t) (cons (car loi) (check-and-set (cdr loi) x y))]
                             [else (cons (set-active (car loi)) (check-and-set (cdr loi) x y))])]
                          [else
                           (cond
                             [(equal? (textbox-active (car loi)) #t) (cons (set-inactive (car loi)) (check-and-set (cdr loi) x y))]
                             [else (cons (car loi) (check-and-set (cdr loi) x y))])])))
       
       ;; check-button-list: list-of-buttons mouse-x mosue-y -> button
       ;; Purpose: Iterates over a list of buttons and checks if one was pressed. If so then returns the button otherwise
       ;; it returns null.
       (check-button-list (lambda (lob x y)
                            (cond
                              [(empty? lob) null]
                              [(button-pressed? x y (car lob)) (car lob)]
                              [else (check-button-list (cdr lob) x y)])))

       ;; active-button-list: list-of-buttons mouse-x mouse-y -> list-of-buttons
       ;; Purpose: Creates a new list of buttons where the click button is set to active
       (active-button-list (lambda (lob x y)
                             (cond
                               [(empty? lob) '()]
                               [(button-pressed? x y (car lob)) (cons (set-active-button (car lob)) (active-button-list (cdr lob) x y))]
                               [else (cons (car lob) (active-button-list (cdr lob) x y))]))))
    
    (cond
      [(string=? me "button-down")
       (define buttonPressed (check-button-list (world-button-list w) x y))
       (cond
         [(not (null? buttonPressed)) (run-function buttonPressed (create-new-world-button w (active-button-list (world-button-list w) x y)))]
         [else (create-new-world-input w (check-and-set (world-input-list w) x y))])]
      [(string=? me "button-up")
       (create-new-world-button w (map (lambda (x) (set-inactive-button x)) (world-button-list w)))]
      [else (redraw-world w)])))




;; process-key: world key-> world
;; Purpose: processes a key users key event
(define (process-key w k)
  (letrec
      ((check-and-add (lambda (loi action)
                        (cond
                          [(empty? loi) '()]
                          [(equal? (textbox-active (car loi)) #t)
                           (cond
                             [(equal? action #t) (cons (add-text (car loi) k) (check-and-add (cdr loi) action))]
                             [else (cons (remove-text (car loi) 1) (check-and-add (cdr loi) action))])]
                          [else (cons (car loi) (check-and-add (cdr loi) action))]))))
    (cond
      [(and (or (or (key=? k "-") (key=? k " "))(string<=? "a" (string-downcase k) "z") (string<=? "1" (string-downcase k) "9")) (not (equal? k "shift")))
       (create-new-world-input w (check-and-add (world-input-list w) #t))]
      [(key=? k "\b") (create-new-world-input w (check-and-add (world-input-list w) #f))]
      [else w])))


;; create-new-world-input: world list-of-input-fields -> world
;; Purpose: Creates a new world to handle the list-of-input-fields changes
(define (create-new-world-input a-world loi)
  (world (world-state-list a-world) (world-symbol-list a-world) (world-start-state a-world) (world-final-state-list a-world) (world-rule-list a-world)
         (world-sigma-list a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) (world-button-list a-world)
         loi (world-processed-config-list a-world) UNPROCESSED-CONFIG-LIST))

;; create-new-world-button: world list-of-button-fields -> world
;; Purpose: Creates a new world to handle the list-of-button-fields changes
(define (create-new-world-button a-world lob)
  (world (world-state-list a-world) (world-symbol-list a-world) (world-start-state a-world) (world-final-state-list a-world) (world-rule-list a-world)
         (world-sigma-list a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) lob
         (world-input-list a-world) (world-processed-config-list a-world) UNPROCESSED-CONFIG-LIST))

;; redraw-world: world -> world
;; redraws the same world as before
(define (redraw-world a-world)
  (world (world-state-list a-world) (world-symbol-list a-world) (world-start-state a-world) (world-final-state-list a-world) (world-rule-list a-world)
         (world-sigma-list a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) (world-button-list a-world)
         (world-input-list a-world) (world-processed-config-list a-world) UNPROCESSED-CONFIG-LIST))
  

(big-bang
    INIT-WORLD
  (name "FSM GUI (Early ALPHA)")
  (on-draw draw-world)
  (on-mouse process-mouse-event)
  (on-key process-key))