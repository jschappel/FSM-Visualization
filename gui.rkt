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

;; WORLD GLOBAL VARIABLES
(define STATE-LIST '()) ;; The list of states for the machine 
(define SYMBOL-LIST '()) ;; The list of symbols for the machine
(define START-STATE null) ;; The starting state of the machine
(define FINAL-STATE-LIST '()) ;; The list of final states that the machine has
(define RULE-LIST '()) ;; The list of rules that the machine must follow
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
(define-struct world (state-list symbol-list start-state final-state-list rule-list sigma-list tape-position cur-rule cur-state button-list input-list processed-config-list unporcessed-config-list))







;; TEST BUTTON FUNCTION!!!!
(define (test-f)
  (println "This is the test f"))


;; **** BUTTONS BELOW ***
(define BTN-ADD-STATE (button 70 25 "Add" "solid" (make-color 230 142 174) #f (posn (- WIDTH 150) (- CONTROL-BOX-H 25)) test-f))
(define BTN-REMOVE-STATE (button 70 25 "Remove" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- CONTROL-BOX-H 25)) null))

(define BTN-ADD-ALPHA (button 70 25 "Add" "solid" (make-color 230 142 174) #f (posn (- WIDTH 150) (- (* 2 CONTROL-BOX-H) 25)) null))
(define BTN-REMOVE-ALPHA (button 70 25 "Remove" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 2 CONTROL-BOX-H ) 25)) null))

(define BTN-ADD-START (button 50 25 "Add" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 71)) null))
(define BTN-REMOVE-START (button 50 25 "Remove" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 25)) null))

(define BTN-ADD-END (button 50 25 "Add" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 71)) null))
(define BTN-REMOVE-END (button 50 25 "Remove" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 25)) null))

(define BTN-ADD-RULES (button 70 25 "Add" "solid" (make-color 230 142 174) #f (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 25)) null))
(define BTN-REMOVE-RULES (button 70 25 "Remove" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 25)) null))

;; BUTTON-LIST: A List containing all buttons that are displayed on the scene.
(define BUTTON-LIST (list BTN-ADD-STATE BTN-REMOVE-STATE
                          BTN-ADD-ALPHA BTN-REMOVE-ALPHA
                          BTN-ADD-START BTN-REMOVE-START
                          BTN-ADD-END BTN-REMOVE-END
                          BTN-ADD-RULES BTN-REMOVE-RULES))

;; INPUT FIELDS BELOW
(define IPF-STATE (textbox 150 25 (make-color 110 162 245) "" (posn (- WIDTH 100) (- CONTROL-BOX-H 70)) #f))

;; INPUT-LIST: A list containing all input fields that are displayed on the scene.
(define INPUT-LIST (list IPF-STATE))


;; Initialize the world
(define INIT-WORLD (make-world STATE-LIST SYMBOL-LIST START-STATE FINAL-STATE-LIST RULE-LIST SIGMA-LIST TAPE-POSITION
                               CURRENT-RULE CURRENT-STATE BUTTON-LIST INPUT-LIST PROCESSED-CONFIG-LIST UNPROCESSED-CONFIG-LIST))



(define (draw-world w)
  (letrec((draw-input-list (lambda (loi scn)
           (cond
             [(empty? loi) scn]
             [else (draw-textbox (car loi) (draw-input-list (cdr loi) scn))])))
          (draw-button-list (lambda (lob scn)
           (cond
             [(empty? lob) scn]
             [else (draw-button (car lob) (draw-button-list (cdr lob) scn))]))))
    
    (place-image (create-gui-left) (- WIDTH 100) (/ HEIGHT 2)
                 (place-image (create-gui-top) (/ WIDTH 2) (/ TOP 2)
                              (place-image (create-gui-bottom) (/ WIDTH 2) (- HEIGHT (/ BOTTOM 2))
                                           (draw-button-list (world-button-list w) (draw-input-list (world-input-list w) E-SCENE)))))))


;; top-input-label: null -> image
;; Purpose: Creates the top left input lable
(define (top-input-label)
  (overlay
   (text (string-upcase "Input") 24 "Black")
   (rectangle (/ WIDTH 11) TOP "outline" "red")))


;; los-top-label: null -> Image
;; Purpose: Creates the top list of sigmas lable
(define (los-top-label)
  (overlay
   (text (string-upcase "List of sigmas goes here") 24 "Black")
   (rectangle (- (- WIDTH (/ WIDTH 11)) 200) TOP "outline" "green")))


;; create-gui-bottom: null -> image
;; Purpose: Creates the top of the gui layout
;; create-gui-bottom: null -> image
;; Purpose: Creates the top of the gui layout
(define (create-gui-bottom)
  (overlay/align "left" "middle"
                 (align-items
                  (rules-bottom-label)
                  (lor-bottom-label))
                 (rectangle WIDTH BOTTOM "outline" "transparent")))


;; rules-bottom-label: null -> image
;; Purpose: Creates the left bottom label in the gui
(define (rules-bottom-label)
  (overlay
   (text (string-upcase "Rules") 24 "Black")
   (rectangle (/ WIDTH 11) BOTTOM "outline" "red")))

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

;; lor-bottom-label: null -> image
;; Purpose: The label for the list of rules
(define (lor-bottom-label)
  (overlay
   (text (string-upcase "List of rules will go here") 24 "Black")
   (rectangle (- (- WIDTH (/ WIDTH 11)) 200) BOTTOM "outline" "green")))

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


; process-mouse-event: world integer integer string --> world
(define (process-mouse-event w x y me)
  (cond
    [(string=? me "button-down")
     (define buttonPressed (check-button-list (world-button-list w) x y))
     (cond
       [(not (null? buttonPressed)) (begin
                                      (run-function buttonPressed)
                                      (redraw-world w))]
       [else (create-new-world w (check-and-set (world-input-list w) x y))

             ;;(define inputPressed (check-input-list (world-input-list w) x y))
             ;; (cond
             ;;   [(not (null? inputPressed))
             ;;    (begin
             ;;    (deactivate-inputs (world-input-list w))
             ;;    (set-active inputPressed)
             ;;   [else (println "More checks needed")])
             ])]
    [else (redraw-world w)]))

(define (create-new-world a-world loi)
  (make-world (world-state-list a-world) (world-symbol-list a-world) (world-start-state a-world) (world-final-state-list a-world) (world-rule-list a-world)
              (world-sigma-list a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) (world-button-list a-world)
              loi (world-processed-config-list a-world) UNPROCESSED-CONFIG-LIST))

;; check-button-list: list-of-buttons mouse-x mosue-y -> button
;; Purpose: Iterates over a list of buttons and checks if one was pressed. If so then returns the button otherwise
;; it returns null.
(define (check-button-list lob x y)
  (cond
    [(empty? lob) null]
    [(button-pressed? x y (car lob)) (car lob)]
    [else (check-button-list (cdr lob) x y)]))



(define (check-and-set loi x y)
  (cond
    [(empty? loi) '()]
    [(textbox-pressed? x y (car loi))
     (cond
       [(equal? (textbox-active (car loi)) #t) (cons (car loi) (check-and-set (cdr loi) x y))]
       [else (begin (println "set to active")(cons (set-active (car loi)) (check-and-set (cdr loi) x y)))])]
     
    [else
     (cond
       [(equal? (textbox-active (car loi)) #t) (begin (println "set to inactive")(cons (set-inactive (car loi)) (check-and-set (cdr loi) x y)))]
       [else (cons (car loi) (check-and-set (cdr loi) x y))])]))


(define (redraw-world a-world)
  (make-world (world-state-list a-world) (world-symbol-list a-world) (world-start-state a-world) (world-final-state-list a-world) (world-rule-list a-world)
              (world-sigma-list a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) (world-button-list a-world)
              (world-input-list a-world) (world-processed-config-list a-world) UNPROCESSED-CONFIG-LIST))
  


(big-bang
    INIT-WORLD
  (on-draw draw-world)
  (on-mouse process-mouse-event))