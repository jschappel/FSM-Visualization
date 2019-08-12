#lang racket
(require 2htdp/image 2htdp/universe "posn.rkt")

;; ------- msgWindow.rkt -------
;; This file contains the functionality for a message window
;; Written by: Joshua Schappel 8/9/2019


;; Export necessary functions
(provide
 (struct-out msgWindow)
 draw-window
 exit-pressed?)

(define HEIGHT 300) ;; Height of the message box
(define WIDTH 400) ;; Width of the message box
(define TOP-HEIGHT 20) ;; Height of the header for the message box
(define MSG-WIDTH 300) ;; Width of of the displayed message
(define TEXT-COLOR (make-color 255 0 0)) ;; The color of the message
(define BACKGROUND-SHADE (make-color 0 0 0 100)) ;; The color of the shaded background


;; msgWindow: A structure that represents a GUI msg window
;; - msg: A string that holds the message for the window to display
;; - header: A string that is the header for the message window
;; - location: A posn that is the location of where the window will be rendered
(struct msgWindow (msg header location))


;; draw-window: msgWindow scene Scene-width Scene-height -> image
;; Purpose: Returns an image that contains the elemets of the msgWindow structure.
;; IMPORTANT: make sure the widht and the height are the same as the scene's width and height so that the shading effct takes up the whole scene
(define (draw-window window scn width height)
  (place-image (create-window-image window width height) (posn-x (msgWindow-location window)) (posn-y (msgWindow-location window)) scn))


;; create-window-top: window width height -> image
;; Purpose: creates the main image for the message window
(define (create-window-image window w h)
  (letrec (
           ;; render-text: array accum int -> string
           ;; Purpose: decides if the string should continue onto the next line
           (render-text (lambda (msg-array accum fnt-size)
                         
                          (cond
                            [(empty? msg-array) (text accum fnt-size TEXT-COLOR)]
                            [(equal? "" accum) (render-text (cdr msg-array) (string-append accum (car msg-array)) fnt-size)]
                            [(> (image-width (text (string-append accum " " (car msg-array)) fnt-size TEXT-COLOR)) MSG-WIDTH) (render-text (cdr msg-array) (string-append accum "\n" (car msg-array)) fnt-size)]
                            [else (render-text (cdr msg-array) (string-append accum " " (car msg-array)) fnt-size)]))))
    (overlay
     (render-text (string-split (msgWindow-msg window)) "" 18)
     (overlay/align "left" "top"
                    (overlay/align "right" "top"
                                   (overlay
                                    (text "X" 12 "white")
                                    (rectangle 30 TOP-HEIGHT "outline" "black")
                                    (rectangle 30 TOP-HEIGHT "solid" "red"))
                                   (overlay
                                    (text (msgWindow-header window) 15 "black")
                                    (rectangle WIDTH TOP-HEIGHT "outline" "black")
                                    (rectangle WIDTH TOP-HEIGHT "solid" (make-color 197 199 199)))
                                   )
                    (rectangle WIDTH HEIGHT "outline" "black")
                    (rectangle WIDTH HEIGHT "solid" "white"))
     (create-window-background w h))))


;; create-window-background: msgWindow scn -> image
;; Purpose: creates the shaded background for the window image
(define (create-window-background w h)
  (rectangle w h "solid" BACKGROUND-SHADE))


;; exit-pressed? int int msgWindow int int -> boolean
;; Purpose: Determins if the exit button was pressed
;; When given an x and y corrdinate, will determine if that coordinate was inside the exit button. If so returns true
(define (exit-pressed? mouse-x mouse-y msgWindow scnW scnH)
  (cond
    [(and (and (> mouse-x (- (+ (/ scnW 2) (/ WIDTH 2)) 30))
               (< mouse-x  (+ (/ scnW 2) (/ WIDTH 2))))
          (and (> mouse-y  (- (/ scnH 2) (/ HEIGHT 2)))
               (< mouse-y (+ (- (/ scnH 2) (/ HEIGHT 2)) TOP-HEIGHT))))
     #t]
    [else #f]))