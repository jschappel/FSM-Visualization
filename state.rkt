#lang racket

(require 2htdp/image 2htdp/universe "posn.rkt")

;; ------- machine.rkt -------
;; This file contains the structure for fsm state
;; Written by: Joshua and Sash 9/10/2019

(provide
 (struct-out fsm-state))

;; state: A structure that represents a state in the GUI
;; - name { symbol }: The name of the state
;; - function { lambda }: A invariant function associated with the state
;; - type { int }: 0 if normal, 1 if start-state, 2 if end-state
;; - posn { posn }: The position of the state
(struct fsm-state (name function type posn) #:mutable)


;; state-pressed x y -> boolean
;; Purpose: Determins if a specific state was pressed
;; When given an x and y corrdinate, will determine if that coordinate was inside the state. If so returns true
(define (fsm-state-pressed? mouse-x mouse-y tbox)
  (cond
    [(and (and (> mouse-x (- (posn-x (fsm-state-posn tbox)) (/ (fsm-state-posn tbox) 2)))
               (< mouse-x (+ (posn-x (fsm-state-posn tbox)) (/ (fsm-state-posn tbox) 2))))
          (and (> mouse-y (- (posn-y (fsm-state-posn tbox)) (/ (fsm-state-posn tbox) 2)))
               (< mouse-y (+ (posn-y (fsm-state-posn tbox)) (/ (fsm-state-posn tbox) 2)))))
     #t]
    [else #f]))