#lang racket
(require 2htdp/image 2htdp/universe fsm net/sendurl racket/date "button.rkt" "posn.rkt" "state.rkt" "input.rkt" "msgWindow.rkt" "machine.rkt")

;; GLOBAL VALIRABLES
(define WIDTH 1200) ;; The width of the scene
(define HEIGHT 600) ;; The height of the scene
(define TOP (/ HEIGHT 10))
(define RIGHT (/ WIDTH 5))
(define BOTTOM(/ HEIGHT 8))
(define CONTROL-BOX-H (/ HEIGHT 5)) ;; The height of each left side conrol box
(define MAIN-SCENE (empty-scene WIDTH HEIGHT "white")) ;; Create the initial scene
(define SCENE-TITLE "FSM GUI ALPHA 2.0")

;; CIRCLE VARIABLES
(define X0  (/ (-  WIDTH 200) 2))
(define Y0 (/  HEIGHT 2))
(define R 175)
(define inner-R (- R 50))
(define the-circle (circle R "outline" "transparent"))
(define state-pen(pen "black" 3 "solid" "round" "miter"))
(define pointer-circle (circle 5 "solid" "black"))
(define pointer-square(square 10 "solid" "black"))

;; TEST MACHINES BELOW
(define INIT-STATES '(A B C D))
(define INIT-START 'A)
(define INIT-FINALS '(C D))
(define INIT-RULES (list '(A a B) '(B b A) '(A c C) '(C b D)))
(define INIT-SIGMA '(a b c b))
(define INIT-CURRENT 'A)
(define INIT-ALPHA '(a b c))

(define M1 (make-dfa INIT-STATES INIT-ALPHA INIT-START INIT-FINALS INIT-RULES))

;; This machine works
(define M2 (make-dfa '(A B C)
                     '(a b c)
                     'A
                     '(B C)
                     (list '(A b C)
                           '(A a B)
                           '(B c A))))



;; WORLD GLOBAL VARIABLES
(define STATE-LIST '()) ;; The list of states for the machine 
(define SYMBOL-LIST '()) ;; The list of symbols for the machine
(define START-STATE null) ;; The starting state of the machinen
(define FINAL-STATE-LIST '()) ;; The list of final states that the machine has
(define RULE-LIST '()) ;; The list of rules that the machine must follow
(define SIGMA-LIST '()) ;; The list of sigma for the mahcine
(define TAPE-POSITION 0) ;; The current position on the tape
(define CURRENT-RULE '(null null null)) ;; The current rule that the machine is following
(define CURRENT-STATE null) ;; The current state that the machine is in
(define PROCESSED-CONFIG-LIST '()) ;; TODO
(define UNPROCESSED-CONFIG-LIST '()) ;; TODO
(define ALPHA-LIST '()) ;; TODO
(define INIT-INDEX 0) ;; The initail index of the scrollbar

;; COLORS FOR GUI
(define CONTROLLER-BUTTON-COLOR (make-color 48 63 159))
(define INPUT-COLOR (make-color 255 193 7))
(define START-STATE-COLOR (make-color 6 142 60))
(define END-STATE-COLOR (make-color 219 9 9))
(define MSG-ERROR (make-color 255 0 0))
(define MSG-SUCCESS (make-color 65 122 67))
(define MSG-CAUTION (make-color 252 156 10))



;; world: The world for the GUI
;; - machine: A machine structure for the world
;; - tape-position: The current position on the tape
;; - cur-rule:
;; - cur-state:
;; - button-list: A list containing all buttons to be rendered on the GUI
;; - input-list: A list containing all the input-fields to be rendered on the GUI
;; - processed-config-list:
;; - unprocessed-config-list:
;; - error msg: A msgWindow structure that will be rendered on the screen if not null.
;; - scroll-bar-index: An integer that represents the first position in the rule list to be rendered on the screen.
;; - TODO stack list and stack alphabet
(struct world (fsm-machine tape-position cur-rule cur-state button-list input-list processed-config-list unporcessed-config-list error-msg scroll-bar-index) #:transparent)

;; ***** BUTTON FUCTIONS BELOW *****

;; THIS FUNCTION IS JUST A PLACEHOLDER
(define NULL-FUNCTION (lambda (w)
                        (redraw-world w)))


;; oppenHelp; world -> world
;; Purpose: opens the help link in an external browser window
(define openHelp (lambda (w)
                   (send-url "https://github.com/jschappel/FSM-Visualization/blob/master/help.md" #t)
                   (redraw-world w)))

;; addState: world -> world
;; Purpose: Adds a state to the world
(define addState (lambda (w)
                   (let ((state (string-trim (textbox-text (car (world-input-list w)))))
                         (new-input-list (list-set (world-input-list w) 0 (remove-text (car (world-input-list w)) 100))))
                     (cond[(equal? "" state) w]
                          [(ormap (lambda (x) (equal? state (symbol->string (fsm-state-name x)))) (machine-state-list (world-fsm-machine w)))
                           w]
                          [else
                           (begin
                             (set-machine-state-list! (world-fsm-machine w) (cons (fsm-state (string->symbol state) (lambda(v)v) (posn 0 0)) (machine-state-list (world-fsm-machine w))))
                             (create-new-world-input w new-input-list))]))))

;; removeState: world -> world
;; Purpose: Removes a state from the world
(define removeState (lambda(w)
                      (letrec ((state (string-trim (textbox-text (car (world-input-list w)))))
                               (new-input-list (list-set (world-input-list w) 0 (remove-text (car (world-input-list w)) 100)))

                               ;; remove-all: list-of-rules -> list-of-rules
                               ;; Purpose: Removes all rules from the machine that contain the current rule being removed
                               (remove-all (lambda (lor)
                                             (filter (lambda (x) (cond
                                                                   [(equal? (symbol->string (car x)) state) #f]
                                                                   [(equal? (symbol->string (caddr x)) state) #f]
                                                                   [else #t]))
                                                     lor))))
                        
                        (if (equal? (string->symbol state) (world-cur-state w))
                            (begin
                              (set-machine-state-list! (world-fsm-machine w) (remove (string->symbol state) (machine-state-list (world-fsm-machine w))))
                              (set-machine-rule-list! (world-fsm-machine w) (remove-all (machine-rule-list (world-fsm-machine w))))
                              (world (world-fsm-machine w)(world-tape-position w) (world-cur-rule w)
                                     null (world-button-list w) new-input-list
                                     (world-processed-config-list w) (world-unporcessed-config-list w) (world-error-msg w) (world-scroll-bar-index w)))

                            (begin
                              (set-machine-state-list! (world-fsm-machine w) (remove (string->symbol state) (machine-state-list (world-fsm-machine w))))
                              (set-machine-rule-list! (world-fsm-machine w) (remove-all (machine-rule-list (world-fsm-machine w))))
                              (create-new-world-input w new-input-list))))))


;; format-input: symbol -> symbol
;; Purpose: This is a helper function for addRule and removeRule that formats certine symbols into valid fsm symbols
;; EX: 'DEAD will become 'ds
(define format-input (lambda (s)
                       (case s
                         [(DEAD) 'ds]
                         [(EMP) 'e]
                         [(BLANK) '_]
                         [(LEFT) 'L]
                         [(LM) '@]
                         [(RIGHT) 'R]
                         [else s])))

;; addRule: world -> world
;; Purpose: Addes a rule to the world rule list
(define addRule (lambda (w)
                  (letrec ((input-list (world-input-list w))
                           (r1 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 4)))))
                           (r2 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 5)))))
                           (r3 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 6)))))
                           (new-input-list (list-set (list-set (list-set (world-input-list w) 6 (remove-text (list-ref (world-input-list w) 6) 100)) 5 (remove-text (list-ref (world-input-list w) 5) 100)) 4 (remove-text (list-ref (world-input-list w) 4) 100))))
                    (cond
                      [(or (equal? r1 '||) (equal? r2 '||) (equal? r3 '||)) (redraw-world w)]
                      [else
                       (begin
                         (set-machine-rule-list! (world-fsm-machine w) (cons (list (format-input r1) r2 (format-input r3)) (machine-rule-list (world-fsm-machine w))))
                         (create-new-world-input w new-input-list))]))))

;; removeRule: world -> world
;; Purpose: Removes a world from the world list
(define removeRule (lambda (w)
                     (let ((input-list (world-input-list w))
                           (r1 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 4)))))
                           (r2 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 5)))))
                           (r3 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 6)))))
                           (new-input-list (list-set (list-set (list-set (world-input-list w) 6 (remove-text (list-ref (world-input-list w) 6) 100)) 5 (remove-text (list-ref (world-input-list w) 5) 100)) 4 (remove-text (list-ref (world-input-list w) 4) 100))))
                       (cond
                         [(or (equal? r1 '||) (equal? r2 '||) (equal? r3 '||)) (redraw-world w)]
                         [else
                          (begin
                            (set-machine-rule-list! (world-fsm-machine w) (remove (list (format-input r1) r2 (format-input r3)) (machine-rule-list (world-fsm-machine w))))
                            (create-new-world-input w new-input-list))]))))

;; addState: world -> world
;; Purpose: Adds a start state to the world
(define addStart (lambda(w)
                   (let
                       ((start-state (string-trim(textbox-text(list-ref (world-input-list w) 2))))
                        (new-input-list (list-set (world-input-list w) 2 (remove-text (list-ref(world-input-list w) 2) 100))))
                    
                     (cond
                       [(equal? "" start-state) (redraw-world w)]
                       [(and (null? (machine-start-state (world-fsm-machine w))) (ormap(lambda(x) (equal? (string->symbol start-state) x)) (machine-state-list (world-fsm-machine w))))
                        (begin
                          (set-machine-start-state! (world-fsm-machine w) (string->symbol start-state))
                          (world (world-fsm-machine w)(world-tape-position w) (world-cur-rule w)
                                 (string->symbol start-state) (world-button-list w) new-input-list
                                 (world-processed-config-list w) (world-unporcessed-config-list w) (world-error-msg w) (world-scroll-bar-index w)))]
                       [ (null? (machine-start-state (world-fsm-machine w)))
                         (begin
                           (set-machine-state-list! (world-fsm-machine w) (cons (string->symbol start-state) (machine-state-list (world-fsm-machine w))))
                           (set-machine-start-state! (world-fsm-machine w) (string->symbol start-state))
                           (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w)
                                  (string->symbol start-state) (world-button-list w) new-input-list
                                  (world-processed-config-list w) (world-unporcessed-config-list w) (world-error-msg w) (world-scroll-bar-index w)))]
                       [ (ormap (lambda (x) (equal? start-state x)) (machine-state-list (world-fsm-machine w)))
                         (begin
                           (set-machine-start-state! (world-fsm-machine w) (string->symbol start-state))
                           (world (world-fsm-machine w)(world-tape-position w) (world-cur-rule w)
                                  (string->symbol start-state) (world-button-list w) new-input-list
                                  (world-processed-config-list w) (world-unporcessed-config-list w)(world-error-msg w) (world-scroll-bar-index w)))]
                       [else w]))))


;; replaceStart: world -> world
;; Purpose: Replaces the start state in the world
(define replaceStart (lambda(w)
                       (let
                           ((start-state (string-trim(textbox-text(list-ref (world-input-list w) 2))))
                            (new-input-list (list-set (world-input-list w) 2 (remove-text (list-ref (world-input-list w) 2) 100))))
                         (cond
                           [(equal? "" start-state) (redraw-world w)]
                           
                           [ (ormap (lambda (x) (equal? (string->symbol start-state) x)) (machine-state-list (world-fsm-machine w)))
                             (begin
                               (set-machine-start-state! (world-fsm-machine w) (string->symbol start-state))
                               (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w)
                                      (string->symbol start-state) (world-button-list w) new-input-list
                                      (world-processed-config-list w) (world-unporcessed-config-list w) (world-error-msg w) (world-scroll-bar-index w)))]
                           
                           [else
                            (begin
                              (set-machine-state-list! (world-fsm-machine w) (cons (string->symbol start-state) (machine-state-list (world-fsm-machine w))))
                              (set-machine-start-state! (world-fsm-machine w) (string->symbol start-state))
                              (world (world-fsm-machine w)(world-tape-position w) (world-cur-rule w)
                                     (string->symbol start-state) (world-button-list w) new-input-list
                                     (world-processed-config-list w) (world-unporcessed-config-list w) (world-error-msg w) (world-scroll-bar-index w)))]))))

;; addEnd: world -> world
;; Purpose: Adds an end state to the world
(define addEnd (lambda(w)
                 (let
                     ((end-state (string-trim (textbox-text(list-ref (world-input-list w) 3))))
                      (new-input-list (list-set (world-input-list w) 3 (remove-text (list-ref (world-input-list w) 3) 100))))
                   (cond
                     [(equal? "" end-state) (redraw-world w)]
                     [(ormap (lambda (x) (equal? x (string->symbol end-state))) (machine-state-list (world-fsm-machine w)))
                      (begin
                        (set-machine-final-state-list! (world-fsm-machine w) (remove-duplicates (cons (string->symbol end-state) (machine-final-state-list (world-fsm-machine w)))))
                        (create-new-world-input w new-input-list))]
                     [else
                      (begin
                        (set-machine-state-list! (world-fsm-machine w) (cons (string->symbol end-state) (machine-state-list (world-fsm-machine w))))
                        (set-machine-final-state-list! (world-fsm-machine w) (remove-duplicates (cons (string->symbol end-state) (machine-final-state-list (world-fsm-machine w)))))
                        (create-new-world-input w new-input-list))]))))
                      

;; rmvEnd: world -> world
;; Purpose: removes a end state from the world-final-state-list
(define rmvEnd (lambda (w)
                 (let
                     ((end-state (string-trim(textbox-text(list-ref (world-input-list w) 3))))
                      (new-input-list (list-set (world-input-list w) 3 (remove-text (list-ref (world-input-list w) 3) 100))))
                   (cond
                     [(equal? "" end-state) (redraw-world w)]
                     [(ormap (lambda(x) (equal? x (string->symbol end-state))) (machine-state-list (world-fsm-machine w)))
                      (begin
                        (set-machine-final-state-list! (world-fsm-machine w) (remove (string->symbol end-state) (machine-final-state-list (world-fsm-machine w))))
                        (create-new-world-input w new-input-list))]
                     [else
                      (begin
                        (set-machine-final-state-list! (world-fsm-machine w) (cons (string->symbol end-state) (machine-final-state-list (world-fsm-machine w))))
                        (create-new-world-input w new-input-list))]))))
                      

;; addAlpha: world -> world
;; Purpose: Adds a letter to the worlds alpha-list
(define addAlpha (lambda (w)
                   (let ((input-value (string-trim (textbox-text(list-ref (world-input-list w) 1))))
                         (new-input-list (list-set (world-input-list w) 1 (remove-text (list-ref (world-input-list w) 1) 100))))

                     (cond
                       [(equal? input-value "") (redraw-world w)]
                       [else
                        (begin
                          (set-machine-alpha-list! (world-fsm-machine w) (sort (remove-duplicates (cons (string->symbol input-value) (machine-alpha-list (world-fsm-machine w)))) symbol<?))
                          (create-new-world-input w new-input-list))]))))

;; rmvAlpha: world -> world
;; Purpose: Removes a letter from the worlds alpha-list
(define rmvAlpha (lambda (w)
                   (let ((input-value (string-trim (textbox-text(list-ref (world-input-list w) 1))))
                         (new-input-list (list-set (world-input-list w) 1 (remove-text (list-ref (world-input-list w) 1) 100)))

                         ;; remove-all: list-of-rules symbol -> list-of-rules
                         ;; Purpose: Removes all rules that are associated with the alpha that is being removed.
                         (remove-all (lambda (lor alpha)
                                       (filter (lambda (x) (cond
                                                             [(equal? (symbol->string (cadr x)) alpha) #f]
                                                             [else #t]))
                                               lor))))
                     
                     (cond
                       [(equal? input-value "") (redraw-world w)]
                       [else
                        (begin
                          (set-machine-alpha-list! (world-fsm-machine w) (sort (remove (string->symbol input-value) (machine-alpha-list (world-fsm-machine w))) symbol<?))
                          (set-machine-rule-list! (world-fsm-machine w) (remove-all (machine-rule-list (world-fsm-machine w)) input-value))
                          (create-new-world-input w new-input-list))]))))

;; addSigma: world -> world
;; Purpose: adds a letter or group of letters to the sigma list
(define addSigma (lambda (w)
                   (letrec ((input-value (string-trim (textbox-text(list-ref (world-input-list w) 7))))

                            ;; real-string->list: string -> list-of-symbols
                            ;; Purpose: converts a string to a list. Unlike Racket's string->list, this function converts every element of the
                            ;; list to a string as opposed to a char.
                            (real-string->list (lambda (str)
                                                 (letrec (;; convert-to-list: string list -> list-of-symbols
                                                          ;; Purpose: this function uses an accumulator to accumulate all elements of the string converted to a list  
                                                          (convert-to-list (lambda (str accum) 
                                                                             (cond
                                                                               [(< (string-length str) 1) accum]
                                                                               [(equal? (substring str 0 1) " ") (convert-to-list (substring str 1) accum)]
                                                                               [else (convert-to-list (substring str 1) (cons (string->symbol (substring str 0 1)) accum))]))))
                                                   (convert-to-list str '()))))

                            ;; check-alpha: list-of-alpha list-of-sigma -> boolean
                            ;; Purpose: Determins if all elements of sigma are in alpha. If they are then returns true, otherwise retunrs false
                            (check-alpha (lambda (loa los)
                                           (letrec (;; check-lists: list list -> boolean
                                                    ;; Purpose: given two list will check to see if the elements of list2 are in list1
                                                    (check-lists (lambda (list1 list2)
                                                                   (cond
                                                                     [(empty? list2) #t]
                                                                     [(equal? (member (car list2) list1) #f) #f]
                                                                     [else (check-lists list1 (cdr list2))]))))
                                             (cond
                                               [(empty? loa) #f]
                                               [(empty? los) #f]
                                               [else (check-lists loa los)]))))
                            (new-input-list (list-set (world-input-list w) 7 (remove-text (list-ref (world-input-list w) 7) 100))) 
                            (sigma-list (reverse (real-string->list input-value))))

                     (cond
                       [(equal? (check-alpha (machine-alpha-list (world-fsm-machine w)) sigma-list) #f) (redraw-world w)]
                       [(equal? input-value "") (redraw-world w)]
                       [(> (string-length input-value) 0)

                        ;; Check if the unprocessed list and processed lists are empty... If they are we know run code was never pressed
                        ;; If they are not empty then run code was pressed. If run code was pressed then we know to update the gui to handle to new sigma
                        ;; so we will call run code from here.
                        (cond
                          [(empty? (and (world-unporcessed-config-list w) (world-processed-config-list w)))
                           (begin
                             (set-machine-sigma-list! (world-fsm-machine w) (append sigma-list (machine-sigma-list (world-fsm-machine w))))
                             (create-new-world-input w new-input-list))]
                          [else
                           (begin
                             (set-machine-sigma-list! (world-fsm-machine w) (append sigma-list (machine-sigma-list (world-fsm-machine w))))
                             (runProgram (create-new-world-input w new-input-list)))])]
                       [else (redraw-world w)]))))


;; clearSigma: world -> world
;; Purpose: Removes all elements of the sigma list
(define clearSigma (lambda (w)
                     (let ((new-input-list (list-set (world-input-list w) 7 (remove-text (list-ref (world-input-list w) 7) 100))))
                       (begin
                         (set-machine-sigma-list! (world-fsm-machine w) '())
                         (create-new-world-input w new-input-list)))))


;; runProgram: world -> world
;; Purpose: Calles sm-showtransitons on the world machine. If it is valid then the next and prev buttons will work and the user can use the program
(define runProgram(lambda (w)
                    (let (
                          (fsm-machine (world-fsm-machine w)))
                      (cond
                        [(equal? #t (check-machine (machine-state-list fsm-machine) (machine-alpha-list fsm-machine) (machine-final-state-list fsm-machine) (machine-rule-list fsm-machine) (machine-start-state fsm-machine) (machine-type fsm-machine)))
                         (let ((unprocessed-list (sm-showtransitions (make-dfa (machine-state-list (world-fsm-machine w))
                                                                               (machine-alpha-list (world-fsm-machine w))
                                                                               (machine-start-state (world-fsm-machine w))
                                                                               (machine-final-state-list (world-fsm-machine w))
                                                                               (machine-rule-list (world-fsm-machine w)))
                                                                     (machine-sigma-list (world-fsm-machine w)))))
                           (world (world-fsm-machine w) (world-tape-position w) CURRENT-RULE
                                  (machine-start-state (world-fsm-machine w)) (world-button-list w) (world-input-list w)
                                  (list (car unprocessed-list)) (cdr unprocessed-list)
                                  (msgWindow "The machine was sucessfuly Built. Press Next and Prev to show the machine's transitions" "Success" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)
                                  0))]
                        [else
                         (redraw-world-with-msg w "The Machine failed to build. Please see the cmd for more info" "Error" MSG-ERROR)]))))


;; genCode: world -> world
;; Purpose: Exports the GUI machine to a external file called: 'fsmGUIFunctions.rkt'
(define genCode (lambda (w)
                  (letrec(
                          ;; The machine type ('dfa, 'ndfa, 'pda, ect...)
                          (type (machine-type (world-fsm-machine w)))
                          
                          (fsm-machine (world-fsm-machine w)) ;; The machine for the world
                          ;; nameGen: null -> symbol
                          ;; Purpose: generates a random name that consists of one capital letter(A-Z) and one number(1-9)
                          ;;   for the mechine being created.
                          (nameGen (lambda ()
                                     (let ((letter (string (integer->char (random 65 91))))
                                           (number (string (integer->char (random 48 58)))))
                                       (string->symbol (string-append letter number)))))

                          ;; construct-machine: list-of-states symbol list-of-finals list-of-rules list-of-alpha symbol
                          ;; Purpose: Constructs the code to create the specified machine
                          (construct-machine (lambda (states start finals rules alpha type)
                                               (letrec (
                                                        ;; nameGen: null -> symbol
                                                        ;; Purpose: generates a random name that consists of one capital letter(A-Z) and one number(1-9)
                                                        ;;   for the mechine being created.
                                                        (nameGen (lambda ()
                                                                   (let ((letter (string (integer->char (random 65 91))))
                                                                         (number (string (integer->char (random 48 58)))))
                                                                     (string->symbol (string-append letter number))))))
                      
                                                 (case type
                                                   [(dfa) `(define ,(nameGen) (make-dfa (quote (,@states)) (quote (,@alpha)) (quote ,start) (quote ,finals) (quote (,@rules))))]
                                                   [(ndfa) `(define ,(nameGen) (make-ndfa (quote (,@states)) (quote (,@alpha)) (quote ,start) (quote ,finals) (quote (,@rules))))]
                                                   [(pda) (println "TODO ADD PDA")]
                                                   [(dfst) (println "TODO ADD DFST")]
                                                   [else (error (format "The machine type: ~s, is not currently supported" type))]))))

                          ;; write-to-file: string quasiquote boolean -> File
                          ;; Purpose writes the comments and code to create a machine in the specified file.
                          ;;   If the file does not exist it will create a file in the users current directory. If the file
                          ;;   does exist then it adds the supplied args to the end of the file.
                          (write-to-file (lambda (file machine status)
                                           (letrec (
                                                    ;; comments: boolean -> string
                                                    ;; Purpose: Creates the comments for the machine
                                                    (comments (lambda (status)
                                                                (letrec (
                                                                         (d (current-date)) ;; The current date
                                                                         (formatt-date (string-append (number->string (date-month d)) "/" (number->string (date-day d)) "/" (number->string (date-year d)))) ;; formatted date in form: mm/dd/yyyy
                                                                         (formatt-date-minute (if (< (date-minute d) 10) (string-append "0" (number->string (date-minute d))) (number->string (date-minute d))))
                                
                                                                         ;; formatt-time: null -> string
                                                                         ;; Purpose: formatts military time into the from hh:mm:am/pm
                                                                         (formatt-time (lambda ()
                                                                                         (cond
                                                                                           [(and (> (date-hour d) 12) (< (date-hour d) 24)) (string-append (number->string (- (date-hour d) 12)) ":" formatt-date-minute "pm")]
                                                                                           [(equal? (date-hour d) 12) (string-append (number->string (date-hour d)) ":" formatt-date-minute "pm")]
                                                                                           [(equal? (date-hour d) 24) (string-append (number->string (- (date-hour d) 12)) ":" formatt-date-minute "am")]
                                                                                           [else (string-append (number->string (date-hour d)) ":" formatt-date-minute "am")]))))
                                                                  (cond
                                                                    [status (string-append ";; Created by fsm-GUI on: " formatt-date " at " (formatt-time)"\n;; This machine passed all tests.")]
                                                                    [else (string-append ";; Created by fsm-GUI on: " formatt-date " at " (formatt-time)"\n;; WARNIMG: this machine failed to build!")])))))
                                             (cond
                                               [(file-exists? file) (call-with-output-file file
                                                                      #:exists 'append
                                                                      (lambda (out)
                                                                        (displayln "" out)
                                                                        (displayln (comments status) out)
                                                                        (displayln machine out)))]
                                               [else (call-with-output-file file
                                                       (lambda (out)
                                                         (displayln "#lang Racket" out)
                                                         (displayln "(require fsm)" out)
                                                         (displayln "" out)
                                                         (displayln (comments status) out)
                                                         (displayln machine out)))])))))

                    (cond
                      [(equal? #t (check-machine (machine-state-list fsm-machine) (machine-alpha-list fsm-machine) (machine-final-state-list fsm-machine) (machine-rule-list fsm-machine) (machine-start-state fsm-machine) (machine-type fsm-machine)))
                       (let (
                             (fsm-machine (world-fsm-machine w)))
                         (begin
                           (write-to-file
                            "fsmGUIFunctions.rkt"
                            (construct-machine
                             (machine-state-list fsm-machine)
                             (machine-start-state fsm-machine)
                             (machine-final-state-list fsm-machine)
                             (machine-rule-list fsm-machine)
                             (machine-alpha-list fsm-machine)
                             (machine-type fsm-machine))
                            #t)
                           (redraw-world-with-msg w (string-append "The machine was sucessfuly built and exported to fsmGUIFunctions.rkt. This file can be found at: ~n " (path->string (current-directory))) "Success!" MSG-SUCCESS)))]

                      [else
                       (begin
                         (write-to-file
                          "fsmGUIFunctions.rkt"
                          (construct-machine
                           (machine-state-list fsm-machine)
                           (machine-start-state fsm-machine)
                           (machine-final-state-list fsm-machine)
                           (machine-rule-list fsm-machine)
                           (machine-alpha-list fsm-machine)
                           (machine-type fsm-machine))
                          #f)
                         (redraw-world-with-msg w (string-append "The machine built with errors! Please see the cmd for more info. ~n ~n The machine was exported to fsmGUIFunctions.rkt. This file can be found at: ~n " (path->string (current-directory)) "Please fix the erros and press Gen Code again.") "Error" MSG-ERROR))]))))



;; getCurRule: processed-list -> rule
;; Purpose: get the rule that the machine just executed
(define getCurRule (lambda (pl)
                     (cond
                       [(< (length pl) 2) (list 'empty 'empty 'empty)] ;; If the processed list doesn't have at least 2 items in it then no rule was followed...
                       [else
                        (list
                         (cadadr pl)
                         (caaadr pl)
                         (cadar pl))])))



;; getScrollBarPosition: list-of-rules rule -> int
;; Purpose: Trys to place the currently highlighted rule at the beginning of the scrollbar. If not possiable moves to scrollbar to a position
;;      where the rule will be sceen by the user.
(define getScrollBarPosition (lambda (lor rule)
                               (let ((ruleIndex (index-of lor rule)))
                                 (cond
                                   ;; See if there is no current rule. If so return the starting index of the scrollbar
                                   [(equal? rule '(empty empty empty)) 0]
                                   ;; If true then we set the scroll index to max
                                   [(> (+ ruleIndex 10) (- (length lor) 1)) (- (length lor) 10)]
                                   ;; Otherwise return the rule index
                                   [else ruleIndex]))))

                  

;; showNext: world -> world
;; Purpose: shows the next state that the machine is in
(define showNext(lambda(w)
                  ;; Check if sigma list is empty
                  (cond
                    [(empty? (machine-sigma-list (world-fsm-machine w))) (redraw-world-with-msg w "Your Tape is currently empty! Please add variables to the Tap to continue." "Notice" MSG-CAUTION)]
                    [else
                     ;; Check if the unprocessed list is empty. If so then gencode was not yet pressed
                     (cond
                       [(empty? (world-unporcessed-config-list w)) (redraw-world-with-msg w "You must build your machine before you can continue. Please press Run to proceed." "Error" MSG-CAUTION)]
                       [else
                        (let(
                             (nextState (car (world-unporcessed-config-list w)))
                             (transitions (cdr (world-unporcessed-config-list w))))
                    
                          (cond
                            [(eq? nextState 'accept)
                             (redraw-world-with-msg w "The input was accepted." "Success" MSG-SUCCESS)]
                            [(eq? nextState 'reject)
                             (redraw-world-with-msg w "The input was rejected." "Notice" MSG-CAUTION)]
                            [else
                             (world (world-fsm-machine w) (world-tape-position w) (getCurRule (append (list nextState) (world-processed-config-list w)))
                                    (car (cdr nextState)) (world-button-list w) (world-input-list w)
                                    (append (list nextState) (world-processed-config-list w)) transitions (world-error-msg w)
                                    (getScrollBarPosition (reverse (machine-rule-list (world-fsm-machine w))) (getCurRule (append (list nextState) (world-processed-config-list w)))))]))])])))

;; showPrev: world -> world
;; shows the previous state that the machine was in
(define showPrev (lambda(w)
                   (cond
                     [(empty? (world-processed-config-list w)) (redraw-world-with-msg w "The tape is currently empty. Please add variables to the tape, then press Run and try again" "Notice" MSG-CAUTION)]
                     [(empty? (cdr (world-processed-config-list w))) (redraw-world-with-msg w "You have reached the beginning of the machine! There are no more previous states." "Notice" MSG-CAUTION)]
                     [else
                      (let(
                           (previousState (car (cdr (world-processed-config-list w)))))
                        (world (world-fsm-machine w) (world-tape-position w) (getCurRule (cdr (world-processed-config-list w)))
                               (car (cdr previousState)) (world-button-list w) (world-input-list w)
                               (cdr (world-processed-config-list w)) (cons (car (world-processed-config-list w)) (world-unporcessed-config-list w)) (world-error-msg w)
                               (getScrollBarPosition (reverse (machine-rule-list (world-fsm-machine w))) (getCurRule (cdr (world-processed-config-list w))))))])))

;; scrollbarRight: world -> world
;; Purpose: moves the scroll bar over 1 place to the right
(define scrollbarRight (lambda (w)
                         (let ((index (world-scroll-bar-index w)))
                           (cond
                             [(< (length (list-tail (machine-rule-list (world-fsm-machine w)) (add1 index))) 10) (redraw-world w)]
                             [else

                              (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w) (world-cur-state w) (world-button-list w)
                                     (world-input-list w) (world-processed-config-list w)(world-unporcessed-config-list w) (world-error-msg w) (add1 index))]))))

;; scrollbarLeft: world -> world
;; Purpose: moves the scroll bar over 1 place to the left
(define scrollbarLeft (lambda (w)
                        (let ((index (world-scroll-bar-index w)))
                          (cond
                            [(< (- index 1) 0)(redraw-world w)]
                            [else

                             (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w) (world-cur-state w) (world-button-list w)
                                    (world-input-list w) (world-processed-config-list w)(world-unporcessed-config-list w) (world-error-msg w) (sub1 index))]))))
                        
                        

;; **** BUTTONS BELOW ***
(define BTN-ADD-STATE (button 70 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 150) (- CONTROL-BOX-H 25)) addState))
(define BTN-REMOVE-STATE (button 70 25 "Remove" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 50) (- CONTROL-BOX-H 25)) removeState))

(define BTN-ADD-ALPHA (button 70 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 150) (- (* 2 CONTROL-BOX-H) 25)) addAlpha))
(define BTN-REMOVE-ALPHA (button 70 25 "Remove" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 50) (- (* 2 CONTROL-BOX-H ) 25)) rmvAlpha))

(define BTN-ADD-START (button 50 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 71)) addStart))
(define BTN-REMOVE-START (button 50 25 "Replace" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 25)) replaceStart))

(define BTN-ADD-END (button 50 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 71)) addEnd))
(define BTN-REMOVE-END (button 50 25 "Remove" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 25)) rmvEnd))

(define BTN-ADD-RULES (button 70 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 25)) addRule))
(define BTN-REMOVE-RULES (button 70 25 "Remove" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 25)) removeRule))
(define BTN-SCROLL-LEFT-RULES (button 30 BOTTOM "<" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 36 #f #f (posn 125 (- HEIGHT 37)) scrollbarLeft))
(define BTN-SCROLL-RIGHT-RULES (button 30 BOTTOM ">" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 36 #f #f (posn (- WIDTH 215) (- HEIGHT 37)) scrollbarRight))



(define BTN-RUN (button 95 30 "Run" "solid" (make-color 4 120 40) (make-color 4 120 40) 25 #f #f (posn 55 105) runProgram))
(define BTN-HELP (button 20 20 "?" "solid" (make-color 39 168 242) (make-color 39 168 242) 15 #t #f (posn 130 80) openHelp))

(define BTN-NEXT (button 95 30 "NEXT =>" "solid" (make-color 252 130 73) (make-color 252 130 73) 25 #f #f (posn 55 140) showNext))
(define BTN-PREV (button 95 30 "<= PREV" "solid" (make-color 252 130 73) (make-color 252 130 73) 25 #f #f (posn 55 175) showPrev))
(define BTN-GENCODE (button 95 50 "GEN CODE" "solid" (make-color 240 79 77) (make-color 240 79 77) 30 #f #f (posn 55 220) genCode))

(define BTN-SIGMA-ADD (button 40 25 "ADD" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 20 #f #f (posn 30 70) addSigma))
(define BTN-SIGMA-CLEAR (button 40 25 "CLEAR" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 20 #f #f (posn 80 70) clearSigma))

;; BUTTON-LIST: A List containing all buttons that are displayed on the scene.
(define BUTTON-LIST (list BTN-ADD-STATE BTN-REMOVE-STATE
                          BTN-ADD-ALPHA BTN-REMOVE-ALPHA
                          BTN-ADD-START BTN-REMOVE-START
                          BTN-ADD-END BTN-REMOVE-END
                          BTN-ADD-RULES BTN-REMOVE-RULES
                          BTN-GENCODE BTN-NEXT BTN-PREV
                          BTN-SIGMA-ADD BTN-SIGMA-CLEAR
                          BTN-RUN BTN-SCROLL-LEFT-RULES
                          BTN-SCROLL-RIGHT-RULES BTN-HELP))



;; **** INPUT FIELDS BELOW ****
(define IPF-STATE (textbox 150 25 INPUT-COLOR INPUT-COLOR "" 5 (posn (- WIDTH 100) (- CONTROL-BOX-H 70)) #f))
(define IPF-ALPHA (textbox 150 25 INPUT-COLOR INPUT-COLOR "" 10 (posn (- WIDTH 100) (- (* 2 CONTROL-BOX-H) 70)) #f))
(define IPF-START (textbox 75 25 INPUT-COLOR INPUT-COLOR "" 10 (posn (- WIDTH 150) (- (* 3 CONTROL-BOX-H) 50)) #f))
(define IPF-END (textbox 75 25 INPUT-COLOR INPUT-COLOR "" 10 (posn (- WIDTH 150) (- (* 4 CONTROL-BOX-H) 50)) #f))
(define IPF-RULE1 (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 70)) #f))
(define IPF-RULE2 (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 100) (- (* 5 CONTROL-BOX-H) 70)) #f))
(define IPF-RULE3 (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 70)) #f))
(define IPF-SIGMA (textbox 90 25 INPUT-COLOR INPUT-COLOR "" 100 (posn (/ (/ WIDTH 11) 2) 40) #f))

;; INPUT-LIST: A list containing all input fields that are displayed on the scene.
(define INPUT-LIST (list IPF-STATE IPF-ALPHA IPF-START IPF-END IPF-RULE1 IPF-RULE2 IPF-RULE3 IPF-SIGMA))


;; create-init-world: machine msgWindow(optional) -> world
;; Purpose: Creates the initail world with the given machine
(define (create-init-world m . msg)
  (cond
    [(null? msg) (world m TAPE-POSITION CURRENT-RULE CURRENT-STATE BUTTON-LIST INPUT-LIST PROCESSED-CONFIG-LIST UNPROCESSED-CONFIG-LIST null INIT-INDEX)]
    [else (world m TAPE-POSITION CURRENT-RULE CURRENT-STATE BUTTON-LIST INPUT-LIST PROCESSED-CONFIG-LIST UNPROCESSED-CONFIG-LIST (car msg) INIT-INDEX)]))



;; cmd functions

;; visualize: fsm-machine -> world
;; Purpose: allows a user to pre-load a machine
(define (visualize fsm-machine)
  (letrec ((run-program (lambda (w)
                          (big-bang
                              w
                            (name SCENE-TITLE)
                            (on-draw draw-world)
                            (on-mouse process-mouse-event)
                            (on-key process-key)))))
    
    ;; check if it is a pre-made machine or a brand new one
    (cond
      [(symbol? fsm-machine)
       (case fsm-machine
         [(dfa) (run-program (create-init-world (machine '() null '() '() '() '() 'dfa )))]
         [(ndfa) (run-program (create-init-world (machine '() null '() '() '() '() 'ndfa )))]
         [(pda) (println "TODO ADD PDA")]
         [(dfst) (println "TODO ADD DFST")]
         [else (error (format "~s is not a valid machine type" fsm-machine))])]
      [else
    
       (case (sm-type fsm-machine)
         [(dfa) (run-program (create-init-world (machine (map (lambda (x) (fsm-state x (lambda(v) v) (posn 0 0))) (sm-getstates fsm-machine)) (sm-getstart fsm-machine) (sm-getfinals fsm-machine)
                                                         (reverse (sm-getrules fsm-machine)) '() (sm-getalphabet fsm-machine) (sm-type fsm-machine))
                                                (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press Gen Code to start simulation." "dfa" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))]
         
         [(ndfa) (run-program (create-init-world (machine (sm-getstates fsm-machine) (sm-getstart fsm-machine) (sm-getfinals fsm-machine)
                                                          (reverse (sm-getrules fsm-machine)) '() (sm-getalphabet fsm-machine) (sm-type fsm-machine))
                                                 (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press Gen Code to start simulation." "ndfa" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))]
         [(pda) (println "TODO ADD PDA")]
         [(dfst) (println "TODO ADD DFST")])])))
  


;; draw-world: world -> world
;; Purpose: draws the world every time on-draw is called
(define (draw-world w)
  (letrec(
          ;; draw-input-list: list-of-inputs sceen -> sceen
          ;; Purpose: draws every input structure from the list onto the given sceen
          (draw-input-list (lambda (loi scn)
                             (cond
                               [(empty? loi) scn]
                               [else (draw-textbox (car loi) (draw-input-list (cdr loi) scn))])))
          
          ;; draw-button-list: list-of-buttons sceen -> sceen
          ;; Purpose: draws every button structure from the list onto the given sceen
          (draw-button-list (lambda (lob scn)
                              (cond
                                [(empty? lob) scn]
                                [else (draw-button (car lob) (draw-button-list (cdr lob) scn))])))
          
          ;; draw-error-msg: msgWindow sceen -> sceen
          ;; Purpose: renders the error message onto the screen if there is one.
          (draw-error-msg (lambda (window scn)
                            (cond
                              [(null? window) scn]
                              [else (draw-window window scn WIDTH HEIGHT)])))
          
          (deg-shift (if (empty? (machine-state-list (world-fsm-machine w))) 0 (/ 360 (length (machine-state-list (world-fsm-machine w))))))
          
          (get-x (lambda (theta rad) (truncate (+ (* rad (cos (degrees->radians theta))) X0))))
                
          (get-y(lambda (theta rad)
                  (truncate (+ (* rad (sin (degrees->radians theta))) Y0))))
          (current-index (if (null? (world-cur-state w)) 0 (index-of (machine-state-list (world-fsm-machine w)) (world-cur-state w))))
          (tip-x (get-x (* deg-shift current-index) inner-R))
          (tip-y(get-y (* deg-shift current-index) inner-R))
          (the-arrow(rotate 180 (triangle 15 "solid" "tan")))
          (find-state-pos
           (λ(l i) (if (empty? l) (void)
                       (begin
                         (set-fsm-state-posn! (car l) (posn (get-x (* deg-shift i) R) (get-y (* deg-shift i) R)))
                         (find-state-pos (cdr l) (add1 i))))))
          
          ;;draw-states: list-of-states index scene -> scene
          ;; Purpose: Draws the states onto the GUI
          (draw-states (lambda (l i s)
                         (begin
                           (find-state-pos (machine-state-list (world-fsm-machine w)) 0)
                           (cond[(empty? l) s]
                                [(equal? (fsm-state-name (car l)) (machine-start-state (world-fsm-machine w)))
                                 (place-image(overlay (text (symbol->string (fsm-state-name (car l))) 25 START-STATE-COLOR)
                                                      (circle 25 "outline" START-STATE-COLOR))
                                             (posn-x (fsm-state-posn (car l)))
                                             (posn-y (fsm-state-posn (car l)))
                                             (draw-states(cdr l) (add1 i) s))]
                                [(ormap (lambda(x) (equal? (fsm-state-name (car l)) x)) (machine-final-state-list (world-fsm-machine w)))
                                 (place-image (overlay (text (symbol->string (fsm-state-name (car l))) 20 "red")
                                                       (overlay
                                                        (circle 20 "outline" END-STATE-COLOR)
                                                        (circle 25 "outline" END-STATE-COLOR)))
                                              (posn-x (fsm-state-posn (car l)))
                                              (posn-y (fsm-state-posn (car l)))
                                              (draw-states (cdr l) (add1 i) s))]
                                [else (place-image (text  (symbol->string (fsm-state-name (car l))) 25 "black")
                                                   (posn-x (fsm-state-posn  (car l)))
                                                   (posn-y (fsm-state-posn (car l)))
                                                   (draw-states (cdr l) (add1 i) s))])))))
         
    (if (not (null? (world-cur-state w)))
        (draw-error-msg (world-error-msg w)(place-image pointer-square X0 Y0 (place-image pointer-circle tip-x tip-y (add-line (place-image the-circle X0 Y0 (draw-states (machine-state-list (world-fsm-machine w)) 0 
                                                                                                                                                                          (place-image (create-gui-left) (- WIDTH 100) (/ HEIGHT 2)
                                                                                                                                                                                       (place-image (create-gui-top (machine-sigma-list (world-fsm-machine w))) (/ WIDTH 2) (/ TOP 2)
                                                                                                                                                                                                    (place-image (create-gui-bottom (machine-rule-list (world-fsm-machine w)) (world-cur-rule w) (world-scroll-bar-index w)) (/ WIDTH 2) (- HEIGHT (/ BOTTOM 2))
                                                                                                                                                                                                                 (draw-button-list (world-button-list w)
                                                                                                                                                                                                                                   (draw-input-list (world-input-list w)
                                                                                                                                                                                                                                                    (place-image (create-gui-alpha (machine-alpha-list (world-fsm-machine w))) (/ (/ WIDTH 11) 2) (/ (- HEIGHT BOTTOM) 2) MAIN-SCENE))))))))
                                                                                                                               X0 Y0 tip-x tip-y state-pen))))
        
        (draw-error-msg (world-error-msg w) (place-image the-circle X0 Y0 (draw-states (machine-state-list (world-fsm-machine w)) 0 
                                                                                       (place-image (create-gui-left) (- WIDTH 100) (/ HEIGHT 2)
                                                                                                    (place-image (create-gui-top (machine-sigma-list (world-fsm-machine w))) (/ WIDTH 2) (/ TOP 2)
                                                                                                                 (place-image (create-gui-bottom (machine-rule-list (world-fsm-machine w)) (world-cur-rule w) (world-scroll-bar-index w)) (/ WIDTH 2) (- HEIGHT (/ BOTTOM 2))
                                                                                                                              (draw-button-list (world-button-list w)
                                                                                                                                                (draw-input-list (world-input-list w)
                                                                                                                                                                 (place-image (create-gui-alpha (machine-alpha-list (world-fsm-machine w))) (/ (/ WIDTH 11) 2) (/ (- HEIGHT BOTTOM) 2) MAIN-SCENE))))))))))))


;; top-input-label: null -> image
;; Purpose: Creates the top left input lable
(define (top-input-label)
  (overlay/align "right" "top"
                 (control-header3 "Tape Input")
                 (rectangle (/ WIDTH 11) TOP "outline" "transparent")))


;; los-top-label: null -> Image
;; Purpose: Creates the top list of sigmas lable
(define (los-top-label los)
  (letrec ((list-to-string (lambda (lor)
                             (cond
                               [(empty? lor) ""]
                               [else (string-append (symbol->string (car lor)) " " (list-to-string (cdr lor)))]))))
    (scale-text-to-image (text (list-to-string los) 24 "Black") (rectangle (- (- WIDTH (/ WIDTH 11)) 200) TOP "outline" "blue") 1)))


;; create-gui-bottom: list-of-rules rule int -> image
;; Purpose: Creates the bottom of the gui layout
(define (create-gui-bottom lor cur-rule scroll-index)
  (cond
    [(empty? lor) (overlay/align "left" "middle"
                                 (align-items
                                  (rules-bottom-label)
                                  (rectangle (- (- WIDTH (/ WIDTH 11)) 200) BOTTOM "outline" "blue"))
                                 (rectangle WIDTH BOTTOM "outline" "transparent"))]
    [else 
     (overlay/align "left" "middle"
                    (align-items
                     (rules-bottom-label)
                     (lor-bottom-label lor 83 cur-rule scroll-index))
                    (rectangle WIDTH BOTTOM "outline" "transparent"))]))


;; rules-bottom-label: null -> image
;; Purpose: Creates the left bottom label in the gui
(define (rules-bottom-label)
  (overlay
   (text (string-upcase "Rules:") 24 "Black")
   (rectangle (/ WIDTH 11) BOTTOM "outline" "blue")))

;; create-gui-top: null -> image
;; Creates the top of the gui layout
(define (create-gui-top los)
  (overlay/align "left" "middle"
                 (beside
                  (top-input-label)
                  (los-top-label los))
                 (rectangle WIDTH TOP "outline" "transparent")))

;; align-items image image -> image
;; Purpose: Aligns 2 images next to each other
(define (align-items item1 item2)
  (beside
   item1
   item2))

;; lor-bottom-label: list-of-rules int rule int -> image
;; Purpose: The label for the list of rules
(define (lor-bottom-label lor rectWidth cur-rule scroll-index)
  (letrec (;; list-2-img: list-of-rules int int -> image
           ;; Purpose: Converts the list of rules into image that overlays the rul in the center
           (list-2-img (lambda (lor rectWidth accum)
                         (cond
                           [(>= accum 10) empty-image]
                           [(empty? lor) null]
                           [(equal? 1 (length lor)) (rule-box (inner-list-2-img (car lor) "") 18)]
                           [(beside
                             (rule-box (inner-list-2-img (car lor) "") 18)
                             (list-2-img (cdr lor) rectWidth (add1 accum)))])))

           ;; inner-list-2-img: rule (tuple) string -> image
           ;; Purpose: Converts a rule into an image were the rule is overlayed on an image
           (inner-list-2-img (lambda (tup accum)
                               (cond
                                 [(empty? tup) (string-append "(" (string-append (substring accum 1 (string-length accum)) ")"))]
                                 [else (inner-list-2-img (cdr tup) (string-append accum " " (symbol->string (car tup))))])))
           

           ;; rule-box: string -> image
           ;; Purpose: given a string, will overlay the text onto a image
           (rule-box (lambda (a-string fnt-size)
                       (cond
                         [(equal? (inner-list-2-img cur-rule "") a-string)
                          (overlay
                           (text a-string fnt-size "red")
                           (rectangle rectWidth BOTTOM "outline" "grey"))]
                         [else
                          (overlay
                           (text a-string fnt-size "Black")
                           (rectangle rectWidth BOTTOM "outline" "grey"))]))))
    (cond
      ;; We will only render 10 rules at a time. Make sure this happens!!!!
      [(>= (length lor) 10) (overlay
                             (rectangle (- (- WIDTH (/ WIDTH 11)) 200) BOTTOM "outline" "blue")
                             (list-2-img (list-tail (reverse lor) scroll-index) rectWidth 0))]
      [else (overlay
             (rectangle (- (- WIDTH (/ WIDTH 11)) 200) BOTTOM "outline" "blue")
             (list-2-img (reverse lor) rectWidth 0))])))


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

;; create-gui-alpha: list of alpha -> image
(define (create-gui-alpha loa)
  (overlay/align "left" "bottom"
                 (rectangle (/ WIDTH 11) (- HEIGHT BOTTOM) "outline" "blue")
                 (create-alpha-control loa)))

;; create-alpha-control: list of alpha -> image
(define (create-alpha-control loa)
  (overlay/align "right" "top"
                 (rectangle (/ WIDTH 11) (- (/ HEIGHT 2) 30) "outline" "blue")
                 (above
                  (control-header2 "Alpha List")
                  (draw-alpha loa 14) 
                  )))
                

;; draw-alpha: list-of-alpha string int -> image
;; Purpose: draws the alphabet image with every letter on another line
(define (draw-alpha loa fnt-size)
  (letrec (
           ;; t-box: string int -> image
           ;; Purpose: Creates a box for the sting to be placed in
           (t-box (lambda (a-string fnt-size)
                    (overlay
                     (text (symbol->string a-string) fnt-size "Black")
                     (rectangle (/ WIDTH 11) fnt-size "outline" "transparent")))))
    
    (cond
      [(empty? loa) (rectangle 10 10 "outline" "transparent")]
      [(<= (length loa) 1) (t-box (car loa) fnt-size)]
      [else (above
             (t-box (car loa) fnt-size)
             (draw-alpha (cdr loa) fnt-size))])))


  

;; state-left-control: null -> image
;; Purpose: Creates the state control panel
(define (state-left-control)
  (overlay/align "left" "top"
                 (control-header "State Options")
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")))

                 
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
   (text (string-upcase msg) 14 "Black")
   (rectangle 200 25 "outline" "transparent")))


;; control-header2: string -> image
;; Purpose: Creates a header label for right control panel
(define (control-header2 msg)
  (overlay
   (text (string-upcase msg) 14 "Black")
   (rectangle (/ WIDTH 11) 40 "outline" "transparent")))


(define (control-header3 msg)
  (overlay
   (text (string-upcase msg) 14 "Black")
   (rectangle (/ WIDTH 11) 25 "outline" "transparent")))


;; scale-text-to-image image image integer (between 0 and 1) -> image
;; Purpose: Scales the text of an image to not be larger then the image it is overlayed on
(define (scale-text-to-image text img sc)
  (let ((newScale (- sc .2)))
    (cond
      [(> (image-width text) (image-width img))
       (scale-text-to-image (scale newScale text) img 1)]
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
                               [else (cons (car lob) (active-button-list (cdr lob) x y))])))
       
       ;; checkButtonStates: list-of-states mouse-x mouse-y -> null or state
       ;; Purpose: Returns the state that was pressed on the GUI
       (checkButtonStates (lambda (los x y)
                            (cond
                              [(empty? los) null]
                              [(fsm-state-pressed? x y (car los)) (car los)]
                              [else (checkButtonStates (cdr los) x y)]))))
    (cond
      [(string=? me "button-down")
       (cond
         ;; See if there is an error to be displayed. If so disable all buttons and inputs
         [(not (null? (world-error-msg w)))
          (cond
            [(equal? (exit-pressed? x y (world-error-msg w) WIDTH HEIGHT) #t) (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w)
                                                                                     (world-cur-state w) (world-button-list w) (world-input-list w)
                                                                                     (world-processed-config-list w) (world-unporcessed-config-list w) null
                                                                                     (world-scroll-bar-index w))]
            [else (redraw-world w)])]

         ;; Check if a state was pressed
         [(not (null? (checkButtonStates (machine-state-list (world-fsm-machine w)) x y)))
          (begin
            (println
             (string-append
              "State: "
              (symbol->string (fsm-state-name (checkButtonStates (machine-state-list (world-fsm-machine w)) x y)))
              " was pressed"))
            (redraw-world w))]

         ;; Check if a button or input was pressed
         [else (begin
                 (define buttonPressed (check-button-list (world-button-list w) x y))
                 (cond
                   [(not (null? buttonPressed)) (run-function buttonPressed (create-new-world-button w (active-button-list (world-button-list w) x y)))]
                   [else (create-new-world-input w (check-and-set (world-input-list w) x y))]))])]
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
      [(and (or (or (key=? k "-") (key=? k " "))(string<=? "a" (string-downcase k) "z") (string<=? "1" (string-downcase k) "9")) (and (not (equal? k "shift")) (not (equal? k "rshift"))))
       (create-new-world-input w (check-and-add (world-input-list w) #t))]
      [(key=? k "\b") (create-new-world-input w (check-and-add (world-input-list w) #f))]
      [else w])))


;; ----- world redrawing functions below -----

;; create-new-world-input: world list-of-input-fields -> world
;; Purpose: Creates a new world to handle the list-of-input-fields changes
(define (create-new-world-input a-world loi)
  (world (world-fsm-machine a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) (world-button-list a-world)
         loi (world-processed-config-list a-world)(world-unporcessed-config-list a-world) (world-error-msg a-world) (world-scroll-bar-index a-world)))

;; create-new-world-button: world list-of-button-fields -> world
;; Purpose: Creates a new world to handle the list-of-button-fields changes
(define (create-new-world-button a-world lob)
  (world (world-fsm-machine a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) lob
         (world-input-list a-world) (world-processed-config-list a-world) (world-unporcessed-config-list a-world) (world-error-msg a-world) (world-scroll-bar-index a-world)))

;; redraw-world: world -> world
;; redraws the same world as before
(define (redraw-world a-world)
  (world (world-fsm-machine a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) (world-button-list a-world)
         (world-input-list a-world) (world-processed-config-list a-world)(world-unporcessed-config-list a-world) (world-error-msg a-world) (world-scroll-bar-index a-world)))

;; redraw-world-with-msg: world string string color -> world
;; Purpose: redraws the same world with a message
(define (redraw-world-with-msg a-world msg-body msg-header msg-color)
  (world (world-fsm-machine a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) (world-button-list a-world)
         (world-input-list a-world) (world-processed-config-list a-world)(world-unporcessed-config-list a-world)
         (msgWindow msg-body msg-header (posn (/ WIDTH 2) (/ HEIGHT 2)) msg-color) (world-scroll-bar-index a-world)))