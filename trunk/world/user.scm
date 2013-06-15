;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a user controlled world entity
;;
;;   Terminal_and_Windows
;;   Window_functions_and_initialization
;;   Avatar_color_chooser
;;   Button_commands
;;   Buttons
;;   Typing_and_talking
;;    Prototypes_and_fun_things
;;   Genesis
;;
(load "world/world.scm")
(load "ultima4.cells")
(load "scrabble.scm") ; TODO temporary
(load "world/graphics.scm")
(load "wwww/web.scm")

(define HUB-PORT 7155)
(define KITTEHBRAIN  #f)
(define ActivityTime (time))
(define SHOWBUTTONS #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal_and_Windows
;;

; Create an instance of the Terminal.  Force only one instance of the object by using the same name.
(define Terminal (Terminal))

; Default keyboard reader.  Returns button characters
; like #\a or #\!  s well as symbols like 'up or 'left
(define getKey (Terminal 'getKey))

; Keyboard handler registration
(define keyQueueStackRegister (Terminal 'keyQueueStackRegister))
(define keyQueueStackUnRegister (Terminal 'keyQueueStackUnRegister))

; Mouse handler registration
(define mouseQueueRegister (Terminal 'mouseQueueRegister)) ; Was mouseDispatcherRegister
(define mouseQueueUnRegister (Terminal 'mouseQueueUnRegister)) ; Was mouseDispatcherUnRegister 

; Chat window.
(define WinChat ((Terminal 'BufferNew)
  0 0
  (- (Terminal 'Theight) 1)  (Terminal 'Twidth)
  #x000f))
(define WinChatPutc (WinChat 'putc))
(define WinChatPuts (WinChat 'puts))
(define WinChatSetColor (WinChat 'set-color))
(define (WinChatDisplay . l)
  (for-each (lambda (o) (for-each WinChatPuts (display->strings o))) l))
(define (WinChatWrite o)
  (for-each (WinChat 'puts) (write->strings o)))
(WinChat '(set! ScrollbackHack #t))

; Console window
(define WinConsole ((Terminal 'BufferNew)
  (- (Terminal 'Theight) 27) 0
  26  (Terminal 'Twidth)
  #xe907))
(define WinConsolePuts (WinConsole 'puts))

(define (WinConsoleDisplay . l)
  (for-each
    (lambda (x)
     (if (not (and (pair? x) (eq? (car x) 'mapUpdateColumns))) ; Ignore mapUpdateColumns IPC message as it is very long.  That's what she said.
         (for-each WinConsolePuts (display->strings x))))
    l))

(define (WinConsoleWrite . l)
  (for-each
    (lambda (x)
     (if (not (and (pair? x) (eq? (car x) 'mapUpdateColumns))) ; Ignore mapUpdateColumns IPC message as it is very long.  That's what she said.
         (for-each WinConsolePuts (write->strings x))))
    l))


((WinConsole 'toggle))

; Input Window
(define WinInput ((Terminal 'WindowNew)
  (- (Terminal 'Theight) 1) 0
  1 (Terminal 'Twidth)
  #x040a))
(define WinInputPutc (WinInput 'putc))
(define WinInputPuts (WinInput 'puts))
(define WinInputSetColor (WinInput 'set-color))

; Help Window
(define WinHelpBorder ((Terminal 'WindowNew) 4 20 16 32 #x0200))
(define WinHelp ((Terminal 'WindowNew) 5 21 14 30 #x000a))
((WinHelpBorder 'toggle))
((WinHelp 'toggle))

; Color chooser palette window
(define WinPalette ((Terminal 'WindowNew) 2 15 9 36 #x000f))
(define (WinPaletteDisplay . e) (for-each (lambda (x) (for-each (WinPalette 'puts) (display->strings x))) e))
(define WinPaletteColor (WinPalette 'set-color))
(define WinPaletteGoto (WinPalette 'goto))
((WinPalette 'cursor-visible) #f)
((WinPalette 'toggle))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window_functions_and_initialization
;;

; Setup the help windows with an artistic border and prerender the help text
(map (lambda (x) ((WinHelp 'alpha) 0 x #f)) '(0 1 2 3 4 5 6 7 8 21 22 23 24 25 26 27 28 29))
(map (lambda (x) ((WinHelpBorder 'alpha) 0 x #f)) '(0 1 2 3 4 5 6 7 8 23 24 25 26 27 28 29 30 31))
((WinHelp 'puts) "          !! Help !!")
((WinHelp 'set-color) 0 15)
((WinHelp 'puts) "\r\n?  toggle help window")
((WinHelp 'puts) "\r\nt  talk mode (tab to exit)")
((WinHelp 'puts) "\r\nC  color of talking")
((WinHelp 'puts) "\r\nW  who is connected")
((WinHelp 'puts) "\r\nM  map toggle")
((WinHelp 'puts) "\r\nS  scrolling map mode")
((WinHelp 'puts) "\r\nA  animated cells toggle")
((WinHelp 'puts) "\r\n>  map size bigger")
((WinHelp 'puts) "\r\n<  map size smaller")
((WinHelp 'puts) "\r\nQ  quit World[tm]")
((WinHelp 'puts) "\r\nHJKL move map")
((WinHelp 'puts) "\r\n* To walk hit ESC then arrows")
((WinHelp 'puts) "\r\n  keys or 'nethack' keys")

; Initialize the color palette window

;(let generatePaletteWindowIndex ()
; (define y 0) (define x 0)
; (display "#((")       (loop 8  (lambda (i) (displayl (cons y (+ i x)) 0 ")(")))
; (set! y 1) (set! x 0) (loop 8  (lambda (i) (displayl (cons y (+ i x)) 0 ")(")))
; (set! y 2) (set! x 0) (loop 216(lambda (i) (displayl (cons (+ y (/ i 36)) (modulo  (+ i x) 36)) 0 ")(")))
; (set! y 0) (set! x 8) (loop 12 (lambda (i) (displayl (cons y (+ i x)) 0 ")(")))
; (set! y 1) (set! x 8) (loop 12 (lambda (i) (displayl (cons y (+ i x)) 0 ")(")))
; (display "))"))

(define WinPaletteDesc #(((0 . 0)0)((0 . 1)0)((0 . 2)0)((0 . 3)0)((0 . 4)0)((0 . 5)0)((0 . 6)0)((0 . 7)0)((1 . 0)0)((1 . 1)0)((1 . 2)0)((1 . 3)0)((1 . 4)0)((1 . 5)0)((1 . 6)0)((1 . 7)0)((2 . 0)0)((2 . 1)0)((2 . 2)0)((2 . 3)0)((2 . 4)0)((2 . 5)0)((2 . 6)0)((2 . 7)0)((2 . 8)0)((2 . 9)0)((2 . 10)0)((2 . 11)0)((2 . 12)0)((2 . 13)0)((2 . 14)0)((2 . 15)0)((2 . 16)0)((2 . 17)0)((2 . 18)0)((2 . 19)0)((2 . 20)0)((2 . 21)0)((2 . 22)0)((2 . 23)0)((2 . 24)0)((2 . 25)0)((2 . 26)0)((2 . 27)0)((2 . 28)0)((2 . 29)0)((2 . 30)0)((2 . 31)0)((2 . 32)0)((2 . 33)0)((2 . 34)0)((2 . 35)0)((3 . 0)0)((3 . 1)0)((3 . 2)0)((3 . 3)0)((3 . 4)0)((3 . 5)0)((3 . 6)0)((3 . 7)0)((3 . 8)0)((3 . 9)0)((3 . 10)0)((3 . 11)0)((3 . 12)0)((3 . 13)0)((3 . 14)0)((3 . 15)0)((3 . 16)0)((3 . 17)0)((3 . 18)0)((3 . 19)0)((3 . 20)0)((3 . 21)0)((3 . 22)0)((3 . 23)0)((3 . 24)0)((3 . 25)0)((3 . 26)0)((3 . 27)0)((3 . 28)0)((3 . 29)0)((3 . 30)0)((3 . 31)0)((3 . 32)0)((3 . 33)0)((3 . 34)0)((3 . 35)0)((4 . 0)0)((4 . 1)0)((4 . 2)0)((4 . 3)0)((4 . 4)0)((4 . 5)0)((4 . 6)0)((4 . 7)0)((4 . 8)0)((4 . 9)0)((4 . 10)0)((4 . 11)0)((4 . 12)0)((4 . 13)0)((4 . 14)0)((4 . 15)0)((4 . 16)0)((4 . 17)0)((4 . 18)0)((4 . 19)0)((4 . 20)0)((4 . 21)0)((4 . 22)0)((4 . 23)0)((4 . 24)0)((4 . 25)0)((4 . 26)0)((4 . 27)0)((4 . 28)0)((4 . 29)0)((4 . 30)0)((4 . 31)0)((4 . 32)0)((4 . 33)0)((4 . 34)0)((4 . 35)0)((5 . 0)0)((5 . 1)0)((5 . 2)0)((5 . 3)0)((5 . 4)0)((5 . 5)0)((5 . 6)0)((5 . 7)0)((5 . 8)0)((5 . 9)0)((5 . 10)0)((5 . 11)0)((5 . 12)0)((5 . 13)0)((5 . 14)0)((5 . 15)0)((5 . 16)0)((5 . 17)0)((5 . 18)0)((5 . 19)0)((5 . 20)0)((5 . 21)0)((5 . 22)0)((5 . 23)0)((5 . 24)0)((5 . 25)0)((5 . 26)0)((5 . 27)0)((5 . 28)0)((5 . 29)0)((5 . 30)0)((5 . 31)0)((5 . 32)0)((5 . 33)0)((5 . 34)0)((5 . 35)0)((6 . 0)0)((6 . 1)0)((6 . 2)0)((6 . 3)0)((6 . 4)0)((6 . 5)0)((6 . 6)0)((6 . 7)0)((6 . 8)0)((6 . 9)0)((6 . 10)0)((6 . 11)0)((6 . 12)0)((6 . 13)0)((6 . 14)0)((6 . 15)0)((6 . 16)0)((6 . 17)0)((6 . 18)0)((6 . 19)0)((6 . 20)0)((6 . 21)0)((6 . 22)0)((6 . 23)0)((6 . 24)0)((6 . 25)0)((6 . 26)0)((6 . 27)0)((6 . 28)0)((6 . 29)0)((6 . 30)0)((6 . 31)0)((6 . 32)0)((6 . 33)0)((6 . 34)0)((6 . 35)0)((7 . 0)0)((7 . 1)0)((7 . 2)0)((7 . 3)0)((7 . 4)0)((7 . 5)0)((7 . 6)0)((7 . 7)0)((7 . 8)0)((7 . 9)0)((7 . 10)0)((7 . 11)0)((7 . 12)0)((7 . 13)0)((7 . 14)0)((7 . 15)0)((7 . 16)0)((7 . 17)0)((7 . 18)0)((7 . 19)0)((7 . 20)0)((7 . 21)0)((7 . 22)0)((7 . 23)0)((7 . 24)0)((7 . 25)0)((7 . 26)0)((7 . 27)0)((7 . 28)0)((7 . 29)0)((7 . 30)0)((7 . 31)0)((7 . 32)0)((7 . 33)0)((7 . 34)0)((7 . 35)0)((0 . 8)0)((0 . 9)0)((0 . 10)0)((0 . 11)0)((0 . 12)0)((0 . 13)0)((0 . 14)0)((0 . 15)0)((0 . 16)0)((0 . 17)0)((0 . 18)0)((0 . 19)0)((1 . 8)0)((1 . 9)0)((1 . 10)0)((1 . 11)0)((1 . 12)0)((1 . 13)0)((1 . 14)0)((1 . 15)0)((1 . 16)0)((1 . 17)0)((1 . 18)0)((1 . 19)0)))

;(WinPaletteGoto 0 0) (WinPaletteColor 0 15) (WinPaletteDisplay #\X)
;(WinPaletteGoto 0 1) (loop 7  (lambda (i) (WinPaletteColor (+ i 1) 0) (WinPaletteDisplay #\ )))
;(loop 12 (lambda (i) (WinPaletteColor (+ 232 i) 0) (WinPaletteDisplay #\ )))
;(WinPaletteGoto 1 0) (loop 8  (lambda (i) (WinPaletteColor (+ 8 i) 0) (WinPaletteDisplay #\ )))
;(loop 12 (lambda (i) (WinPaletteColor (+ 232 12 i) 0) (WinPaletteDisplay #\ )))
;(WinPaletteDisplay "\r\n")
;(loop 216 (lambda (i)
;  (WinPaletteColor (+ 16 i) 0)
;  (WinPaletteDisplay #\ )))

; Render all 256 colors in the color palette window
(loop 256 (lambda (i)
  (let ((d (vector-ref WinPaletteDesc i)))
    (WinPaletteGoto (caar d) (cdar d))
    (WinPaletteColor i i)
    (WinPaletteDisplay #\ ))))


; Screen redraw and signal handler
(define handlerCount 0)
(define (handleTerminalResize . forcedSize)
  ; TODO Temporary assertion
  (if (!= handlerCount 0) (WinChatDisplay "\nWARNING: handleTerminalResize is not reentrant"))
  (set! handlerCount 1)
  (letrec ((newTermSize (if (null? forcedSize) (terminal-size) (car forcedSize)))
           (tw #f)
           (th #f))
    ((Terminal 'ResetTerminal) newTermSize)
    (set! tw (Terminal 'Twidth))
    (set! th (Terminal 'Theight))
    (WinConsoleDisplay "\r\nSIGWINCH: newTermSize " (cons tw th))
    ((WinChat 'resize)          (- th 1) tw)
    ((avatarViewport 'move)                0 (- tw (avatarViewport 'Wwidth) 2))
    ((WinInput 'resize)                1 tw)
    ((WinInput 'move)           (- th 1) 0))
  (set! handlerCount 0))

(define sigwinch
 (let ((count 0)
       (sem (open-semaphore 1))) (lambda ()
   ; Either inc count because already redrawing or start
   ; redraw loop while more potential requests occur
   (semaphore-down sem)
   (if (!= 0 count)
     ; Count is not 0
     (begin
       (set! count (+ count 1))
       (semaphore-up sem)) ; Done
     ; Count is 0 (first time)
     (let ~ ()
       (set! count 1)
       (semaphore-up sem)
       (handleTerminalResize) ; Count may increase during this call
       (semaphore-down sem)
       ; Either no more redraw requests so done or redraw again while resetting count to 1
       (if (= 1 count)
         (begin
           (set! count 0)
           (semaphore-up sem)) ; Done
         (begin
           (WinConsoleDisplay "  count " count)
           (~))))))))

; Welcome message marquee displayed when connecting.
(define (welcome)
 (define WinMarquee
  ((Terminal 'WindowNew)
    (/ (Terminal 'Theight) 3)  (- (/ (Terminal 'Twidth) 2) 12)
    3  24
    #x0f))
 (define WinMarqueePuts (WinMarquee 'puts))
 (define WinMarqueePutc (WinMarquee 'putc))
 (define WinMarqueeSetColor (WinMarquee 'set-color))
 ((WinMarquee 'cursor-visible) #f) ; Disable cursor in map window.
 (WinMarqueePuts " +====================+ ")
 (WinMarqueePuts " |                    | ")
 (WinMarqueePuts " +====================+ ")
 (let ~~ ((i -5))
   (sleep 200) ; Delay
   ((WinMarquee 'goto) 0 0)
   (WinMarqueeSetColor 0 15)
   (if (= 0 (modulo i 4)) (begin
      (WinMarqueePuts "  +====================+")
      (WinMarqueePuts " /                    / ")
      (WinMarqueePuts "+====================+  "))
   (if (or (= 3 (modulo i 4)) (= 1 (modulo i 4))) (begin
      (WinMarqueePuts " +====================+ ")
      (WinMarqueePuts " |                    | ")
      (WinMarqueePuts " +====================+ "))
   (begin
      (WinMarqueePuts "+====================+  ")
      (WinMarqueePuts " \\                    \\ ")
      (WinMarqueePuts "  +====================+"))))
   ((WinMarquee 'goto) 1 2)
   (let ~ ((j 0))
     (WinMarqueeSetColor 0 (vector-ref #(07 07 07 07 07 07 07 07   07 07 07
                                       9 11 10 12 5  1  2  6  4 7)
                                     (modulo (+ i j) 21)))
     (WinMarqueePutc (vector-ref #(#\W #\e #\l #\c #\o #\m #\e #\   #\t #\o #\ 
                                   #\W #\o #\r #\l #\d #\[ #\t #\m #\] #\ )
                                 (modulo (+ i j) 21)))
     (if (< j 19) (~ (+ j 1)))) ; Marquee area width
   (if (< i 60) ; Msg scroll count
       (~~ (+ i 1))))
 ((WinMarquee 'delete)))

(define (makeProgressBar y x title)
 (let ((win ((Terminal 'WindowNew) y x 3 22 #x4a))
       (pos 0))
  ((win 'cursor-visible) #f)
  ((win 'puts) "+--------------------+")
  ((win 'puts) "|                    |")
  ((win 'puts) "+--------------------+")
  ((win 'goto) 0 2) ((win 'puts) title)
  ((win 'goto) 1 1) ((win 'set-color) 7 0)
  (lambda ()
   (if (= pos 20)
    (begin
     ((win 'delete))
     #f)
    (begin
     ((win 'putc) #\ )
     (set! pos (+ pos 1))
     #t)))))

(define (boxInput title)
 (define box
  ((Terminal 'WindowNew)
    (/ (Terminal 'Theight) 2)  (- (/ (Terminal 'Twidth) 2) 10)
    3  20
    #x0f))
 (define puts (box 'puts))
 (define putc (box 'putc))
 (define setcolor (box 'set-color))
 (define str "")
 (define myGetKey ((Terminal 'getKeyCreate)))
 (puts "+------------------+")
 (puts "|                  |")
 (puts "+------------------+")
 ((box 'goto) 0 2) (puts title)
 ((box 'goto) 1 1)
 (let ~ ((c (myGetKey)))
   (or (eq? c RETURN) (eq? c NEWLINE)
    (begin
     (if (or (eq? c 'left)
             (eq? c CHAR-CTRL-H) ; Rubout characters
             (eq? c CHAR-CTRL-_)
             (eq? c CHAR-CTRL-?))
       (if (eq? str "") "" (begin
             ((box 'backspace) #\ )
             (set! str (substring str 0 (- (string-length str) 1)))))
       (if (and (< (string-length str) 18) ; Name length limit
                (<= #\! c) (<= c #\~))     ; Name character restriction
            (begin
              (putc c)
              (set! str (string str c)))))
     (~ (myGetKey)))))
 (myGetKey 'destroy)
 ((box 'delete))
 str)



(define (boxBool title)
 (define eventQueue (QueueCreate))
 (define box
  ((Terminal 'WindowNew)
    (- (/ (Terminal 'Theight) 2) 2)  (- (/ (Terminal 'Twidth) 2) 10)
    3  20
    #x010b))
 (define puts (box 'puts))
 (define setcolor (box 'set-color))
 (define ret #f)
 (mouseQueueRegister box eventQueue)
 (keyQueueStackRegister eventQueue)

 (puts "+------------------+")
 (puts "|                  |")
 (puts "+------------------+")
 ((box 'goto) 0 2) (puts title)
 (setcolor #x01 #x0f)
 ((box 'goto) 1 5) (puts "yes    no")

 (set! ret
   (let ~ ((e (QueueGet eventQueue)) ; mouse event
         (s ())) ; click state
     (if (pair? e)
       (if (and (eq? (car e) 'mouse0)
                (= (cadr e) 1) (pair? (memv (caddr e) '(5 6 7)))) ; yes area
           (begin (setcolor #x03 #x0f) ((box 'goto) 1 5) (puts "YES")
                  (setcolor #x01 #x0f) ((box 'goto) 1 12) (puts "no")
                  (~ (QueueGet eventQueue) 'yes))
       (if (and (eq? s 'yes) ; mouse is in down yes state
                (eq? (car e) 'mouseup)
                (and (= (cadr e) 1) (pair? (memv (caddr e) '(5 6 7))))) ; yes area
           #t ; return YES
       (if (and (eq? (car e) 'mouse0)
                (= (cadr e) 1) (pair? (memv (caddr e) '(12 13)))) ; no area
           (begin (setcolor #x01 #x0f) ((box 'goto) 1 5) (puts "yes")
                  (setcolor #x03 #x0f) ((box 'goto) 1 12) (puts "NO")
                  (~ (QueueGet eventQueue) 'no))
       (if (and (eq? s 'no) ; mouse is in down no state
                (eq? (car e) 'mouseup)
                (and (= (cadr e) 1) (pair? (memv (caddr e) '(12 13))))) ; no area
           #f ; return YES
       (begin (setcolor #x01 #x0f) ((box 'goto) 1 5) (puts "yes    no")
              (~ (QueueGet eventQueue) ()))))))
    (pair? (memq e '(#\Y #\y)))))) ; Pressed a key

 (keyQueueStackUnRegister eventQueue)
 (mouseQueueUnRegister box)
 ((box 'delete))
 (QueueDestroy eventQueue)
 ret)


(define (scrollFocusedWindow d)
  (let ((win (if (WinConsole 'ENABLED) WinConsole WinChat)))
    (if (eq? d 'home) ((win 'scrollHome))
    (if (eq? d 'end)  ((win 'scrollEnd))
    (if (eq? d 'up)   ((win 'scrollBack))
    (if (eq? d 'down) ((win 'scrollForward))))))))


;;;;;;;;;;;;;;;;;;;;;;;;
; Avatar_color_chooser
(define eventQueue (QueueCreate))
(define LastColors (make-list 32 0))
(define CursorYX (cons 0 0))

(mouseQueueRegister WinPalette eventQueue) ; This window's mouse even queue is alwasy registered

; #( ((y x) dimColorIdx) ...)
; Called by the keyboard and mouse handler to
; update the cursor in the palette window.
(define (updatePaletteCursor y x)
  (let ((oy (car CursorYX))
        (ox (cdr CursorYX)))
    (WinPaletteColor (/ ((WinPalette 'getColor) oy ox) 256) 0)
    (WinPaletteGoto oy ox)
    (WinPaletteDisplay #\ )
    (WinPaletteColor (/ ((WinPalette 'getColor) y x) 256) ; Background color 
                     (if (null? (memv y '(0 2))) 0 15)) ; Cursor color
    (WinPaletteGoto y x)
    (WinPaletteDisplay #\X))
    (set! CursorYX (cons y x)))

(define (keyColorsAction c)
 ; Return done.  Everything else returns true signalling we want to keep reading the keyboard
 (if (pair? (memv c (list CHAR-ESC TAB #\C #\c #\q #\Q)))
   #f
 ; Fake a mouse event as the mouse handler contains the bulk of the avatar color change code
 (if (pair? (memv c (list RETURN NEWLINE SPACE)))
   (begin
     (mouseColorsActionHandler 'mouse0 (car CursorYX) (cdr CursorYX))
     #t)
 (let ((y (car CursorYX))
       (x (cdr CursorYX)))
   (cond ((eq? c #\j) (set! y (modulo (+ y 1)  (- (WinPalette 'Wheight) 1))))
         ((eq? c #\k) (set! y (modulo (- y 1)  (- (WinPalette 'Wheight) 1))))
         ((eq? c #\h) (set! x (modulo (- x 1)  (WinPalette 'Wwidth))))
         ((eq? c #\l) (set! x (modulo (+ x 1)  (WinPalette 'Wwidth))))
         ((eq? c #\K) ((WinPalette 'move) (+ -1 (WinPalette 'Y0))      (WinPalette 'X0)))
         ((eq? c #\J) ((WinPalette 'move) (+  1 (WinPalette 'Y0))      (WinPalette 'X0)))
         ((eq? c #\H) ((WinPalette 'move)       (WinPalette 'Y0) (+ -1 (WinPalette 'X0))))
         ((eq? c #\L) ((WinPalette 'move)       (WinPalette 'Y0) (+  1 (WinPalette 'X0)))))
   (updatePaletteCursor y x)
   #t))))

; A mouse event handler from the color palette window
(define (mouseColorsActionHandler action wy wx)
 (if (eq? action 'mouse0)
 (letrec ((clr (/ ((WinPalette 'getColor) wy wx) 256)))
   ; Update and render the last selected color bars
   (if (not (and (< 19 wx) (< wy 2))) (begin
     (set! LastColors (cdr (append LastColors (list clr))))
     (WinPaletteGoto 0 20)
     (let ~ ((i 0) (lst LastColors)) (if (pair? lst) (begin
       (if (= i 16) (WinPaletteGoto 1 20))
       (WinPaletteColor (car lst) 0)
       (WinPaletteDisplay #\ )
       (~ (+ i 1) (cdr lst)))))))
   (updatePaletteCursor wy wx)
   (WinPaletteGoto 8 0) (WinPaletteColor 0 7)(WinPaletteDisplay "                                    ")
   (WinPaletteGoto 8 1) (WinPaletteDisplay clr)
   (WinPaletteGoto 8 5) (WinPaletteDisplay "#x" (number->string clr 16))
   (WinPaletteGoto 8 10) (WinPaletteColor clr 0) (WinPaletteDisplay "    ")
   (WinPaletteGoto 8 15) (WinPaletteColor 0 clr) (WinPaletteDisplay "**XX")
   (WinPaletteGoto 8 20) (WinPaletteColor clr 0) (WinPaletteDisplay "**XX")
   (WinPaletteGoto 8 25) (WinPaletteColor clr #xf) (WinPaletteDisplay "**XX")
   (WinPaletteGoto 8 30) (WinPaletteColor #xf clr) (WinPaletteDisplay "**XX")
   (let ((glyph (avatar 'glyph)))
     (IpcWrite (list 'entity (avatar 'dna)
                      (Glyph
                        (glyph0bg glyph) clr (glyph0ch glyph)
                        (glyph1bg glyph) clr (glyph1ch glyph))))))))

(define (avatarColor)
  ((WinPalette 'toggle)) ; Enable the Palette window
  ; Push palette window's event queue to button handler.
  ; The mouse handler is always registered.
  (keyQueueStackRegister eventQueue)
  (let ~ ()
    (let ((e (QueueGet eventQueue)))
      (if (pair? e)
        (begin ; Mouse action
           (apply mouseColorsActionHandler e)
           (~))
        (if (keyColorsAction e) ; Button action
            (~)))))
  (keyQueueStackUnRegister eventQueue)
  ((WinPalette 'toggle)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Button_commands
;;
; Toggle the help window
(define (help)
 ((WinHelp 'toggle))
 ((WinHelpBorder 'toggle)))

; Able to move the following window objects: avatarViewport WinChat WinConsole
; With the H J K L keys toggled with CTRL-W
(define moveableWin #f)

(define (winMapMoveSelect)
 (cond ((eq? moveableWin avatarViewport)
         (WinChatDisplay "\nWinChat")
         (set! moveableWin WinChat))
       ((eq? moveableWin WinChat)
         (WinChatDisplay "\nWinConsole")
         (set! moveableWin WinConsole))
       (else
         (WinChatDisplay "\navatarViewport")
         (set! moveableWin avatarViewport))))

(define (winMapUp)   ((moveableWin 'move) (+ -1 (moveableWin 'Y0))      (moveableWin 'X0)))
(define (winMapDown) ((moveableWin 'move) (+  1 (moveableWin 'Y0))      (moveableWin 'X0)))
(define (winMapLeft) ((moveableWin 'move)       (moveableWin 'Y0) (+ -1 (moveableWin 'X0))))
(define (winMapRight)((moveableWin 'move)       (moveableWin 'Y0) (+  1 (moveableWin 'X0))))
(define (winMapShorten)((moveableWin 'resize) (+ (moveableWin 'Wheight) -1) (moveableWin 'Wwidth)))
(define (winMapTallen) ((moveableWin 'resize) (+ (moveableWin 'Wheight)  1) (moveableWin 'Wwidth))) ; I invted the word 'tallen' the antonym of 'shorten'
(define (winMapThin)   ((moveableWin 'resize) (moveableWin 'Wheight) (+ (moveableWin 'Wwidth) -1)))
(define (winMapWiden)  ((moveableWin 'resize) (moveableWin 'Wheight) (+ (moveableWin 'Wwidth)  1)))

(define (walk d) 
 ((avatar 'walk) d))

; Notify IPC of my name and glyph change
(define (changeName str)
  (IpcWrite
   (list 'entity (avatar 'dna) str
            (Glyph (glyph0bg (avatar 'glyph))
                   (glyph0fg (avatar 'glyph))
                   (string-ref str 0)
                   (glyph1bg (avatar 'glyph))
                   (glyph1fg (avatar 'glyph))
                   (string-ref str (if (< 1 (string-length str)) 1 0)))))) ; Notify IPC of my name change

;(define (rollcall)
; (IpcWrite ; Force all to evaluate the following
;  `(if (!= DNA ,DNA) ; Skip if I sent this message
;   (IpcWrite ; Force all (except me) to evaluate the following
;    `(if (= DNA ,,DNA) ; If me, evaluate this expression from the other peer
;     (voice 0 10
;      (string ,(avatar 'name) " "
;              ,(let ((t (- (time) ActivityTime)))
;                (if (< t 60)   (string (number->string t) "s")
;                (if (< t 3600) (string (number->string (/ t 60)) "m")
;                (if (< t 86400)(string (number->string (/ t 3600)) "h")
;                (string (number->string (/ t 86400)) "d"))))))))))))

(define (buttonSetCell cell)
 (if PortMapAgent
   ; Send to map agent. If map agent doesn't respond then
   ; ignore it just send to everyone.  Map agent eventually
   ; send a mapSetCell message to all other avatars.
   (or ((ipc 'private) PortMapAgent `(setCellAgent ,(avatar 'z) ,(avatar 'y) ,(avatar 'x) ,cell))
     (begin
       (set! PortMapAgent #f) ; No response so unset the port and recurse
       (buttonSetCell)))
   ; Send to everyone
   (IpcWrite `(mapSetCell ,(avatar 'z) ,(avatar 'y) ,(avatar 'x) ,cell))))


(define (mouseWalkActionHandlerLoop)
  (define queue (QueueCreate))
  (mouseQueueRegister avatarViewport queue)
  (thread (let ~ ((e (QueueGet queue)))
    (let ((action (car e))
          (wy (cadr e))
          (wx (caddr e)))
      (if (eq? action 'mouse0)
        (letrec ((mapy (+ (avatarViewport 'my) wy))
                 (mapx (+ (avatarViewport 'mx) (/ wx 2))))
          (let ~ ((l (lineWalks (avatar 'y) (avatar 'x) mapy mapx)))
            (if (pair? l) (begin
              (walk (car l))
              (~ (cdr l)))))))
      (~ (QueueGet queue))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buttons
;;
;; Buttons keep track of expresions to keyboard buttons in either a vector of
;; character indexes and a symbol association list.
;;
(define Buttons (make-vector 257 ()))
(define ButtonsSymbols ())

(define (setButton ch exp)
 (if (char? ch)
   (vector-set! Buttons ch exp) ; For now characters are also integer constants.
   (let ((bs (assq ch ButtonsSymbols)))
     (if (pair? bs)
       (set-cdr! bs exp)
       (set! ButtonsSymbols (cons (cons ch exp) ButtonsSymbols))))))

; Consider the expression associated with this button value.  If data just return the expression.  Otherwise
; assume a closure.  Consider the closure -> closure's code -> the code's pre-compiled
; expression and return it (hack).
(define (getButton ch)
  (if (char? ch)
    (vector-ref Buttons ch)
    (let ((a (assq ch ButtonsSymbols)))
      (if (pair? a) (cdr a) ()))))

(setButton 'down '(walk 6))
(setButton #\j '(walk 6))
(setButton 'up '(walk 2))
(setButton #\k '(walk 2))
(setButton 'left '(walk 4))
(setButton #\h '(walk 4))
(setButton 'right '(walk 0))
(setButton #\l '(walk 0))
(setButton #\b '(walk 5))
(setButton #\n '(walk 7))
(setButton #\y '(walk 3))
(setButton #\u '(walk 1))
(setButton #\- '(walk 8))
(setButton #\+ '(walk 9))
(setButton #\A '(begin
  (set! VIEWPORTANIMATION (not VIEWPORTANIMATION))
  (WinChatDisplay "\nMap animation " VIEWPORTANIMATION)))
(setButton #\C '(avatarColor))
;(setButton #\W '(rollcall))
(setButton CHAR-CTRL-W '(winMapMoveSelect))
(setButton #\H '(winMapLeft))
(setButton #\J '(winMapDown))
(setButton #\K '(winMapUp))
(setButton #\L '(winMapRight))
(setButton #\{ '(winMapShorten))
(setButton #\} '(winMapTallen))
(setButton #\[ '(winMapThin))
(setButton #\] '(winMapWiden))

(setButton #\S '(begin
  (set! MAPSCROLL
    (if (eq? MAPSCROLL 'always) 'edge
    (if (eq? MAPSCROLL 'edge) 'never
    'always)))
  (WinChatDisplay "\nScroll mode set to:" MAPSCROLL)))
(setButton #\M '((avatarMap 'toggleWindow)))
(setButton #\s '(focusTalk 'scream))
(setButton #\t '(focusTalk 'talk))
(setButton #\w '(focusTalk 'whisper))

(setButton CHAR-CTRL-L '(begin ((avatarMap 'canvasResetArray) (avatarMap 'ceiling))
                               ((avatarMap 'viewportRecenterReset) (avatar 'y) (avatar 'x))
                               ((WinChat 'repaint))))
(setButton #\d '(buttonSetCell (avatar 'cell)))
(setButton #\g
   '(let ((o (apply (avatarMap 'baseCell) ((avatar 'gps)))))
     (WinChatDisplay "\nGrabbed " o)
     (buttonSetCell cellAIR)
     (avatar `(set! cell ,o))))
(setButton #\? '(help))
(setButton #\< '((avatarMap 'smaller)))
(setButton #\> '((avatarMap 'bigger)))
(setButton CHAR-CTRL-@ '(shutdown))
(setButton CHAR-CTRL-Q '(shutdown))
(setButton #\Q         '(shutdown))
(setButton #eof        '(begin (sleep 2000) (shutdown "EOF"))) ; Give any interrupt time to exit rather than an #eof from the keyboard
(setButton 'pgup '(scrollFocusedWindow 'up))
(setButton 'pgdown '(scrollFocusedWindow 'down))
(setButton 'home '(scrollFocusedWindow 'home))
(setButton 'end '(scrollFocusedWindow 'end))
;(setButton #\  '(begin (avatar '(stop)) (kat '(stop))))
(if QUIETLOGIN (begin
   (setButton CHAR-CTRL-C '((WinConsole 'toggle)))
   (setButton CHAR-CTRL-E '(begin
      (set! VIEWPORTANIMATION (not VIEWPORTANIMATION))
      (ipc '(set! Debug (not Debug)))
      ;(set! SHOWBUTTONS (not SHOWBUTTONS))
      (set! EDIT (not EDIT))
      ((avatarMap 'debugDumpMapInfoToggle))
      ((avatarMap 'circularizeToggle))
      ((avatarMap 'bigger))
      (WinChatDisplay "\nEDIT " EDIT)))
   (setButton #\1 '(WinChatDisplay "\n" (cellSymbol (cellRef ((avatar 'lookHere))))
                                      " " (cellSymbol (cellRef ((avatar 'lookAt))))))
   (setButton #\2 '(loop2 (avatar 'y) (+ (avatar 'y) 10)
                          (avatar 'x) (+ (avatar 'x) 10)
                          (lambda (y x)
                            (if (= x (avatar 'x)) (WinChatDisplay "\n"))
                            (WinChatDisplay " " (((avatarMap 'myCanvas) 'height) y x) ))))
   ;(setButton #\2 '(handleTerminalResize (cons 600 400)))
   (setButton #\4 '(ghosts))
   (setButton #\5 '(pong))
   (setButton #\6 '(WinChatDisplay "\n" ((avatar 'lookAt)) ((avatarMap 'column) (avatar 'y) (+(avatar 'x)1))))
   (setButton #\7 '(NewKat))
   (setButton #\8 '((avatarMap 'incLightSource) (avatar 'y) (avatar 'x)))
   (setButton CHAR-CTRL-F '(writeIco))
   (setButton #\z '(WinConsoleDisplay (list (WinChat 'TY) (WinChat 'TX) (WinChat 'needToScroll))))
))

; Perform button's action
(define (button . buttonList)
 (each-for buttonList (lambda (b)
  (let ((expr (getButton b)))
    (if SHOWBUTTONS (WinConsoleDisplay "BUTTON(" b " " expr ")"))
    ; Evaluate the button's command value
    (cond ((procedure? expr)  (expr))
          ((not (null? expr)) (eval expr))
          (else (if SHOWBUTTONS (WinConsoleDisplay "\r\nButton " b " undefined"))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typing_and_talking
;;
(define (say phrase . level)
  (apply (avatar 'speak) phrase level))

(define (saySystem . strs)
 (IpcWrite (list 'voice 0 0 (apply string strs))))

; Usage:: (replTalk 'getBuffer)
;         (replTalk '{talk|whisper|scream} {character})
;  Returns: 'more
;           'sent
;           'done
;           
; TODO full cooked keyboard with editing
(define replTalk
 (let ((talkInput ""))
  (lambda (cmd . c) ; cmd can be one of 'talk 'whisper 'scream or 'getBuffer
   (if (eq? cmd 'getBuffer) talkInput ; Return input buffer contents.
   (begin
     (set! c (car c)) ; Rest args chr better be a list with one character
     ; Handle backspace
     (if (or (eq? c CHAR-CTRL-H)
             (eq? c CHAR-CTRL-_)
             (eq? c CHAR-CTRL-?))
       (begin
         (if (not (eq? "" talkInput))
           (begin (if (eq? cmd 'whisper)
                    (begin (WinInputPutc #\ )
                           ((WinInput 'back))
                           ((WinInput 'backspace) #\)))
                    ((WinInput 'backspace) #\ ))
                  (set! talkInput (substring talkInput 0 (- (string-length talkInput) 1)))))
         'more)
     ; Send accumulated buffer as a talk message
     (if (or (eq? c RETURN)
             (eq? c NEWLINE))
       (begin
         ; Toggle help window if certain phrase entered
         ; Send talk chatter to IPC or evaluate expression
         (if (and (not (eq? "" talkInput))
                  (eq? #\: (string-ref talkInput 0)))
             ; Evaluate an expression Parse
             (begin (WinChatDisplay "\n")
                    (WinChatDisplay talkInput)
                    (WinChatDisplay "=>")
                    (WinChatWrite
                      (call/cc (lambda (c) ; Return here if an error occurs
                         (vector-set! ERRORS (tid) c)
                         (avatar (read-string (cdr-string talkInput))))))) ; Eval expression in Avatar's environment
             (if (eq? cmd 'whisper) ((avatar 'speak) (string "(" (string-downcase talkInput) ")"))
               (if (eq? cmd 'scream)  ((avatar 'speak) (string-upcase talkInput))
                 ((avatar 'speak) talkInput))))
         ; Perform actions based on talk phrases.
         (tankTheOperator talkInput)
         (set! talkInput "")
         (WinInputPuts "\r\n")
         'sent)
     ; Quit chat mode.
     (if (or (eq? c CHAR-ESC) ; Escape char
             (eq? c CHAR-CTRL-I)) ; Tab char
       (begin (WinInputPuts "\r\n")
              'done)
     ; Append new character to talk string buffer
     (if (and (>= c #\ )(<= c #\~))
       (begin (WinInputPutc c)
              (if (eq? cmd 'whisper) (begin (WinInputPutc #\))
                                            ((WinInput 'back))))
              (set! talkInput (string talkInput c))
              'more)
     ; else
     'more))))))))) ; replTalk

; Activate event driven talk mechanism
;   type is one of 'whisper 'talk 'scream
(define (focusTalk type)
 (define getc ((Terminal 'getKeyCreate)))
 ((avatar 'setSpeakLevel) (cond ((eq? type 'whisper) 2) ((eq? type 'scream) 500) (else 20)))
 (let ~ ()
   ; Draw current string in input buffer
   (WinInputPuts 
     (if (eq? type 'scream) (string "}}" (replTalk 'getBuffer))
     (if (eq? type 'whisper)(string "(" (replTalk 'getBuffer) ")")
                            (string ">" (replTalk 'getBuffer)))))
   ; If in whisper mode, move cursor back one past the trailing ')'
   (if (eq? type 'whisper) ((WinInput 'back)))
   (let ~~ ()
     (let ((ret (replTalk type (getc))))
       (if (eq? ret 'more) (~~)
       (if (eq? ret 'sent) (~))))))
 (getc 'destroy))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prototypes_and_fun_things
;;

; \2|1/  Return list of avatar movements
;_3\|/0_ required to walk from one (y x)
; 4/|\7  map location to another.
; /5|6\
(define lineWalks
  ; Given a vector on the cartesian plane in quadrant 1 between slope
  ; 0 and 1, return list of Bresenham X or Y,X increments which walk
  ; the line along X.  y must be <= x.
  (let ((lineIncrements (lambda (y x stepDir incDir)
   (letrec ((yy (+ y y))  (yy-xx (- yy (+ x x))))
     (let ~ ((i x)  (e (- yy x)))
       (if (= i 0) ()
       (if (< 0 e) (cons incDir  (~ (- i 1) (+ e yy-xx)))
                   (cons stepDir (~ (- i 1) (+ e yy))))))))))
 (lambda (y0 x0 y1 x1)
   (letrec ((y (- y1 y0))
            (x (- x1 x0))
            (ay (abs y))
            (ax (abs x)))
    (if (< ay ax)
      (if (< 0 x) ; Walk X and increment Y
        (if (< 0 y)
          (lineIncrements ay ax 0 7)      ; 7
          (lineIncrements ay ax 0 1))     ; 0
        (if (< 0 y)
          (lineIncrements ay ax 4 5)      ; 4
          (lineIncrements ay ax 4 3)))    ; 3
      (if (< 0 y) ; Walk Y and increment X
        (if (< 0 x)
          (lineIncrements ax ay 6 7)      ; 6
          (lineIncrements ax ay 6 5))     ; 5
        (if (< 0 x)
          (lineIncrements ax ay 2 1)        ; 1
          (lineIncrements ax ay 2 3)))))))) ; 2

; The first Avatar macro
; Usage:: (avatar '(march)) Start/stop the thread
(define march (macro ()
 (if Stop
   (begin
    (WinChatSetColor 0 10) (tankTalk "\r\n*The journey begins*")
    (set! Stop #f)
    (thread (let ~ ()
      (for-each
        (lambda (x) (and Stop (unthread)) (walk x) (sleep 2000))
        '(0 0 0 0 2 2 2 2 4 4 4 4 6 6 6 6))
      (sleep 500)
      (~))))
   (begin
     (WinChatSetColor 0 10) (WinChatDisplay "\n*Thus ends the journey*")
     (set! Stop #t)))))

; Originally a "walking kitty soldier"
; Usage:: (avatar '(walkAround 100)) Start/stop the thread
(define walkAround (macro cycles
 (set! cycles (if (null? cycles) 32 (car cycles))) ; Set max cycles
 (if Stop
   (letrec ((happyVector (vector 0 0 0 0 0 0 0 0))
          (dist 0)
          (dir 0)) ; Initially start walking right
     (WinChatSetColor 0 10) (tankTalk "\r\n*The aimlessness begins*")
     (set! Stop #f)
     (thread (let ~ () ; Main loop
       ; Walk kitty quasi-randomly.  Set the direction the
       ; avatar will walk based on weighted list of directions
       (set! dir
         (letrec ((dir1 (modulo (+ dir (random 3) -1) 8))
                  (dir2 (modulo (+ dir (random 3) -1) 8)))
              (if (> (vector-ref happyVector dir1)
                     (vector-ref happyVector dir2))
                  dir1 dir2)))
       (walk dir)
       ; Update neuron vector based on new distance from parent
       (let ((newDist (distance (gps) ((avatar 'gps)))))
         (vector-set! happyVector dir
             (+ (if (< newDist dist) 1
                  (if (= newDist dist) -1 -2))
                (vector-ref happyVector dir)))
         (set! dist newDist))
       (sleep 300) ; pause
       ; If avatar and kitty meet do something
       ;(if (equal? (gps) ((avatar 'gps))) (say "Hiss!"))
       ; Neuron depletion.  After a few iterations, halve each weight.
       (if (= (modulo cycles 10) 0)
         (vector-map! (lambda (x) (/ x 2)) happyVector))
       ; Kill entity or loop again
       (set! cycles (- cycles 1))
       (if (and (not Stop) (< 0 cycles)) (~)))))
   (begin
     (WinChatSetColor 0 10) (WinChatDisplay "\n*Thus ends the aimlessness*")
     (set! Stop #t))))) ; walkAround


(define pongPower #f)

(define pongActionMacro (macro ()
  (let ((oy (* (/ (avatar 'y) MapBlockSize) MapBlockSize)) ; Origin of this map block
        (ox (* (/ (avatar 'x) MapBlockSize) MapBlockSize))
        (m 0) ; Map location ball is walking to
        (n 0))
   (set! pongPower #t)
   (WinChatDisplay "\nPong starts " dna " " name)
   (let ~ ((wall 0)) (if pongPower (begin
     (if (= wall 0) (begin (set! m (random MapBlockSize)) (set! n (- MapBlockSize 1)))
      (if (= wall 1) (begin (set! m 0)                     (set! n (random MapBlockSize)))
       (if (= wall 2) (begin (set! m (random MapBlockSize)) (set! n 0))
        (if (= wall 3) (begin (set! m (- MapBlockSize 1))    (set! n (random MapBlockSize)))))))
     (let ~ ((l (lineWalks y x (+ oy m) (+ ox n))))
       (if (pair? l) (begin
         (face (car l))
         ; Is there something there?
         (if (= cellAIR (apply (myMap 'firstCell) (gpsFace)))
           (begin
             (mapWalkDetails self)
             (sleep 100)
             (if pongPower (~ (cdr l))))
           (set! wall (+ 1 wall))))))
     (~ (modulo (+ wall 1) 4)))))
   (WinChatDisplay "\nPong ends " dna " " name)
   (die)))) ; pong

(define (pong)
 (if pongPower
   (set! pongPower #f)
   (thread
     ((Avatar "()PongBall" (avatar 'z) (avatar 'y) (+ (avatar 'x) 1) ipc #t)
      '(pongActionMacro)))))


; Tank agent - The first interactive user agent.
(define (tankTalk . l)
  (thread
    (sleep 500)
    (WinChatSetColor 0 15) (WinChatDisplay "\nTank ")
    (WinChatSetColor 0 7) (apply WinChatDisplay l)))

(define tankHangupTime #f)

(define (tankStartListening)
 (or tankHangupTime
   (begin
     (tankTalk "Operator...")
     (thread (let ~ ()  ; tankHangupTime will be set right after this thread is started
       (sleep 12000)
       (if (< (time) tankHangupTime)
         (~)
         (begin
           (tankTalk "*CLICK*")
           (set! tankHangupTime #f)))))))
  (set! tankHangupTime (+ 60 (time))))

(define (tankTheOperator talkInput)
 (if (string=? "?" talkInput) (help))
 (if (string=? talkInput "tank")
   (tankStartListening)
   (let ((strLen (string-length talkInput)))
    (if (and (> strLen 11) (string=? "my name is " (substring talkInput 0 11)))
      (thread (changeName (substring talkInput 11 strLen))))))
 (if tankHangupTime
  (letrec ((words (split talkInput #\ ))
           (w1 (car words)))
   (cond
    ;((string=? "who" talkInput) (IpcWrite '(say "I'm here!")))
    ((string=? "load the jump program" talkInput) (tankTalk "I can't find the disk"))
    ((eqv? w1 "sex") (apply satc (cdr words)))
    ((string=? "march" talkInput) (avatar '(march)))
    ((string=? "walk around" talkInput) (avatar '(walkAround)))
    ((string=? "edit" talkInput) (begin (set! EDIT (not EDIT)) (tankTalk "Edit mode " EDIT)))
    ((string=? "island" talkInput) ((avatar 'jump) 1 4150 5602))
    ((string=? "theoffice" talkInput) ((avatar 'jump) 1 3869 1053))
    ((string=? "scrabble" talkInput)  ((avatar 'jump) 1 3338 3244))
    ((string=? "britania" talkInput)  ((avatar 'jump) 1 3456 2751))))))

; Sex and the City episode recommender
(define SexEpisodes #f)
(cond ((open-file "satc.scm" 1) => load))
; (load "satc.scm") ; defines SexEpisodes

(define (satc . args)
 (if SexEpisodes
 (let ((index #f))
  (if (null? args)
    (set! index (random (vector-length SexEpisodes)))
    (let ((s (- (read-string (car args)) 1)) ; Normalize season and episode number WRT 0
          (e (- (read-string (cadr args)) 1)))
      (cond ((or (< s 0) (< 5 s))
             (tankTalk "Only seasons 1 through 6 exist"))
            ((or (< e 0) (<= (vector-ref #(12 18 18 18 8 20) s) e))
             (tankTalk "Season " (+ s 1) " has only " (vector-ref #(12 18 18 18 8 20) s) " episodes"))
            (else
             (set! index (+ e (vector-ref #(0 12 30 48 66 74) s)))))))
  (if index (let ((d (vector-ref SexEpisodes index)))
    (tankTalk  "(s " (car d) " e " (cadr d) ") " (caddr d) "\r\n" (car (cdddr d))))))))

; Display the same string repeatedly with colors of increasing inensity.
(define (fancyDisplay c s)
 (for-each
   (lambda (c)
        (WinChatSetColor 0 c)
        (WinChatDisplay "\r" s)
        (sleep 50))
   (list 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 254 c))
 "")

; Pacman
(define ghostsOn #f)
(define desiredDir 'ghost)

; Create or destroy the four ghosts
(define ghosts
 (let ((g1 #f) (g2 #f) (g3 #f) (g4 #f))
 (lambda ()
  (WinChatDisplay "(ghosts)")
  (if g1
    (begin
      ((g1 'die)) ((g2 'die)) ((g3 'die)) ((g4 'die))
      (set! g1 #f) (set! g2 #f) (set! g3 #f) (set! g4 #f)
      )
    (begin
      (let ((z (avatar 'z)) (y (avatar 'y)) (x (avatar 'x)))
        (set! g1 (or ghostsOn (Avatar "G1" z (- y 1) (- x 1) ipc #t)))
        (set! g2 (or ghostsOn (Avatar "G2" z (- y 1) (+ x 1) ipc #t)))
        (set! g3 (or ghostsOn (Avatar "G3" z (+ y 1) (- x 1) ipc #t)))
        (set! g4 (or ghostsOn (Avatar "G4" z (+ y 1) (+ x 1) ipc #t)))
      )
      (g1 '(ghostMacro))
      (g2 '(ghostMacro))
      (g3 '(ghostMacro))
      (g4 '(ghostMacro))
)))))

; Given a direction, return a list of possible directions (same direction, left or right)
; after filtering out non-obstructed directions
(define (pacmanFilterDirections ent l)
  (filter-not
    (lambda (d)
      ((ent 'look) d)
      (let ((cell (apply ((ent 'myMap) 'firstCell) ((ent 'gpsLook)))))
        (or (not (cellValidIndex? cell)) (cellSolid? (cellRef cell)))))
    l))

; Generate list of possible directions for a ghost given a direction
; for now that's just left, the direction specified and right.
(define (pacmanNewGhostDirections ent dir)
  (pacmanFilterDirections ent (list (modulo (- dir 2) 8) dir (modulo (+ dir 2) 8))))

; Generate list with desired and/or current pacman directions
(define (pacmanNewPacmanDirections end dir)
  (pacmanFilterDirections end (list desiredDir dir)))

(define ghostMacro (macro ()
 (set! Stop #f)
 (WinChatDisplay "Lookout pacman!")
 (thread (let ~ ((dir dirLook)) ; Use this local symbol and not the Avatar class'.
   (if Stop
     (WinChatDisplay "The gosts give up")
     (begin
        (set! dir ; Pick a new direction after every movement
          (let ((v (list->vector (pacmanNewGhostDirections self dir))))
            (if (eq? #() v)
              (modulo (+ dir 4) 8) ; The new directions are all invalid so reverse direction
              (vector-random v)))); choose random new dir
        (walk dir)
        (sleep 200)
        (~ dir)))))))

; Handle pacman controls and adjust state for the pacman thread
(define (replPacman b) ; button
 (define newState 'pacman)
 (if (eq? b #\q)
   (begin
     (pacman)
     (set! newState 'cmd)) ; Call pacman to stop thread and set new state.
 (if (eq? b 'right) (set! desiredDir 0)
 (if (eq? b 'up)    (set! desiredDir 2)
 (if (eq? b 'left)  (set! desiredDir 4)
 (if (eq? b 'down)  (set! desiredDir 6)
 (if (eq? b #\l)  (set! desiredDir 0)
 (if (eq? b #\k)  (set! desiredDir 2)
 (if (eq? b #\h)  (set! desiredDir 4)
 (if (eq? b #\j)  (set! desiredDir 6)
 (if (eq? b #\g)    (set! desiredDir 'ghost)))))))))))
 newState) ; want to stay in the pacman state

(rem
 (let ((dir 0)) (lambda ()
  (if ghostsOn
    (begin
      (WinChatSetColor 0 1)
      (WinChatDisplay "\nYour pacman game is over."))
    (begin
      (WinChatSetColor 0 15)
      (WinChatDisplay "\nWelcome to pacman mode")
      (WinChatSetColor 0 7)
      (WinChatDisplay "\n q.......quit")
      (WinChatDisplay "\n g.......act like a ghost")
      (WinChatDisplay "\n arrows..move pacman")))
  (set! ghostsOn (not ghostsOn)) ; Call again to disable
  (thread (let ~ () (if ghostsOn (begin
    (semaphore-down walkSemaphore)
    (if (eq? desiredDir 'ghost)
      ; Ghost logic
      (begin
        (set! dir ; Pick a new direction after every movement
          (let ((v (list->vector (pacmanNewGhostDirections dir))))
            (if (eq? #() v)
              (modulo (+ dir 4) 8) ; The new directions are all invalid so reverse direction
              (vector-random v)))); choose random new dir
        ((avatar 'look) dir)
        (walkDetails))
      ; Pacman logic
      (let ((dirs (pacmanNewPacmanDirections dir)))
        (if (pair? dirs) (begin
          (set! dir (car dirs))
          ((avatar 'look) dir)
          (walkDetails)))))
    (semaphore-up walkSemaphore)
    (sleep 100)
    (~))))))))


; Prototype to display all cells in a window
;(define (chooseCell)
; (define WinCells ((Terminal 'WindowNew) 5 20 2 36 #x07))
; (define (WinCellsDisplay . e) (for-each (lambda (x) (for-each (WinCells 'puts) (display->strings x))) e))
; (define WinCellsSetColor  (WinCells 'set-color))
; (define WinCellsPutc  (WinCells 'putc))
; (sleep 1000)
; (loop 100 (lambda (k)
;   ((WinCells 'home))
;   (loop 10 (lambda (i)
;     (let ((c (cellGlyph (cellRef (+ i k)))))
;       (WinCellsSetColor (glyph0bg c) (glyph0fg c)) (WinCellsPutc (glyph0ch c))
;       (WinCellsSetColor (glyph1bg c) (glyph1fg c)) (WinCellsPutc (glyph1ch c))
;       (WinCellsSetColor 0 15)                      (WinCellsPutc (if (= i 4) #\[ (if (= i 5) #\] #\ ))))))
;   (WinCellsSetColor 0 15)
;   (WinCellsDisplay "\r\n" (cellSymbol (cellRef k)))
;   (sleep 500)))
; ((WinCells 'delete)))

; Load a map file and dump in the current map
;(define (p m) (mapUpdateColumns 3456 2752 32 (read (open-file m))))

(define walkgrid (macro () (thread
 (set! Stop #f)
 (let ~ ()
  (or Stop
   (cond ((begin (say 2) (look 2) (!= (lookAt) cell)) (walk 2) (sleep 200) (~))
         ((begin (say 4) (look 4) (!= (lookAt) cell)) (walk 4) (sleep 200) (~))
         ((begin (say 6) (look 6) (!= (lookAt) cell)) (walk 6) (sleep 200) (~))
         ((begin (say 0) (look 0) (!= (lookAt) cell)) (walk 0) (sleep 200) (~))
         (else (speak "not sure what to do now"))))))))

(define fillgrid (macro ()
 (cond ((begin (look 2) (!= (lookAt) cell))
        (walk 2)
        (buttonSetCell cell)
        (sleep 200)
        (fillgrid))
       ((begin (look 4) (!= (lookAt) cell))
        (walk 4)
        (buttonSetCell cell)
        (sleep 200)
        (fillgrid))
       ((begin (look 6) (!= (lookAt) cell))
        (walk 6)
        (buttonSetCell cell)
        (sleep 200)
        (fillgrid))
       ((begin (look 0) (!= (lookAt) cell))
        (walk 0)
        (buttonSetCell cell)
        (sleep 200)
        (fillgrid)))))

; The IRC agent.  Assigned an instantiated IrcAgent object.
(define irc #f)
(define (NewIrc . args)
  (or irc (thread (set! irc (IrcAgent WinConsole "IRC" (avatar 'z) (avatar 'y) (avatar 'x) ipc)))))

(define LightPoints '(((8 -2) . 5) ((8 -1) . 5) ((8 0) . 5) ((8 1) . 5) ((8 2) . 5) ((7 -4) . 5) ((7 -3) . 5) ((7 -2) . 10) ((7 -1) . 10) ((7 0) . 10) ((7 1) . 10) ((7 2) . 10) ((7 3) . 5) ((7 4) . 5) ((6 -5) . 5) ((6 -4) . 10) ((6 -3) . 10) ((6 -2) . 15) ((6 -1) . 15) ((6 0) . 15) ((6 1) . 15) ((6 2) . 15) ((6 3) . 10) ((6 4) . 10) ((6 5) . 5) ((5 -6) . 5) ((5 -5) . 10) ((5 -4) . 10) ((5 -3) . 15) ((5 -2) . 20) ((5 -1) . 20) ((5 0) . 20) ((5 1) . 20) ((5 2) . 20) ((5 3) . 15) ((5 4) . 10) ((5 5) . 10) ((5 6) . 5) ((4 -7) . 5) ((4 -6) . 10) ((4 -5) . 10) ((4 -4) . 15) ((4 -3) . 20) ((4 -2) . 20) ((4 -1) . 25) ((4 0) . 25) ((4 1) . 25) ((4 2) . 20) ((4 3) . 20) ((4 4) . 15) ((4 5) . 10) ((4 6) . 10) ((4 7) . 5) ((3 -7) . 5) ((3 -6) . 10) ((3 -5) . 15) ((3 -4) . 20) ((3 -3) . 20) ((3 -2) . 25) ((3 -1) . 30) ((3 0) . 30) ((3 1) . 30) ((3 2) . 25) ((3 3) . 20) ((3 4) . 20) ((3 5) . 15) ((3 6) . 10) ((3 7) . 5) ((2 -8) . 5) ((2 -7) . 10) ((2 -6) . 15) ((2 -5) . 20) ((2 -4) . 20) ((2 -3) . 25) ((2 -2) . 30) ((2 -1) . 35) ((2 0) . 35) ((2 1) . 35) ((2 2) . 30) ((2 3) . 25) ((2 4) . 20) ((2 5) . 20) ((2 6) . 15) ((2 7) . 10) ((2 8) . 5) ((1 -8) . 5) ((1 -7) . 10) ((1 -6) . 15) ((1 -5) . 20) ((1 -4) . 25) ((1 -3) . 30) ((1 -2) . 35) ((1 -1) . 35) ((1 0) . 40) ((1 1) . 35) ((1 2) . 35) ((1 3) . 30) ((1 4) . 25) ((1 5) . 20) ((1 6) . 15) ((1 7) . 10) ((1 8) . 5) ((0 -8) . 5) ((0 -7) . 10) ((0 -6) . 15) ((0 -5) . 20) ((0 -4) . 25) ((0 -3) . 30) ((0 -2) . 35) ((0 -1) . 40) ((0 0) . 40) ((0 1) . 40) ((0 2) . 35) ((0 3) . 30) ((0 4) . 25) ((0 5) . 20) ((0 6) . 15) ((0 7) . 10) ((0 8) . 5) ((-1 -8) . 5) ((-1 -7) . 10) ((-1 -6) . 15) ((-1 -5) . 20) ((-1 -4) . 25) ((-1 -3) . 30) ((-1 -2) . 35) ((-1 -1) . 35) ((-1 0) . 40) ((-1 1) . 35) ((-1 2) . 35) ((-1 3) . 30) ((-1 4) . 25) ((-1 5) . 20) ((-1 6) . 15) ((-1 7) . 10) ((-1 8) . 5) ((-2 -8) . 5) ((-2 -7) . 10) ((-2 -6) . 15) ((-2 -5) . 20) ((-2 -4) . 20) ((-2 -3) . 25) ((-2 -2) . 30) ((-2 -1) . 35) ((-2 0) . 35) ((-2 1) . 35) ((-2 2) . 30) ((-2 3) . 25) ((-2 4) . 20) ((-2 5) . 20) ((-2 6) . 15) ((-2 7) . 10) ((-2 8) . 5) ((-3 -7) . 5) ((-3 -6) . 10) ((-3 -5) . 15) ((-3 -4) . 20) ((-3 -3) . 20) ((-3 -2) . 25) ((-3 -1) . 30) ((-3 0) . 30) ((-3 1) . 30) ((-3 2) . 25) ((-3 3) . 20) ((-3 4) . 20) ((-3 5) . 15) ((-3 6) . 10) ((-3 7) . 5) ((-4 -7) . 5) ((-4 -6) . 10) ((-4 -5) . 10) ((-4 -4) . 15) ((-4 -3) . 20) ((-4 -2) . 20) ((-4 -1) . 25) ((-4 0) . 25) ((-4 1) . 25) ((-4 2) . 20) ((-4 3) . 20) ((-4 4) . 15) ((-4 5) . 10) ((-4 6) . 10) ((-4 7) . 5) ((-5 -6) . 5) ((-5 -5) . 10) ((-5 -4) . 10) ((-5 -3) . 15) ((-5 -2) . 20) ((-5 -1) . 20) ((-5 0) . 20) ((-5 1) . 20) ((-5 2) . 20) ((-5 3) . 15) ((-5 4) . 10) ((-5 5) . 10) ((-5 6) . 5) ((-6 -5) . 5) ((-6 -4) . 10) ((-6 -3) . 10) ((-6 -2) . 15) ((-6 -1) . 15) ((-6 0) . 15) ((-6 1) . 15) ((-6 2) . 15) ((-6 3) . 10) ((-6 4) . 10) ((-6 5) . 5) ((-7 -4) . 5) ((-7 -3) . 5) ((-7 -2) . 10) ((-7 -1) . 10) ((-7 0) . 10) ((-7 1) . 10) ((-7 2) . 10) ((-7 3) . 5) ((-7 4) . 5) ((-8 -2) . 5) ((-8 -1) . 5) ((-8 0) . 5) ((-8 1) . 5) ((-8 2) . 5)))

; Spawn a second avatar.  Your free kitteh.
(define kat #f)
(define (NewKat)
 (or kat (set! kat (Kat avatar))))


(define IcoSem (open-semaphore 0))

; Create an ico file based on where I am
(IcoInitialize (lambda x)) ; Disable ICO library debugging
(define (writeIco)
 (define ico (IcoRead "xterm256.ico"))
 (loop2 0 16 0 16 (lambda (y x)
   (IcoIndexedPixelSet ico 0 y x (glyph0fg (((avatarMap 'myCanvas) 'glyph)
                                            (+ -8 y (avatar 'y))
                                            (+ -8 x (avatar 'x)))))))
 (IcoWrite ico "favicon.ico")
 ;(semaphore-up IcoSem)
)

;<m>
; <r> <c a="," c="240"/>
;     <c a="," c="241"> ... </r>
; ...  ;  </m>
; A web server for the map icon
(define SendMapSem (open-semaphore 1))
(define SendMapFlag #t)

(define (walkOccuredForWebClient dir)
 (WinConsoleDisplay "\r\n" (tid) "::(walkOccuredForWebClient)")
 (if (not SendMapFlag)
    (begin (set! SendMapFlag #t)
           (semaphore-up SendMapSem)))
 (WinConsoleDisplay "\r\n" (tid) "  --(walkOccuredForWebClient)")
)

(define (sendMapWait p)
 (WinConsoleDisplay "\r\n" (tid) "::(sendMapWait)")
 (semaphore-down SendMapSem)
 (set! SendMapFlag #f)
 (sendMap p)
 (WinConsoleDisplay "\r\n" (tid) "  --(sendMapWait)")
)

(define (sendMap p)
 (define strs ()) ; List of strings to send
 (define (add . objs)
   (for-each (lambda (o) (for-each (lambda (s) (set! strs (cons s strs)))
                                   (display->strings o)))
             objs))
 (add "<m>")
 (loop2 0 16 0 16 (lambda (y x)
   (if (= x 0) (add "<r>"))
   (add "<c f='")
   (add (glyph0fg (((avatarMap 'myCanvas) 'glyph) (+ -8 y (avatar 'y)) (+ -8 x (avatar 'x)))))
   (add "' c='")
   (add (glyph0ch (((avatarMap 'myCanvas) 'glyph) (+ -8 y (avatar 'y)) (+ -8 x (avatar 'x)))))
   (add "'/>")
   (add "<c f=\'")
   (add (glyph1fg (((avatarMap 'myCanvas) 'glyph) (+ -8 y (avatar 'y)) (+ -8 x (avatar 'x)))))
   (add "' c='")
   (add (glyph1ch (((avatarMap 'myCanvas) 'glyph) (+ -8 y (avatar 'y)) (+ -8 x (avatar 'x)))))
   (add "'/>")
   (if (= 15 x) (add "</r>"))))
 (add "</m>")
 (sendHeaderXML p (apply + (map string-length strs)))
 (let ~ ((ss strs))
   (if (null? ss) 'done
     (begin (~ (cdr ss))
            (display (car ss) p)))))

(define (sendChatHeader p s)
 (outl p "HTTP/1.1 200 OK")
 (outl p "Content-Type: text/plain")
 (outl p "Server: World[tm]")
 (outl p "Connection: keep-alive")
 (outl p "Content-Length: " (string-length s))
 (outl p "")
 (outl p s))

(define (sendChat0 p)
 (sendChatHeader p "Conected to World[tm]!"))

; Incoming chat needs to be sent to web client
(define ChatQueue (QueueCreate))

(define (WWWVoiceCallBack str)
 (QueueAdd ChatQueue str))

(define (acceptTalk p s)
 ((avatar 'speak) s)
 (sendChatHeader p "OK"))


(define (sendChat p)
 (sendChatHeader p (QueueGet ChatQueue)))

; Global object for debugging at the command line
(define www ())

(define (NewWWW)
  ; Initialize web module's debug callback
  (WWWDebugSet WinConsoleDisplay)
  (set! www (HTTPServer 7180))
  (WWWRegisterGetHandler
   (lambda (request)
    (define p (request 'Stream))
    (define s (chopSlash (request 'URI)))
    (cond
      ((eqv? s "map") (sendMapWait p) #t)
      ((eqv? s "map0") (sendMap p) #t)
      ((eqv? s "talk")  (acceptTalk p (request 'Post)) #t)
      ((eqv? s "chat")  (sendChat p) #t)
      ((eqv? s "chat0") (sendChat0 p) #t)
      ((eqv? s "favicon.ico")
       ;(semaphore-down IcoSem)
       (sendFileIcon p s) #t)
      ((eqv? s "") (sendFileHtml p "world.html") #t)
      ((eqv? s "f.html") (sendFileHtml p "f.html") #t)
      ((eqv? s "3d.html") (sendFileHtml p "3d.html") #t)
      ((eqv? s "c256.css") (sendFileCss p "c256.css") #t)
      ((eqv? s "lambda16.cur") (sendFileIcon p "lambda16.cur") #t)
      ((eqv? s "mover.js") (sendFileJavascript p "mover.js") #t)
      (else #f))))

  (WWWRegisterGetHandler
   (lambda (request)
    (define p (request 'Stream))
    (define s (chopSlash (request 'URI)))
    (let ((ret #f)
          (toks (strtok s #\+)))
      (if (eqv? (car toks) "walk")
        (begin
          (set! ret #t)
          (cond ((eqv? (cdr toks) "0") (walk 0))
                ((eqv? (cdr toks) "2") (walk 2))
                ((eqv? (cdr toks) "4") (walk 4))
                ((eqv? (cdr toks) "6") (walk 6))
                (else (walk 1)))
          (sendHeaderText p 0)))
      ret)))
  ; Create the web server object
  (avatar '(set! WalkCallback walkOccuredForWebClient))
  (avatar '(set! VoiceCallback WWWVoiceCallBack))
  (thread ((www 'Start))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Genesis
;;

(WinConsoleDisplay "[IM IN UR CONSOLE READING UR THAWTS]")

; Create ipc object.  Pass in a serializer which prints to the console window.
(define ipc (Ipc WinConsoleDisplay HUB-PORT))
(ipc '(set! Debug #f))

; TODO an often used call by the un-refactored code
(define IpcWrite (ipc 'qwrite))

; Animated welcome marquee
(or QUIETLOGIN (thread (welcome)))

; Get username.  Create avatar object.
(define avatar (if QUIETLOGIN "Administrator" (boxInput "Enter your name")))
(if (eq? "" avatar) (set! avatar "Guest"))
(set! avatar (Avatar avatar 1 3464 2767 ipc #f))
;(set! avatar (Avatar avatar 1 3438 2735 ipc #f)) ; Pacman arena
(avatar '(set! climb #t))

; Consider the avatar's map object
(define avatarMap (avatar 'myMap))

; TODO still used in handleTerminalResize winmapUp/Down/Left/Right mouseWalkActionHandlerLoop
(define avatarViewport (avatarMap 'myViewport))

(define moveableWin avatarViewport)

; Start map mouse action handler
(mouseWalkActionHandlerLoop)

; Catch some signals to control a proper shut-down
; TODO buggy repeated calls to the same handler occurs with I/O signals
(signal-set 1  (lambda () (shutdown "Signal 1 HUP")))
(signal-set 2  (lambda () (shutdown "Signal 2 INT")))
(signal-set 3  (lambda () (shutdown "Signal 3 QUIT")))
(signal-set 6  (lambda () (shutdown "Signal 6 ABRT")))
(signal-set 13 (lambda () (shutdown "Signal 13 PIPE")))
(signal-set 15 (lambda () (shutdown "Signal 15 TERM")))
(signal-set 28 (lambda () (sigwinch) (unthread))) 

; Display welcome information an announce my presence
(or QUIETLOGIN (begin
 (fancyDisplay 13 (string "Welcome to World, " (avatar 'name)))
 (WinChatSetColor 0 10) (WinChatDisplay "\nHit ? to toggle the help window")
 (WinChatSetColor 0 6) (WinChatDisplay "\nSee http://code.google.com/p/worldtm")
 (saySystem (avatar 'name)
  (vector-random #(" *emerges from the Interwebs*"
                   " *CONNECT 2400*"
                   " *CONNECT 14400/ARQ/V34/LAPM/V42BIS*"
                   ;" *PUSH* *SQUIRT* *SPANK* *WAAAAAAAAA*"
                   ;" *All Worldlians Want to Get Borned*"
                   ;" *Happy Birthday*"
                   ;" *I thought you were in Hong Kong*"
                   " *turns on a VT100*")))))

; Call this to quit world
(define (shutdown . msg)
  (if (or (pair? msg) QUIETLOGIN (boxBool "Quit?")) (begin
    (set! SHUTDOWN #t)
    (or QUIETLOGIN (saySystem (avatar 'name) " exits  " (if (pair? msg) (car msg) "")))
    ((avatar 'die)) ; Force an IPC message so avatar's IPC reader thread calls die method
    (sleep 1000)
    (displayl "\e[" (Terminal 'Theight) "H\r\n\e[0m\e[?25h\e[?1000l\r\n")
    (quit))))


; Start Web client server
;(if QUIETLOGIN (NewWWW))

; Keyboard command loop
(let ~ () (let ((b (getKey)))
  (set! ActivityTime (time))
  (button b)
  (~)))
