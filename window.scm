;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Concerns
; redrawing while scrolling up conflict with 'toprow' variable.

(or (procedure? QueueCreate) (begin
  (display "ERROR: window.scm requires adt.scm.  Halting.\n")
  (quit)))

; Return the char if printable and a #\. otherwise
(define (char->visible c)
 (vector-ref #(
#\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  
#\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.
#\   #\!  #\"  #\#  #\$  #\%  #\&  #\'  #\(  #\)  #\*  #\+  #\,  #\-  #\.  #\/
#\0  #\1  #\2  #\3  #\4  #\5  #\6  #\7  #\8  #\9  #\:  #\;  #\<  #\=  #\>  #\?
#\@  #\A  #\B  #\C  #\D  #\E  #\F  #\G  #\H  #\I  #\J  #\K  #\L  #\M  #\N  #\O
#\P  #\Q  #\R  #\S  #\T  #\U  #\V  #\W  #\X  #\Y  #\Z  #\[  #\\  #\]  #\^  #\_
#\`  #\a  #\b  #\c  #\d  #\e  #\f  #\g  #\h  #\i  #\j  #\k  #\l  #\m  #\n  #\o
#\p  #\q  #\r  #\s  #\t  #\u  #\v  #\w  #\x  #\y  #\z  #\{  #\|  #\}  #\~  #\.
#\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.
#\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.  #\.
#\   #\¡  #\¢  #\£  #\¤  #\¥  #\¦  #\§  #\¨  #\©  #\ª  #\«  #\¬  #\­  #\®  #\¯
#\°  #\±  #\²  #\³  #\´  #\µ  #\¶  #\·  #\¸  #\¹  #\º  #\»  #\¼  #\½  #\¾  #\¿
#\À  #\Á  #\Â  #\Ã  #\Ä  #\Å  #\Æ  #\Ç  #\È  #\É  #\Ê  #\Ë  #\Ì  #\Í  #\Î  #\Ï
#\Ð  #\Ñ  #\Ò  #\Ó  #\Ô  #\Õ  #\Ö  #\×  #\Ø  #\Ù  #\Ú  #\Û  #\Ü  #\Ý  #\Þ  #\ß
#\à  #\á  #\â  #\ã  #\ä  #\å  #\æ  #\ç  #\è  #\é  #\ê  #\ë  #\ì  #\í  #\î  #\ï
#\ð  #\ñ  #\ò  #\ó  #\ô  #\õ  #\ö  #\÷  #\ø  #\ù  #\ú  #\û  #\ü  #\ý  #\þ  #\ÿ) c))

; 256 color terminal escape sequence interface.
(define colorTable
  (let ((tbl (make-vector 65536)))
    (loop 65536 (lambda (i)
        (vector-set! tbl i (string "\e[48;5;" (number->string (/ i 256))
                                   ";38;5;" (number->string (modulo i 256)) "m"))))
    tbl))

; 16 color version of the 256 color table
(rem define colorTable
  (let ((tbl (make-vector 65536)))
    (loop 65536 (lambda (i)
        (vector-set! tbl i (string "\e[" (if (> (modulo i 16) 7) "1;3" "0;3")
                                         (number->string (modulo i 8))
                                   ";4" (number->string (modulo (/ i 16) 8)) "m"))))
    tbl))

(define (integer->colorstring i) (vector-ref colorTable i))

; Old 16 color table
;(define (integer->colorstring i)
; (vector-ref #("\e[0;40;30m" "\e[0;40;31m" "\e[0;40;32m" "\e[0;40;33m"
;               "\e[0;40;34m" "\e[0;40;35m" "\e[0;40;36m" "\e[0;40;37m"
;               "\e[1;40;30m" "\e[1;40;31m" "\e[1;40;32m" "\e[1;40;33m"
;               "\e[1;40;34m" "\e[1;40;35m" "\e[1;40;36m" "\e[1;40;37m"
;
;               "\e[0;41;30m" "\e[0;41;31m" "\e[0;41;32m" "\e[0;41;33m"
;               "\e[0;41;34m" "\e[0;41;35m" "\e[0;41;36m" "\e[0;41;37m"
;               "\e[1;41;30m" "\e[1;41;31m" "\e[1;41;32m" "\e[1;41;33m"
;               "\e[1;41;34m" "\e[1;41;35m" "\e[1;41;36m" "\e[1;41;37m"
;
;               "\e[0;42;30m" "\e[0;42;31m" "\e[0;42;32m" "\e[0;42;33m"
;               "\e[0;42;34m" "\e[0;42;35m" "\e[0;42;36m" "\e[0;42;37m"
;               "\e[1;42;30m" "\e[1;42;31m" "\e[1;42;32m" "\e[1;42;33m"
;               "\e[1;42;34m" "\e[1;42;35m" "\e[1;42;36m" "\e[1;42;37m"
;
;               "\e[0;43;30m" "\e[0;43;31m" "\e[0;43;32m" "\e[0;43;33m"
;               "\e[0;43;34m" "\e[0;43;35m" "\e[0;43;36m" "\e[0;43;37m"
;               "\e[1;43;30m" "\e[1;43;31m" "\e[1;43;32m" "\e[1;43;33m"
;               "\e[1;43;34m" "\e[1;43;35m" "\e[1;43;36m" "\e[1;43;37m"
;
;               "\e[0;44;30m" "\e[0;44;31m" "\e[0;44;32m" "\e[0;44;33m"
;               "\e[0;44;34m" "\e[0;44;35m" "\e[0;44;36m" "\e[0;44;37m"
;               "\e[1;44;30m" "\e[1;44;31m" "\e[1;44;32m" "\e[1;44;33m"
;               "\e[1;44;34m" "\e[1;44;35m" "\e[1;44;36m" "\e[1;44;37m"
;
;               "\e[0;45;30m" "\e[0;45;31m" "\e[0;45;32m" "\e[0;45;33m"
;               "\e[0;45;34m" "\e[0;45;35m" "\e[0;45;36m" "\e[0;45;37m"
;               "\e[1;45;30m" "\e[1;45;31m" "\e[1;45;32m" "\e[1;45;33m"
;               "\e[1;45;34m" "\e[1;45;35m" "\e[1;45;36m" "\e[1;45;37m"
;
;               "\e[0;46;30m" "\e[0;46;31m" "\e[0;46;32m" "\e[0;46;33m"
;               "\e[0;46;34m" "\e[0;46;35m" "\e[0;46;36m" "\e[0;46;37m"
;               "\e[1;46;30m" "\e[1;46;31m" "\e[1;46;32m" "\e[1;46;33m"
;               "\e[1;46;34m" "\e[1;46;35m" "\e[1;46;36m" "\e[1;46;37m"
;
;               "\e[0;47;30m" "\e[0;47;31m" "\e[0;47;32m" "\e[0;47;33m"
;               "\e[0;47;34m" "\e[0;47;35m" "\e[0;47;36m" "\e[0;47;37m"
;               "\e[1;47;30m" "\e[1;47;31m" "\e[1;47;32m" "\e[1;47;33m"
;               "\e[1;47;34m" "\e[1;47;35m" "\e[1;47;36m" "\e[1;47;37m") i))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal of Windows, Keyboard and Mouse Class
;;
(define (Terminal)
 (define (self msg) (eval msg))
 (define MAXWINDOWCOUNT 128)
 (define TCOLOR #f)
 (define TCURSOR-VISIBLE #t)
 (define Theight 0)
 (define Twidth  0)
 (define GY -1) ; Global cursor positions.
 (define GX -1)
 (define WindowVec (make-vector MAXWINDOWCOUNT #f)) ; Vector of window objects by ID
 (define WindowList ()) ; List of window objects in display order
 (define TerminalSemaphore (open-semaphore 1))
 (define PublicSemaphore (open-semaphore 1))
 ; 2D table of visible window objects.  #f represents no window which is never drawn on.  Considering
 ; having a default base window always in existence which could act as a static/dynamic background image.
 (define WindowMask ())
 ; Methods
 (define (ResetTerminal termSize)
  (set! Theight (cdr termSize)) (if (< Theight 2) (set! Theight 2))
  (set! Twidth  (car termSize)) (if (< Twidth 2) (set! Twidth 2))
  (set! WindowMask (make-vector-vector Theight Twidth #f))
  (WindowMaskReset 0 0 (- Theight 1) (- Twidth 1)))

 (define (InsideTerminal? y x)
   (and (>= y 0) (>= x 0) (< y Theight) (< x Twidth)))

 (define (topWin y x)
   (and (InsideTerminal? y x)
        (vector-vector-ref WindowMask y x)))

 (define (gputc char color y x)
   (semaphore-down TerminalSemaphore)
   ; Set color.
   (if (!= TCOLOR color) (begin
     (set! TCOLOR color)
     (display (integer->colorstring color))))
   ; Set cursor location.
   (if (or (!= y GY) (!= x GX)) (begin
     (send "\e[" stdout)
     (display (+ y 1))
     (send ";" stdout)
     (display (+ x 1))
     (send "H" stdout)
     (set! GY y) (set! GX x)))
   ; Draw character.
   (display (char->visible char))
   ; Update cursor.
   (set! GX (+ 1 GX))
   (if (<= Twidth GX)
       (begin
         (set! GX 0)
         (set! GY (+ 1 GY))
         ;(if (<= Theight GY) (set! GY (- Theight 1))) ; Allow the cursor to be off screen to force a cursor move.
       ))
   (semaphore-up TerminalSemaphore))

 (define (tcursor-visible)
   (semaphore-down TerminalSemaphore)
   (display (if TCURSOR-VISIBLE "\e[?25l" "\e[?25h"))
   (set! TCURSOR-VISIBLE (not TCURSOR-VISIBLE))
   (semaphore-up TerminalSemaphore))

 (define (TopmostWindowDiscover gy gx)
 ; Return first window object in window list that is visible at this location.
   (let ~ ((w WindowList))
     (if (null? w) #f ; No windows are visible at this location
     (if (((car w) 'InsideWindow?) gy gx) (car w) ; A window is visible at this location
     (~ (cdr w))))))

 ; Reset the terminal window mask and adjust the window's visible count
 (define (WindowMaskReset y0 x0 y1 x1)
  (loop2 y0 (+ y1 1) x0 (+ x1 1) (lambda (gy gx)
     (if (InsideTerminal? gy gx)
      (let ((prevwin (topWin gy gx)) ; Could be #f
            (topwin (TopmostWindowDiscover gy gx))) ; Get top win at global pos
        (vector-vector-set! WindowMask gy gx topwin) ; Cache it
        ; Update the visible count for the window(s) at this global location.
        (if prevwin
          (if (not (eq? prevwin topwin))             ; Previous window here
             (begin
               ((prevwin 'visibleCountAdd) -1)
               (if topwin ((topwin 'visibleCountAdd) 1))))
          (if topwin ((topwin 'visibleCountAdd) 1))) ; No previous window here
        (if topwin
          ((topwin 'globalRefresh) gy gx) ; Redraw glyph of this window at its global position.
          (gputc #\# #x08 gy gx))))))) ; Default background terminal glyph

 (define (lock)
   (semaphore-down PublicSemaphore))

 (define (unlock)
   (semaphore-up PublicSemaphore))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Window_subclass
 ;;
 (define (WindowNew Y0 X0 Wheight Wwidth COLOR . ChildStack)
   (define (self msg) (eval msg))
   (define id
     (let ~ ((i 1))
       (cond ((= i MAXWINDOWCOUNT)
              (display "ERROR: WindowNew: MAXWINDOWCOUNT exceeded")
              #f)
             ((vector-ref WindowVec i)
              (~ (+ i 1)))
             (else (vector-set! WindowVec i self)
                   i))))
   (define Y1 (+ Y0 Wheight))
   (define X1 (+ X0 Wwidth))
   (define TY 0)
   (define TX 0)
   ; 2d vector of cell descriptors #(color char) AKA glyph.
   (define DESC (vector-vector-map!
                  (lambda (x) (vector COLOR #\ ))
                  (make-vector-vector Wheight Wwidth ())))
   ; 2d 'alpha' channel table.  Every postion can be toggled
   ; visible or invisible allowing the window beneath to show.
   ; Initially every location of the window is visible, assuming
   ; it's within the terminal boundary.
   (define ALPHA (make-vector-vector Wheight Wwidth #t))
   (define CURSOR-VISIBLE #t)
   (define ENABLED #t)
   (define topRow 0) ; For vertical scrolling.
   (define needToScroll #f) ; For bottom right character printing.
   (define (cursor-visible s) (set! CURSOR-VISIBLE s))
   (define WindowSemaphore (open-semaphore 1))
   (define ScrollbackHack #f)
   (define VisibleCount 0) ; Number of visible non-obstructed locations
	; Methods
   (define (getColor y x) (vector-ref (vector-vector-ref DESC y x) 0))
   (define (getChar  y x) (vector-ref (vector-vector-ref DESC y x) 1))
   (define (visibleCountAdd c) (set! VisibleCount (+ VisibleCount c)))
   (define (goto y x)
     (set! needToScroll #f)
     (set! TY (min (max 0 y) (- Wheight 1)))
     (set! TX (min (max 0 x) (- Wwidth 1))))
   (define (set-color b f) (set! COLOR (+ (* 256 b) f))) ; 256 colors each fg and bg packed into 16 bits
   (define (InsideWindow? gy gx)
     (and ENABLED
          (>= gy Y0)
          (>= gx X0)
          (<  gy Y1)
          (<  gx X1)
          (vector-vector-ref ALPHA (- gy Y0) (- gx X0))))
   (define (home) (goto 0 0))
   (define (return)
     (set! TX 0))
   (define (newline)
     (set! TY (+ 1 TY))
     (if (>= TY Wheight) (begin
           (set! TY (- Wheight 1))
           (scrollUp)
           (set! needToScroll #f))))
   (define (back)
     (if (and (< 0 TX) (< TX Wwidth))
       (begin (set! TX (- TX 1))
              (set! needToScroll #f)
              (display CHAR-CTRL-H))))
   (define (backspace c)
     (if (and (< 0 TX) (< TX Wwidth))
       (begin (set! TX (- TX 1))
              (set! needToScroll #f)
              (semaphore-down WindowSemaphore)
              (putchar c)
              (semaphore-up WindowSemaphore)
              (display CHAR-CTRL-H) 
              (set! needToScroll #f)
              (set! TX (- TX 1)))))
   (define (hardwareScrollable?)
     (and (= Wwidth Twidth)
          (< 1 Wheight)
          (= VisibleCount (* Wwidth Wheight))))
   (define (scrollUp)
     (if (hardwareScrollable?)
       (begin
         (displayl "\e7\e[" (+ Y0 1) ";" (+ Y0 Wheight) "r\e[" (+ Y0 Wheight) "H\n")
         (displayl "\e[r\e8")
         (loop Wwidth (lambda (x) ; Clear row which will become the bottom row
           (let ((desc (vector-vector-ref DESC topRow x)))
             (vector-set! desc 0 COLOR)
             (vector-set! desc 1 #\ ))))
         (set! topRow (modulo (+ topRow 1) Wheight)) ; Shift buffer
         (repaintRow (- Wheight 1)))
       (begin
         ; Clear top-row which is to become the bottom row.
         ; Force topmost line off the top of the terminal in hopes
         ; of filling the client's terminal backscroll buffer. Set
         ; 2 line scrolling region, move cursor, clear line.
         (if ScrollbackHack (begin
           (display "\e7\e[1;2r\e[H")
           (let ~ ((x 0))
             (if (>= x Wwidth) 'done
               (let ((desc (vector-vector-ref DESC topRow x)))
                  (displayl (integer->colorstring (vector-ref desc 0)) (vector-ref desc 1))
                  (~ (+ x 1)))))
           (display "\e[K\n\n\e[r\e8")
           (WindowMaskReset 0 0 2 Twidth)))
         (loop Wwidth (lambda (x) ; Clear row which will become the bottom row
           (let ((desc (vector-vector-ref DESC topRow x)))
             (vector-set! desc 0 COLOR)
             (vector-set! desc 1 #\ ))))
         (set! topRow (modulo (+ topRow 1) Wheight)) ; Shift buffer
         ; Refresh window.
         (repaint))))
   (define (repaintRow row)
     (loop Wwidth (lambda (x)
       (let ((desc (vector-vector-ref DESC (modulo (+ row topRow) Wheight) x)))
         (and (InsideTerminal? (+ row Y0) (+ x X0))
              (eq? self (topWin (+ row Y0) (+ x X0)))
              (gputc (vector-ref desc 1)
                     (vector-ref desc 0)
                     (+ row Y0) (+ x X0)))))))
   (define (repaint)
     (loop2 0 Wheight 0 Wwidth (lambda (y x)
       (let ((desc (vector-vector-ref DESC (modulo (+ y topRow) Wheight) x)))
         (and (InsideTerminal? (+ y Y0) (+ x X0))
              (eq? self (topWin (+ y Y0) (+ x X0)))
              (gputc (vector-ref desc 1)
                     (vector-ref desc 0)
                     (+ y Y0) (+ x X0)))))))
   ; Repaint char given global coordinate.  Does not mutate window
   ; state.  Modulo the Y coordinate due to horizontal scrolling.
   (define (globalRefresh gy gx)
     (let ((desc (vector-vector-ref DESC
                   (modulo (+ (- gy Y0) topRow) Wheight)
                   (- gx X0))))
       (gputc (vector-ref desc 1) (vector-ref desc 0) gy gx)))
   (define (putchar c)
     (if needToScroll (begin (set! needToScroll #f) (return) (newline)))
     (if (not (eq? TCURSOR-VISIBLE CURSOR-VISIBLE)) (tcursor-visible))
     (if (eq? c NEWLINE) (newline)
     (if (eq? c RETURN) (return)
     (if (eq? c CHAR-CTRL-G) (display c)
     (let ((gy (+ TY Y0))
           (gx (+ TX X0)))
       (begin
         ; Send character to terminal only if window location is visible.
         (and (InsideTerminal? gy gx)
              (eq? self (topWin gy gx))
              (gputc c COLOR gy gx))
         ; Cache color and char to buffer.
         (let ((desc (vector-vector-ref DESC (modulo (+ TY topRow) Wheight) TX)))
           (vector-set! desc 0 COLOR)
           (vector-set! desc 1 c))
         ; Advance cursor.
         (set! TX (+ 1 TX))
         (if (>= TX Wwidth)
           (if (= TY (- Wheight 1))
               (set! needToScroll #t) ; TODO remove this line then develop a framework to debug the ensuing issue
               (begin 
                 (return)
                 (newline))))))))))
   (define (putc c)
     (semaphore-down WindowSemaphore)
     (putchar c)
     (semaphore-up WindowSemaphore))
   (define (puts str)
     (semaphore-down WindowSemaphore)
     (loop (string-length str) (lambda (i) (putchar (string-ref str i))))
     (semaphore-up WindowSemaphore))
   (define (toggle . state)
     (let ((newstate (if (null? state) (not ENABLED) (car state))))
       (or (= state newstate) (begin ; If same state, do nothing
         (set! ENABLED newstate)
         (WindowMaskReset Y0 X0 Y1 X1)))))
   (define (alpha y x a) ; Create transparent 'pixel'
     (vector-vector-set! ALPHA y x a)
     (if ENABLED (WindowMaskReset (+ y Y0) (+ x X0) (+ y Y0 1) (+ x X0 1))))
   (define (move y x)
     (let ((oY0 Y0)
           (oX0 X0)
           (oY1 Y1)
           (oX1 X1))
       (semaphore-down WindowSemaphore)
       (set! Y0 y)
       (set! X0 x)
       (set! Y1 (+ Y0 Wheight))
       (set! X1 (+ X0 Wwidth))
       (if ENABLED (WindowMaskReset (min oY0 Y0) (min oX0 X0) (max oY1 Y1) (max oX1 X1))) ; Redraw window
       (semaphore-up WindowSemaphore)))
   (define (resize h w)
     (let ((oY1 Y1)
           (oX1 X1))
       (semaphore-down WindowSemaphore)
       (set! Wheight h)
       (set! Wwidth w)
       (set! Y1 (+ Y0 Wheight))
       (set! X1 (+ X0 Wwidth))
       (set! TY (min TY (- Wheight 1))) ; Make sure cursor is not out of bounds.
       (set! TX (min TX (- Wwidth 1)))
       (set! ALPHA (make-vector-vector Wheight Wwidth #t))
       (set! DESC (vector-vector-map! (lambda (x) (vector COLOR #\ ))
                                      (make-vector-vector Wheight Wwidth ())))
       (if ENABLED (WindowMaskReset Y0 X0 (max oY1 Y1) (max oX1 X1))) ; Redraw window
       (set! topRow 0)
       (semaphore-up WindowSemaphore)))
   (define (moveresize y x h w)
     (let ((oY0 Y0)
           (oX0 X0)
           (oY1 Y1)
           (oX1 X1))
       (semaphore-down WindowSemaphore)
       (set! Y0 y)
       (set! X0 x)
       (set! Wheight h)
       (set! Wwidth w)
       (set! Y1 (+ Y0 Wheight))
       (set! X1 (+ X0 Wwidth))
       (set! TY (min TY (- Wheight 1))) ; Make sure cursor is not out of bounds.
       (set! TX (min TX (- Wwidth 1)))
       (set! ALPHA (make-vector-vector Wheight Wwidth #t))
       (set! DESC (vector-vector-map! (lambda (x) (vector COLOR #\ ))
                                      (make-vector-vector Wheight Wwidth ())))
       (if ENABLED (WindowMaskReset
                      (min oY0 Y0) (min oX0 X0) ; Redraw window
                      (max oY1 Y1) (max oX1 X1)))
       (set! topRow 0)
       (semaphore-up WindowSemaphore)))
   (define (delete)
    (vector-set! WindowVec id #f)
    (set! WindowList (list-delete WindowList self))
    (close-semaphore WindowSemaphore)
    (WindowMaskReset Y0 X0 Y1 X1))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Buffer_subclass
   ;;
   (define Buffer (let ((parent self)) (lambda ()
     (define (self msg) (eval msg))
     (define Buffer (list "")) ; List of every line sent.  Append a new empty string.
     (define BufferCount 0)
     (define offset 0) ; Scroll back offset location index.  0 means end of buffer, greater than 0 number of lines back to scroll.
     ; Need to parse the string for newlines so a list of lines can be assembled over time
     (define (parseString str)
       (let ~ ((i 0)
               (len (string-length str))
               (line (car Buffer))) ; Current new empty string text line
          (if (= i len)
            (set-car! Buffer line) ; Save currently parsed line back to buffer
            (let ((ch (string-ref str i))) ; Otherwise process the next character
              (if (pair? (memq ch (list RETURN NEWLINE)))
                (begin
                  (if (not (eq? line ""))
                    (begin (set! Buffer (cons "" (cons line (cdr Buffer))))
                           (set! BufferCount (+ 1 BufferCount)))) ; Add new line to line bufer
                  (set! line ""))
                (set! line (string line ch))) ; Add char to parsed line
              (~ (+ i 1) len line)))))
     (define (redrawBuffer)
       (let ~ ((c Wheight) (b (list-skip Buffer offset)))
         (if (null? b) (set! b (list ""))) ; Don't expect the modified buffer list to be empty.  Bad base case.
         (if (or (= c 1) (null? (cdr b)))
           (home)
           (~ (- c 1) (cdr b)))
         (if (not (eq? "" (car b)))
           (begin
             (if (or (!= TY 0) (!= TX 0)) (begin (return) (newline))) ; Don't newline if home
             ((parent 'puts) (car b))))))
     (define (scrollHome)
       (if (< offset (- BufferCount Wheight -2))
         (begin
           (set! offset (- BufferCount Wheight -2))
           ((parent 'resize) Wheight Wwidth) ; Force a reset of the window glyphs
           (redrawBuffer))))
     (define (scrollEnd)
       (if (!= offset 0)
         (begin
           (set! offset 0)
           ((parent 'resize) Wheight Wwidth) ; Force a reset of the window glyphs
           (redrawBuffer))))
     (define (scrollBack)
       (if (< offset (- BufferCount Wheight -2))
         (begin
           (set! offset (+ 1 offset))
           ((parent 'resize) Wheight Wwidth) ; Force a reset of the window glyphs
           (redrawBuffer))))
     (define (scrollForward)
       (if (< 0 offset) 
         (begin (set! offset (- offset 1))
                ((parent 'resize) Wheight Wwidth) ; Force a reset of the window glyphs
                (redrawBuffer))))
     (define (puts str)
       (parseString str)
       ((parent 'puts) str))
     (define (resize h w)
       ((parent 'resize) h w)
       (redrawBuffer))
     (define (moveresize y x h w)
       ((parent 'moveresize) y x h w)
       (redrawBuffer))
     self)))
   ;;
   ;; Buffer_subclass
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ; When a window is instantiated, insert this new window object
   ; on top of the global window object list and initialize the
   ; window area.
   (if id
    (begin
      (set! WindowList (cons self WindowList)) ; TODO this needs a semaphore.
      (WindowMaskReset Y0 X0 Y1 X1)
      (if (pair? ChildStack)
        ; childstack = ((child parameters) child-macro . reset of child stack)
        (apply (cadr ChildStack) self (append (car ChildStack) (cddr ChildStack)))
        self))
    #f))
 ;;
 ;; Window_subclass
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ; Call to instantiate a buffer object which inherits a window object.
 (define (BufferNew Y0 X0 Wheight Wwidth COLOR)
   (let ((win (WindowNew Y0 X0 Wheight Wwidth COLOR)))
     (if win ((win 'Buffer)) #f )))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Keyboard_and_mouse
 ;;
 ; Stack of queues, implemented as a list, accepting keyboard characters
 (define keyQueueStack ())

 ; Vector of event (action y x) queues one per window.  TODO only 128 windows supported
 (define mouseQueueVector (make-vector MAXWINDOWCOUNT #f))

 ;; Keyboard character handling
 (define (keyQueueStackRegister q)
   (set! keyQueueStack (cons q keyQueueStack)))

 (define (keyQueueStackUnRegister q)
   (or (eq? (car keyQueueStack) q) (display "WARNING: keyQueueStackUnRegister: not top queue"))
   (set! keyQueueStack (list-delete keyQueueStack q)))

 (define (keyDispatch . charList)
   (if (pair? keyQueueStack) ; Only if a key queue exists is on the stack.  There might be nothing expecting keyboard characters.
     (map (lambda (c) (QueueAdd (car keyQueueStack) c)) charList)))
 
 ;; Mouse character string handling
 (define (mouseQueueRegister win q) ; Was mouseDispatcherRegister 
   (vector-set! mouseQueueVector (win 'id) q))

 (define (mouseQueueUnRegister win) ; Was mouseDispatcherUnRegister
   (mouseQueueRegister win #f))

 (define (mouseDispatch event y x) ; Send the handler "'mouse0 2 3" for example.
   (letrec ((win ((Terminal 'topWin) y x))
            (id (if win (win 'id) #f))
            (q (if id (vector-ref mouseQueueVector id) #f)))
     ;(or win (WinChatDisplay "\r\n" (list event y x)))
     (if q  (begin
              (QueueAdd q (list event (- y (win 'Y0)) (- x (win 'X0))))))))
 
 ; State machine to read and parse stdin.
 
 ; An "\e" scanned
 (define (keyScannerEsc)
  (let ((c (read-char 500 stdin))) ; Return character stdin or #f after 500ms
    (cond ((eq? c #\[) (keyScannerEscBracket))
          ((eq? c CHAR-ESC) (keyDispatch CHAR-ESC)
                            (keyScannerEsc))
          ; Timed out so accept escape char and start over
          ((not c) (keyDispatch CHAR-ESC))
          ; Not a recognized escape sequence, so send escape and the [ character
          (else (keyDispatch CHAR-ESC)
                (keyDispatch c)))))
 
 ; An "\e[" scanned
 (define (keyScannerEscBracket)
   (let ((c (read-char #f stdin)))
     (cond ((eq? c #\A) (keyDispatch 'up))
           ((eq? c #\B) (keyDispatch 'down))
           ((eq? c #\C) (keyDispatch 'right))
           ((eq? c #\D) (keyDispatch 'left))
           ((eq? c #\1) (keyScannerEscBracket1))
           ((eq? c #\2) (keyScannerEscBracket2))
           ((eq? c #\3) (keyScannerEscBracket3))
           ((eq? c #\4) (keyScannerEscBracket4))
           ((eq? c #\5) (keyScannerEscBracket5))
           ((eq? c #\6) (keyScannerEscBracket6))
           ((eq? c #\M) (keyScannerEscBracketM))
           ; Not an arrow key sequence, so send all the character to the key queue
           (else (keyDispatch CHAR-ESC)
                 (keyDispatch #\[)
                 (keyDispatch c)))))
 
 ; An "\e[1" has been scanned
 (define (keyScannerEscBracket1)
  (letrec ((c (read-char #f stdin)))
    (if (eq? c #\~)
      (keyDispatch 'home)
      (begin (keyDispatch CHAR-ESC)
             (keyDispatch #\[)
             (keyDispatch #\1)
             (keyDispatch c)))))

 ; An "\e[2" has been scanned
 (define (keyScannerEscBracket2)
  (letrec ((c (read-char #f stdin)))
    (if (eq? c #\~)
      (keyDispatch 'insert)
      (begin (keyDispatch CHAR-ESC)
             (keyDispatch #\[)
             (keyDispatch #\2)
             (keyDispatch c)))))


 ; An "\e[3" has been scanned
 (define (keyScannerEscBracket3)
  (letrec ((c (read-char #f stdin)))
    (if (eq? c #\~)
      (keyDispatch 'delete)
      (begin (keyDispatch CHAR-ESC)
             (keyDispatch #\[)
             (keyDispatch #\3)
             (keyDispatch c)))))

 ; An "\e[4" has been scanned
 (define (keyScannerEscBracket4)
  (letrec ((c (read-char #f stdin)))
    (if (eq? c #\~)
      (keyDispatch 'end)
      (begin (keyDispatch CHAR-ESC)
             (keyDispatch #\[)
             (keyDispatch #\4)
             (keyDispatch c)))))

 ; An "\e[5" has been scanned
 (define (keyScannerEscBracket5)
  (letrec ((c (read-char #f stdin)))
    (if (eq? c #\~)
      (keyDispatch 'pgup)
      (begin (keyDispatch CHAR-ESC)
             (keyDispatch #\[)
             (keyDispatch #\5)
             (keyDispatch c)))))

 ; An "\e[6" has been scanned
 (define (keyScannerEscBracket6)
  (letrec ((c (read-char #f stdin)))
    (if (eq? c #\~)
      (keyDispatch 'pgdown)
      (begin (keyDispatch CHAR-ESC)
             (keyDispatch #\[)
             (keyDispatch #\6)
             (keyDispatch c)))))

 ; An "\e[M" has been scanned
 (define (keyScannerEscBracketM)
  (letrec ((c (read-char #f stdin))
           (action (cond ((eq? c #\ ) 'mouse0)
                         ((eq? c #\!) 'mouse2)
                         ((eq? c #\") 'mouse1)
                         ((eq? c #\#) 'mouseup)
                         (else 'mouse)))
           (x (- (read-char #f stdin) #\  1))
           (y (- (read-char #f stdin) #\  1)))
   (mouseDispatch action y x)))
 
 (define (keyScannerAgentLoop)
  (let ((c (read-char #f stdin)))
    (if (eq? c CHAR-ESC)
      (keyScannerEsc) ; Escape char so attempt to read an escape sequence
      (keyDispatch c)) ; Add new keyboard character to queue
    (keyScannerAgentLoop))) ; rinse and repeat

 (define (getKeyCreate)
   (define keyQueue (QueueCreate)) ; Needs to be deleted
   (keyQueueStackRegister keyQueue)
   ; Return a blocking key reader.  If passed an integer, timeout.  Anything else, shutdown.
   (lambda restargs
     (if (null? restargs)
       (QueueGet keyQueue)
       (if (integer? (car restargs))
         (QueueGet keyQueue timeout) ; TODO add timeout capabilities to this or rethink semaphore implementations.
         (begin
           (keyQueueStackUnRegister keyQueue)
           (QueueDestroy keyQueue))))))

 ; The default keyboard read queue.  Continuously read stdin and append to
 ; a queue accessed via (getKey).  It is possible that another queue has
 ; been registered and receives stdin captured characters instead.

 (define getKey (getKeyCreate))
 ;;
 ;; Keyboard_and_mouse
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ; Initialize everything and return this object
 (thread (keyScannerAgentLoop))
 (display "\e[?1000h") ; Enable mouse reporting
 (ResetTerminal (terminal-size))
 self)
;;
;; Terminal of Windows, Keyboard and Mouse Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
