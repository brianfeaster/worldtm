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
(define colorTable #f)

(define (ColorTable256)
 (set! colorTable
  (let ((tbl (make-vector 65536)))
    (loop 65536 (lambda (i)
        (vector-set! tbl i (string "\e[48;5;" (number->string (/ i 256))
                                   ";38;5;" (number->string (modulo i 256)) "m"))))
    tbl))
 256)

; 16 color version of the 256 color table
(define (ColorTable8)
 (set! colorTable
  (let ((tbl (make-vector 65536))
        (cnv #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0 4 4 4 12 12 2 6 6 6 6 12 2 6 6 6 6 12 2 2 6 6 6 6 2 6 6 6 14 14 10 10 14 14 14 14 1 5 5 5 5 12 3 8 6 12 12 12 3 2 6 12 12 12 3 2 6 6 6 6 3 10 10 14 14 14 10 10 10 14 14 14 1 5 5 5 5 13 3 3 5 12 12 12 3 2 8 12 12 12 3 2 2 6 6 6 3 10 10 14 14 14 10 10 10 14 14 14 1 5 5 5 13 13 3 9 5 5 13 13 3 3 9 5 13 13 3 3 3 7 12 12 11 10 10 14 14 14 10 10 10 14 14 14 9 5 5 13 13 13 3 9 9 13 13 13 3 3 9 13 13 13 3 3 3 13 13 12 11 11 11 10 7 15 11 11 10 15 15 15 9 9 13 13 13 13 9 9 9 13 13 13 3 9 9 13 13 13 3 3 3 9 13 13 11 11 11 11 15 15 11 11 11 15 15 15 0 0 8 8 8 8 8 8 8 8 8 8 7 7 7 7 7 7 7 7 7 7 15 15)))
    (loop 65536 (lambda (i)
      (let ((b (vector-ref cnv (/ i 256)))
            (f (vector-ref cnv (modulo i 256))))
        (vector-set! tbl i (string "\e[" (if (< 7 f) "0;1;3" "0;3") (number->string (modulo f 8))
                                      (if (< 7 b) ";5;4" ";4") (number->string (modulo b 8)) "m")))))
    tbl))
 8)

(define (ColorTable i)
 (if (= i 256) (ColorTable256))
 (if (= i 8) (ColorTable8)))

(ColorTable 256)


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
 (define ENABLE #t)
 (define Theight 0)
 (define Twidth  0)
 (define TY -1) ; Global cursor positions.
 (define TX -1)
 (define PASTLASTCOLUMN #f) ; Last character was on the last column.  Cursor is technically in two places now.
 (define WindowVec (make-vector MAXWINDOWCOUNT #f)) ; Vector of window objects by ID
 (define WindowList ()) ; List of window objects in display order
 (define TerminalSemaphore (open-semaphore 1))
 (define PublicSemaphore (open-semaphore 1))
 ; 2D table of visible window objects.  #f represents no window which is never drawn on.  Considering
 ; having a default base window always in existence which could act as a static/dynamic background image.
 (define WindowMask ())

 ; Disable Terminal updates to the remote console.  Allows for hidden screen refresh/redraws.
 (define (TerminalEnable)
   (set! ENABLE #t)
   (RefreshTerminal))

 (define (TerminalDisable)
   (set! ENABLE #f))

 (define (InsideTerminal? ty tx)
   (and (>= ty 0) (>= tx 0) (< ty Theight) (< tx Twidth)))

 (define (topWin ty tx)
   (and (InsideTerminal? ty tx)
        (vector-vector-ref WindowMask ty tx)))

 ; Return first window object in window list that is visible at this location.
 (define (TopmostWindowDiscover gy gx)
   (let ~ ((w WindowList))
     (if (null? w) #f ; No windows are visible at this location
     (if (((car w) 'InsideWindow?) gy gx) (car w) ; A window is visible at this location
     (~ (cdr w))))))

 ; Reset the terminal window mask and adjust the window's visible count
 ; The window mask is the terminal size.
 (define (WindowMaskReset y0 x0 y1 x1)
   (loop2 y0 y1 x0 x1 (lambda (gy gx) ; Over every terminal char position in the specified box
   (if (InsideTerminal? gy gx)
     (let ((prevwin (topWin gy gx)) ; Consider the cached and actual top win at this loc
           (topwin (TopmostWindowDiscover gy gx)))
       ; Update the visible count for the window(s) at this global location.
       ; A different window implies we need to decreement the last window's count (if any)
       ; and inc the new window's count (if any).
       (or (eq? prevwin topwin)
         (begin
           (vector-vector-set! WindowMask gy gx topwin) ; Update the window reference at this position
           (if prevwin ((prevwin 'visibleCountAdd) -1))
           (if topwin  ((topwin 'visibleCountAdd) 1))))
       ; Redraw the glyph (or background)
       (if topwin
         ((topwin 'globalRefresh) gy gx)
         (drawBackgroundCell gy gx)))))))

 (define (ResetTerminal termSize)
  (set! Theight (cdr termSize)) (if (< Theight 2) (set! Theight 2))
  (set! Twidth  (car termSize)) (if (< Twidth 2) (set! Twidth 2))
  (set! WindowMask (make-vector-vector Theight Twidth #f))
  ; Clear each window's visible count to coincide with the uninitialized WindowMask array
  ; then reset each window's visibile count and terminal window mask.
  (map (lambda (w) ((w 'visibleCountClear))) WindowList)
  (WindowMaskReset 0 0 Theight Twidth))

 (define (RefreshTerminal)
   ;(loop2 0 Theight 0 Twidth (lambda (ty tx) (gputc #\. #x0001 ty tx))) ; Clear every terminal char position
   (loop2 0 Theight 0 Twidth (lambda (ty tx) ; Over every terminal char position
     (let ((win (topWin ty tx)))
       (if win
         ((win 'globalRefresh) ty tx)   ; Ask window to draw one of its characters to the terminal
         (drawBackgroundCell ty tx)))))); No window at this location so draw the background

 (define (tgoto y x)
   (if (and (or (!= y TY) (!= x TX))
            (InsideTerminal? y x))
     (begin
       (semaphore-down TerminalSemaphore)
       (set! PASTLASTCOLUMN #f)
       (send "\e[" stdout)
       (display (+ y 1))
       (send ";" stdout)
       (display (+ x 1))
       (send "H" stdout)
       (set! TY y)
       (set! TX x)
       (semaphore-up TerminalSemaphore))))

 ; Match the behavior of sane terminals internally.  Sending a printable char
 ; advances the cursor including non-glyph chars which are represented by a
 ; generic the generic glyph '.'.  The cursor is expected not to advance past
 ; the last column until another char is displayed which occurs on the next row
 ; and first column.
 (define (gputc char color y x)
  (and ENABLE (begin
    (semaphore-down TerminalSemaphore)
    ; Set color.
    (if (!= TCOLOR color) (begin
      (set! TCOLOR color)
      (send (integer->colorstring color) stdout)))
    (if PASTLASTCOLUMN
      ; Adjust virtual terminal cursor to next line if last char was displayed on the last column.
      ; If already on the last line, send a return to prevent the screen from scrolling.
      (begin
        (set! PASTLASTCOLUMN #f)
        (set! TX 0)
        (if (< TY (- Theight 1))
            (set! TY (+ TY 1))
            (send "\r" stdout))))
    ; Set cursor location.
    (if (or (!= y TY) (!= x TX)) (begin
      (send "\e[" stdout)
      (display (+ y 1))
      (send ";" stdout)
      (display (+ x 1))
      (send "H" stdout)
      (set! TY y) (set! TX x)))
    ; Draw char as something (even non-glyphed chars).
    (display (char->visible char))
    ; Advance cursor.  The cursor's visible range is [0 .. Twidth - 1]
    ; A character was sent to the last column, keep TX on the
    ; last column but set end of physical line EOPL flag.
    (if (< TX (- Twidth 1))
      (set! TX (+ 1 TX))
      (set! PASTLASTCOLUMN #t))
    (semaphore-up TerminalSemaphore))))

 ; Given a global terminal coordinate, plot a background character.
 (define drawBackgroundCell (let ((i 0)) (lambda (ty tx)
   (gputc (vector-ref #(#\[ #\t #\m #\] #\  ) i) ; The char
          #xeb16                            ; The 8bit background and 8bit foreground color
          ty tx)                            ; physical terminal y and x location
   (if (< i 4) (set! i (+ i 1)))
   (if (and (= i 4) (= 0 (random 20))) (set! i 0)))))

 (define (tcursor-visible)
   (semaphore-down TerminalSemaphore)
   (display (if TCURSOR-VISIBLE "\e[?25l" "\e[?25h"))
   (set! TCURSOR-VISIBLE (not TCURSOR-VISIBLE))
   (semaphore-up TerminalSemaphore))

 (define (lock) (semaphore-down PublicSemaphore))
 (define (unlock) (semaphore-up PublicSemaphore))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Window_subclass
 ;;
 (define WindowNew (let ((parent self)) (lambda (Y0 X0 Wheight Wwidth COLOR . ChildStack)
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
   (define WY 0) ; Cursor position relative to window
   (define WX 0)
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
   (define topRow 0) ; For vertical scrolling.  The logical "top row" of the DESC array
   (define PASTLASTCOLUMN #f) ; For last column cursor positioning
   (define (cursor-visible s) (set! CURSOR-VISIBLE s))
   (define WindowSemaphore (open-semaphore 1))
   (define ScrollbackHack #f)
   (define VisibleCount 0) ; Number of visible non-obstructed locations
	; Methods
   (define (getColor y x) (vector-ref (vector-vector-ref DESC y x) 0))
   (define (getChar  y x) (vector-ref (vector-vector-ref DESC y x) 1))
   (define (visibleCountClear) (set! VisibleCount 0))
   (define (visibleCountAdd c) (set! VisibleCount (+ VisibleCount c)))
   (define (goto y x)
     (set! PASTLASTCOLUMN #f)
     (set! WY (min (max 0 y) (- Wheight 1)))
     (set! WX (min (max 0 x) (- Wwidth 1)))
     (tgoto (+ Y0 WY) (+ X0 WX)))
   (define (set-color b f)
     (set! COLOR (+ (* 256 b) f))) ; 256 colors each fg and bg packed into 16 bits
   (define (set-color-word c)
     (set! COLOR c))
   (define (InsideWindow? gy gx)
     (and ENABLED
          (>= gy Y0)
          (>= gx X0)
          (<  gy Y1)
          (<  gx X1)
          (vector-vector-ref ALPHA (- gy Y0) (- gx X0))))
   (define (hardwareScrollable?)
     (and (= Wwidth Twidth)
          (< 1 Wheight)
          (= VisibleCount (* Wwidth Wheight))))
   ; Assumtions: Window cursor X is back to column 0
   (define (scrollUp)
     (if (hardwareScrollable?)
       (begin
         (semaphore-down TerminalSemaphore)
         ; Manually update the terminal color
         (if (!= COLOR TCOLOR)
           (send (integer->colorstring COLOR) stdout))
         ; Save cursor location, set scrolling region to entire window, move
         ; cursor to bottom of region, newline (causing region to scroll).
         (displayl "\e7\e[" (+ Y0 1) ";" (+ Y0 Wheight) "r\e[" (+ Y0 Wheight) "H\n")
         ; Reset the new last row in the window descriptor array and
         ; manually clear the new line.
         (loop Wwidth (lambda (x) ; Clear row which will become the bottom row
           (let ((desc (vector-vector-ref DESC topRow x)))
             (vector-set! desc 0 COLOR)
             (vector-set! desc 1 #\ )
             (send #\  stdout))))
         ; Restore scrolling region, restore cursor.
         (display "\e[r\e8")
         (if (!= COLOR TCOLOR) ; Manually sync terminal color
           (set! TCOLOR COLOR))
         (set! topRow (modulo (+ topRow 1) Wheight)) ; Shift the buffer's top-row index.
         (semaphore-up TerminalSemaphore)
         (return)) ; Send the cursor to the beginning of bottom row.
       (begin
         ; Clear top-row which is to become the bottom row.
         ; Force topmost line off the top of the terminal in hopes
         ; of filling the client's terminal backscroll buffer. Set
         ; 2 line scrolling region, move cursor, clear line.
         (if ScrollbackHack (begin
           (display "\e7\e[1;2r\e[H")
           (let ~ ((x 0))
             (if (< x Wwidth)
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
         (repaint)
         (return))))
   (define (repaintRow row)
     (loop Wwidth (lambda (x)
       (let ((desc (vector-vector-ref DESC (modulo (+ row topRow) Wheight) x)))
         (and (InsideTerminal? (+ row Y0) (+ x X0))
              (eq? self (topWin (+ row Y0) (+ x X0)))
              (gputc #\- ;(vector-ref desc 1)
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
   (define (return)
     (set! PASTLASTCOLUMN #f)
     (set! WX 0)
     (tgoto (+ Y0 WY) (+ X0 WX)))
   (define (newline)
     (set! PASTLASTCOLUMN #f)
     (if (< WY (- Wheight 1))
         (set! WY (+ WY 1))
         (scrollUp))
     (tgoto (+ Y0 WY) (+ X0 WX)))
   (define (home)
     (goto 0 0))
   (define (_back)
     ; If just displayed last char in row, do nothing.
     (if (and (< 0 WX) (< WX Wwidth))
       (begin
         (or PASTLASTCOLUMN
           (set! WX (- WX 1)))
         (set! PASTLASTCOLUMN #f))))
   (define (back)
     (semaphore-down WindowSemaphore)
     (_back)
     (goto WY WX)
     (semaphore-up WindowSemaphore))
   (define (backspace c) ; backspacing with a non glyph is dangerous
     (semaphore-down WindowSemaphore)
     (if (and (< 0 WX) (< WX Wwidth))
       (begin
         (_back)
         (putchar c)
         (_back)
         (goto WY WX)))
     (semaphore-up WindowSemaphore))
   (define (putchar c)
     (if (not (eq? TCURSOR-VISIBLE CURSOR-VISIBLE)) (tcursor-visible)) ; Sync window and terminal's cursor visibility
     (if (eq? c NEWLINE) (newline) ; This should actually move the cursor
      (if (eq? c RETURN) (return) ; This should actually move the cursor
       (if (eq? c CHAR-CTRL-G) (display c) ; Bell doesn't physically print anything so no state to adjust.
        (begin
          ; If the last char printed was on the last column, we
          ; need to set window cursor to the right spot first.
          (if PASTLASTCOLUMN
            (begin
              (set! PASTLASTCOLUMN #f)
              (set! WX 0) ; back to columm 0
              (if (< WY (- Wheight 1)) ; Either cursor down a row or scroll window up
                (set! WY (+ 1 WY))
                (scrollUp))))
          (let ((ty (+ WY Y0)) ; Consider terminal coordinate location for next character
                (tx (+ WX X0)))
            ; Send character only if the this location is in the window and visible
            (if (eq? self (topWin ty tx))
              (gputc c COLOR ty tx))
            ; Cache color and char to buffer.  The modulo is required to compute
            ; the actual row since it depends on the topRow value which increments
            ; whenever the window scrolls up.
            (let ((desc (vector-vector-ref DESC
                                           (modulo (+ WY topRow) Wheight)
                                           WX)))
              (vector-set! desc 0 COLOR)
              (vector-set! desc 1 c))
            ; Advance cursor or leave it on the last column.
            (if (< WX (- Wwidth 1))
              (set! WX (+ 1 WX)) ; The cursor hangs out on the last column until the next char
              (set! PASTLASTCOLUMN #t))))))))
   ; puts is autonomous to guarantee the string output is not altered.  putc must be autonomous as well
   ; for its homgeneity.
   (define (puts str)
     (semaphore-down WindowSemaphore)
     (loop (string-length str) (lambda (i) (putchar (string-ref str i))))
     (semaphore-up WindowSemaphore))
   (define (putc c)
     (semaphore-down WindowSemaphore)
     (putchar c)
     (semaphore-up WindowSemaphore))
   ; Clears rest of line.  Output will resume at original column.
   (define (clearToEnd)
     (semaphore-down WindowSemaphore)
     (let ((ox WX) (oy WY))
       (let ~ ((i WX))
         (if (< i Wwidth)
           (begin
             (putchar #\ )
             (~ (+ i 1)))))
       (set! WX ox)
       (set! WY oy))
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
           (oX1 X1)
           (oldDesc DESC))
       (semaphore-down WindowSemaphore)
       (set! Wheight (if (< h 1) 1 h)) ; Silently reset invalid height
       (set! Wwidth  (if (< w 1) 1 w)) ; Silently reset invalid width
       (set! Y1 (+ Y0 Wheight))
       (set! X1 (+ X0 Wwidth))
       (set! WY (min WY (- Wheight 1))) ; Make sure cursor is not out of bounds.
       (set! WX (min WX (- Wwidth 1)))
       ; Reset alpha mask (no cell is transparent)
       (set! ALPHA (make-vector-vector Wheight Wwidth #t))
       ; Reset descriptor table (each cell is a space with current color set)
       (set! DESC
         (vector-vector-map! (lambda (x) (vector COLOR #\ ))
                               (make-vector-vector Wheight Wwidth ())))
       ;(vector-vector-set-vector-vector! DESC 0 0 oldDesc 0 0)  ; For this to be useful, the call to WindowMaskReset should be smarter and not redraw the entire window (plus the new area)
       ; Recompute the window mask for the terminal.  Redraw the window it it's enabled.
       (if ENABLED
         (WindowMaskReset Y0 X0 (max oY1 Y1) (max oX1 X1)))
       (set! topRow 0)
       (semaphore-up WindowSemaphore)))
   (define (moveresize y x h w)
     (let ((oY0 Y0)
           (oX0 X0)
           (oY1 Y1)
           (oX1 X1))
       (semaphore-down WindowSemaphore)
       ; Mutate the window's upper left (Y0 X0) and lower right (Y1 X1) coordinates
       (set! Y0 y)
       (set! X0 x)
       (set! Wheight (if (< h 1) 1 h)) ; Silently reset invalid height and width (A window must have area of at least cell)
       (set! Wwidth  (if (< w 1) 1 w))
       (set! Y1 (+ Y0 Wheight))
       (set! X1 (+ X0 Wwidth))
       ; Make sure cursor is not out of bounds
       (set! WY (min WY (- Wheight 1)))
       (set! WX (min WX (- Wwidth 1)))
       (set! ALPHA (make-vector-vector Wheight Wwidth #t)) ; Reset the alpha mask (TODO?)
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
   ;; Buffer_subclass -- Implements a
   ;; cooked text window with scrollback buffer
   ;;
   ;; The Buffer structure is a forever growing stack: (... LINE3 LINE2 LINE0)
   ;;
   ;; Printing RED "abc" GREEN "def\n" BLUE "123" PURPLE "456\n" results in:
   ;;
   ;; The Buffer structure: ((PURPLE "456" BLUE "123")  ; second line
   ;;                        (GREEN  "def" RED  "abc")) ; first line
   ;;
   ;; Note purple 456 is the current line color and string and could have more characters appended
   ;;
   ;; A newline or return characters creates a new line entry in the buffer of the form (COLOR "")
   ;; who's color could be changed.
   ;;
   (define Buffer (let ((parent self)) (lambda ()
     (define (self msg) (eval msg))
     (define Buffer (list (list COLOR "Welcome to Zombocom"))) ; List of every line structures.  Initially contains an empty first line.
     (define LineCount 0)
     (define backscroll 0) ; Scrollback offset index.  0 = end of buffer. > 0 is number of lines back to view.
     (define LastCh #f) ; Keep track of last character parsed/displayed
     ; Parse a string for newlines and add to the buffer's list of
     ; line structures which is a reverse list of consecutive color and string values (COLOR STR ...)
     (define (puts str)
       (define line (car Buffer)) ; Current buffer line (might be empty if just created or newline sent)
       (let ~ ((str str) ; The current string, length and parse index
               (len (string-length str))
               (i 0)
               (last LastCh))
        (cond ((= len 0) ; Empty string.  Done.
               (set! LastCh last)
               'done)
              ((= i len) ; Parsed entire string.  Update Buffer structure.  Done.
               (cond ((= (car line) COLOR) ; Current line color is same as window's color
                      (set-car! (cdr line) (string (cadr line) str))) ; Just append parsed string to current string
                     ((eq? (cadr line) "") ; current line string segment is empty
                      (set-car! line COLOR) ; Replace current line color with current window color
                      (set-car! (cdr line) str)) ; Replace curent empty line string with the new parsed string
                     (else
                      (set-car! Buffer (cons COLOR (cons str line))))) ; Add new color/str to current line
               (if (= 0 backscroll) ((parent 'puts) str))
               (set! LastCh last)) ; Send parsed string to window
              (else
                (let ((ch (string-ref str i)))
                  (if (pair? (memv ch (list NEWLINE RETURN)))
                    (if (and (eq? ch NEWLINE) (eq? last RETURN))
                        (~ (substring str 1 len) (- len 1) 0 ch) ; Recurse.  Ignore newline that follows a return
                        (begin
                          (~ (substring str 0 i) i i ch) ; Recurse.  Use this recursive call to display and save color/string to line
                          (return) ; as well as a cooked newline
                          (newline)
                          (set! LineCount (+ 1 LineCount)) ; Add new strb to strb bufer
                          (set! line (list COLOR "")) ; Create new empty line
                          (set! Buffer (cons line Buffer)) ; Start a new line
                          (if (< 1 len) (~ (substring str (+ i 1) len) (- len i 1) 0 ch)))) ; Recurse
                    (~ str len (+ i 1) ch))))))) ; Recurse
     ; Return glyph length of the line of the form (COLOR STR ...)
     (define (line-length line)
       (if (null? line) 0
         (+ (string-length (cadr line)) ; Every 2nd element is a string
            (line-length (cddr line)))))
     ; Returns number of physical lines this line prints on the current window
     (define (line-line-count line)
       (define len (line-length line))
       (if (= len 0)
           1
           (/ (+ len Wwidth -1) Wwidth))) ; number of full lines
     ; Draws a line (CLR STR ...) to the window skipping 'skip' lines and
     ; displaying up to 'left' lines  The list is backwards, thus the
     ; pre-recursive algorithm.  Returns number of chars not printed to on
     ; the last line.
     (define (redrawLine LINE skip left)
       (set! skip (* skip Wwidth)) ; Consider number of chars to skip before displaying logical line
       (set! left  (* left Wwidth)); Total number of chars on screen we can display to.
       (let ~ ((line LINE)) ; List of (CLR STR ...)
         (or (null? line)
         (begin
           (~ (cddr line)) ; pre-recurse
           (if (< 0 left) (let ((str (cadr line))
                                (len (string-length (cadr line))))
             (if (<= len skip)
                 (set! skip (- skip len)) ; Need to skip more characters
                 (begin
                   (set! COLOR (car line))
                   ; consider the full string or sub string
                   (letrec ((substr (substring str skip (string-length str)))
                            (len (string-length substr)))
                     (set! skip 0) ; nothing more to skip now
                     (if (<= left len)
                       (begin
                         ((parent 'puts) (substring substr 0 left))
                         (set! left 0))
                       (begin
                          ((parent 'puts) substr)
                          (set! left (- left len))))))))))))
       (modulo left Wwidth)) ; Return number of chars not printed to on this physical line.
     ; Redraw the current state of the scrollback buffered window.
     ; The 'backscroll' var is the physical row count to scroll back,
     ; with respect to the bottom most printable line.
     (define (redrawBuffer)
       (define skip 0) ; State used when the recursion unwinds
       (define physicalLeft Wheight)
       (define oy 0)
       (define ox 0)
       (home)
       (let ~ ((physical (+ backscroll Wheight)) ; The number of physical lines we need to skip until redrawing occurs.
               (lst Buffer)) ; The stack buffer of lines
         (cond ((< physical 1);Base case 2: The current line exceeds beyond the top of the screen, we only need to skip (abs physical) number of lines
                 (set! skip (- physical)))
               ((null? lst)
                 ()) ; Base case 1: We've hit the "top" of the buffer.  So return and start the dumping of the lines we've traversed recursively.
               (else
                 (letrec ((line (car lst)) ; Consider the current line
                          (llcount (line-line-count line))) ; and the number of physical lines it requires to display
                 (~ (- physical llcount) (cdr lst)); Pre-recurse
                 ; Print this line, assuming skip lines and physicalLeft lines to go
                 (if (< 0 physicalLeft)
                   (let ((charsLeft (redrawLine line skip physicalLeft))) ; Render the line to multiple physical lines
                     (if (< 0 charsLeft) ((parent 'clearToEnd)))
                     (set! physicalLeft (- physicalLeft (- llcount skip))) ; subtract number of lines displayed (total line count - skipped lines)
                     (set! skip 0)
                     (if (eq? lst Buffer)
                       (begin ; Make sure the last line printed is the next cursor position.
                         (set! oy WY)
                         (set! ox WX)
                         (let ~ ((i physicalLeft)) ; Clear rest of window if there aren't enough lines to fill the screen
                           (if (< 0 i)
                             (begin
                               (return)
                               (newline)
                               ((parent 'clearToEnd))
                               (~ (- i 1)))))
                         (goto oy ox))
                       (if (and (< 0 charsLeft) (< 0 physicalLeft)) ; If there are more physical lines and not last line to print, clear and carriage return.
                         (begin
                           (return)
                           (newline)))))))))))
     (define (scrollHome)
       (set! backscroll (- LineCount Wheight))
       (redrawBuffer))
     (define (scrollEnd)
       (set! backscroll 0)
       (redrawBuffer))
     (define (scrollBack)
       (set! backscroll (+ 1 backscroll))
       (redrawBuffer))
     (define (scrollForward)
       (set! backscroll (- backscroll 1))
       (redrawBuffer))
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
    #f))))
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
   (letrec ((win (topWin y x))
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
     (cond ((null? restargs) (QueueGet keyQueue))
           ((integer? (car restargs)) (QueueGet keyQueue timeout)) ; TODO add this timeout capability or rethink semaphore implementations.
           (else (keyQueueStackUnRegister keyQueue)
                 (QueueDestroy keyQueue)))))

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
