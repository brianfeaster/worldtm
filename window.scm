;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Concerns
; redrawing while scrolling up conflict with 'toprow' variable.

;;-----------------------------------------------------------------------------
;; Escape Sequence Stuff
;;
(define (integer->colorstring i)
 (vector-ref #("\e[0;40;30m" "\e[0;40;31m" "\e[0;40;32m" "\e[0;40;33m"
               "\e[0;40;34m" "\e[0;40;35m" "\e[0;40;36m" "\e[0;40;37m"
               "\e[1;40;30m" "\e[1;40;31m" "\e[1;40;32m" "\e[1;40;33m"
               "\e[1;40;34m" "\e[1;40;35m" "\e[1;40;36m" "\e[1;40;37m"

               "\e[0;41;30m" "\e[0;41;31m" "\e[0;41;32m" "\e[0;41;33m"
               "\e[0;41;34m" "\e[0;41;35m" "\e[0;41;36m" "\e[0;41;37m"
               "\e[1;41;30m" "\e[1;41;31m" "\e[1;41;32m" "\e[1;41;33m"
               "\e[1;41;34m" "\e[1;41;35m" "\e[1;41;36m" "\e[1;41;37m"

               "\e[0;42;30m" "\e[0;42;31m" "\e[0;42;32m" "\e[0;42;33m"
               "\e[0;42;34m" "\e[0;42;35m" "\e[0;42;36m" "\e[0;42;37m"
               "\e[1;42;30m" "\e[1;42;31m" "\e[1;42;32m" "\e[1;42;33m"
               "\e[1;42;34m" "\e[1;42;35m" "\e[1;42;36m" "\e[1;42;37m"

               "\e[0;43;30m" "\e[0;43;31m" "\e[0;43;32m" "\e[0;43;33m"
               "\e[0;43;34m" "\e[0;43;35m" "\e[0;43;36m" "\e[0;43;37m"
               "\e[1;43;30m" "\e[1;43;31m" "\e[1;43;32m" "\e[1;43;33m"
               "\e[1;43;34m" "\e[1;43;35m" "\e[1;43;36m" "\e[1;43;37m"

               "\e[0;44;30m" "\e[0;44;31m" "\e[0;44;32m" "\e[0;44;33m"
               "\e[0;44;34m" "\e[0;44;35m" "\e[0;44;36m" "\e[0;44;37m"
               "\e[1;44;30m" "\e[1;44;31m" "\e[1;44;32m" "\e[1;44;33m"
               "\e[1;44;34m" "\e[1;44;35m" "\e[1;44;36m" "\e[1;44;37m"

               "\e[0;45;30m" "\e[0;45;31m" "\e[0;45;32m" "\e[0;45;33m"
               "\e[0;45;34m" "\e[0;45;35m" "\e[0;45;36m" "\e[0;45;37m"
               "\e[1;45;30m" "\e[1;45;31m" "\e[1;45;32m" "\e[1;45;33m"
               "\e[1;45;34m" "\e[1;45;35m" "\e[1;45;36m" "\e[1;45;37m"

               "\e[0;46;30m" "\e[0;46;31m" "\e[0;46;32m" "\e[0;46;33m"
               "\e[0;46;34m" "\e[0;46;35m" "\e[0;46;36m" "\e[0;46;37m"
               "\e[1;46;30m" "\e[1;46;31m" "\e[1;46;32m" "\e[1;46;33m"
               "\e[1;46;34m" "\e[1;46;35m" "\e[1;46;36m" "\e[1;46;37m"

               "\e[0;47;30m" "\e[0;47;31m" "\e[0;47;32m" "\e[0;47;33m"
               "\e[0;47;34m" "\e[0;47;35m" "\e[0;47;36m" "\e[0;47;37m"
               "\e[1;47;30m" "\e[1;47;31m" "\e[1;47;32m" "\e[1;47;33m"
               "\e[1;47;34m" "\e[1;47;35m" "\e[1;47;36m" "\e[1;47;37m") i))
;;
;; Escape Sequence Stuff

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal of Windows Class v2
;;
(define (Terminal)
 (define self (lambda (msg) (eval msg)))
 (define TCOLOR #f)
 (define TCURSOR-VISIBLE #t)
 (define THeight 0)
 (define TWidth  0)
 (define GY -1) ; Global cursor positions.
 (define GX -1)
 (define WINDOWS ()) ; List of window objects.
 (define TerminalSemaphore (open-semaphore 1))
 (define WindowMask ())

 ; 2D cache of visible window objects.  () is base
 ; window which is never drawn on.  Considering having
 ; a default base window always in existence.
 (define (ResetTerminal)
  (set! THeight (cdr (terminal-size)))
  (set! TWidth  (car (terminal-size)))
  (set! WindowMask (make-vector-vector THeight TWidth ()))
  (WindowMaskReset 0 0 (- THeight 1) (- TWidth 1)))

 (define (InsideTerminal? y x)
   (and (>= y 0) (>= x 0) (< y THeight) (< x TWidth)))

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
   (display char)
   ; Update cursor.
   (set! GX (+ 1 GX))
   (if (<= TWidth GX)
       (begin
         (set! GX 0)
         (set! GY (+ 1 GY))
         ;(if (<= THeight GY) (set! GY (- THeight 1))) ; Allow the cursor to be off scree to force a cursor move.
       ))
   (semaphore-up TerminalSemaphore))

 (define (tcursor-visible)
   (display (if TCURSOR-VISIBLE "\e[?25l" "\e[?25h"))
   (set! TCURSOR-VISIBLE (not TCURSOR-VISIBLE)))

 (define (TopmostWindow gy gx)
 ; Return first window object in window list that is visible at this location.
   (let ~ ((w WINDOWS))
     (if (null? w) ()
     (if (((car w) 'InsideWindow?) gy gx) (car w)
     (~ (cdr w))))))

 ; Reset the terminal's window mask.
 (define (WindowMaskReset y0 x0 y1 x1)
  (let ~ ((gy y0) (gx x0))
    (if (<= gy y1) (if (> gx x1) (~ (+ gy 1) x0) (begin
      (if (InsideTerminal? gy gx) (begin
       (let ((topwin (TopmostWindow gy gx)))         ; Get top win at global pos
         (vector-vector-set! WindowMask gy gx topwin); Cache it
         (if (null? topwin)
             (gputc #\# #x08 gy gx)
             ((topwin 'globalRefresh) gy gx)))))     ; Redraw cell at global pos
      (~ gy (+ gx 1)))))))

 (define (WindowMaskDump)
  (let ~ ((w WINDOWS))
    (or (null? w) (begin (display ((car w) 'ID)) (newline)
                         (~ (cdr w)))))
  (vector-map
     (lambda (v)
        (vector-map (lambda (v) (display (if (null? v) 0 (v 'ID)))) v))
     WindowMask))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Window subclass.
 ;;
 (define (WindowNew Y0 X0 WHeight WWidth COLOR . switches)
   (define self (lambda (msg) (eval msg)))
   (define ID (+ 1 (length WINDOWS)))
   (define Y1 (+ Y0 WHeight))
   (define X1 (+ X0 WWidth))
   ; 2d vector of cell descriptors #(color char)
   (define DESC
     (vector-vector-map! (lambda (x) (vector COLOR #\ ))
                         (make-vector-vector WHeight WWidth ())))
   ; 2d alpha-channel table.
   (define ALPHA (make-vector-vector WHeight WWidth #t))
   (define CURSOR-VISIBLE #t)
   (define ENABLED #t)
   (define topRow 0) ; For horizontal scrolling.
   (define CurY 0)
   (define CurX 0)
   (define needToScroll #f) ; For bottom right character printing.
   (define (cursor-visible s) (set! CURSOR-VISIBLE s))
   (define WindowSemaphore (open-semaphore 1))
   (define (goto y x)
     (set! needToScroll #f)
     (set! CurY (min y (- WHeight 1)))
     (set! CurX (min x (- WWidth 1))))
   (define (set-color c) (set! COLOR c))
   (define (InsideWindow? gy gx)
     (and ENABLED
          (>= gy Y0)
          (>= gx X0)
          (<  gy Y1)
          (<  gx X1)
          (vector-vector-ref ALPHA (- gy Y0) (- gx X0))))
   (define (home) (goto 0 0))
   (define (return)
     (set! CurX 0))
   (define (newline)
     (set! CurY (+ 1 CurY))
     (if (>= CurY WHeight) (begin
           (set! CurY (- WHeight 1))
           (scrollUp)
           (set! needToScroll #f))))
   (define (backspace c)
     (if (and (< 0 CurX) (< CurX WWidth))
       (begin (set! CurX (- CurX 1))
              (set! needToScroll #f)
              (putc c)
              (display CHAR-CTRL-H) 
              (set! needToScroll #f)
              (set! CurX (- CurX 1)))))
   (define (scrollUp)
     ; Clear top-row which is to become the bottom row.
     (let ~ ((x 0))
        (if (< x WWidth)
          (let ((desc (vector-vector-ref DESC topRow x)))
            (vector-set! desc 0 COLOR)
            (vector-set! desc 1 #\ )
            (~ (+ x 1)))))
     (set! topRow (modulo (+ topRow 1) WHeight))
     ; Refresh window.
     (repaint))
   (define (repaint)
     (let ~ ((y 0) (x 0))
       (if (< y WHeight) (if (>= x WWidth) (~ (+ y 1) 0)
         (let ((desc (vector-vector-ref DESC (modulo (+ y topRow) WHeight) x)))
           (and (InsideTerminal? (+ y Y0) (+ x X0))
                (eq? self (vector-vector-ref WindowMask (+ y Y0) (+ x X0)))
                (gputc (vector-ref desc 1)
                       (vector-ref desc 0)
                       (+ y Y0) (+ x X0)))
           (~ y (+ x 1)))))))
   ; Repaint char given global coordinate.  Does not mutate window
   ; state.  Modulo the Y coordinate due to horizontal scrolling.
   (define (globalRefresh gy gx)
     (let ((desc (vector-vector-ref DESC
                   (modulo (+ (- gy Y0) topRow) WHeight)
                   (- gx X0))))
       (gputc (vector-ref desc 1) (vector-ref desc 0) gy gx)))
   (define (putchar c)
     (semaphore-down WindowSemaphore)
     (if needToScroll (begin (set! needToScroll #f) (return) (newline)))
     (if (!= TCURSOR-VISIBLE CURSOR-VISIBLE) (tcursor-visible))
     (if (eq? c NEWLINE) (newline)
     (if (eq? c RETURN) (return)
     (if (eq? c CHAR-CTRL-G) (display c)
     (let ((gy (+ CurY Y0))
           (gx (+ CurX X0)))
       (begin
         ; Send character to terminal only if window location is visible.
         (and (InsideTerminal? gy gx)
              (eq? self (vector-vector-ref WindowMask gy gx))
              (gputc c COLOR gy gx))
         ; Cache color and char to buffer.
         (let ((desc (vector-vector-ref DESC (modulo (+ CurY topRow) WHeight) CurX)))
           (vector-set! desc 0 COLOR)
           (vector-set! desc 1 c))
         ; Advance cursor.
         (set! CurX (+ 1 CurX))
         (if (>= CurX WWidth)
           (if (= CurY (- WHeight 1))
               (set! needToScroll #t) ; TODO remove this line then develop a framework to debug the issue.
               (begin 
                 (return)
                 (newline)))))))))
     (semaphore-up WindowSemaphore))
   (define putc putchar)
   (define (puts str)
     (for-each putchar (string->list str)))
   (define (toggle)
     (set! ENABLED (not ENABLED))
     (WindowMaskReset Y0 X0 Y1 X1))
   ; Create transparent 'pixel'.
   (define (alpha y x a)
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
       (set! Y1 (+ Y0 WHeight))
       (set! X1 (+ X0 WWidth))
       (if ENABLED (WindowMaskReset (min oY0 Y0) (min oX0 X0) (max oY1 Y1) (max oX1 X1))) ; Redraw window
       (semaphore-up WindowSemaphore)))
   (define (resize h w)
     (let ((oY1 Y1)
           (oX1 X1))
       (semaphore-down WindowSemaphore)
       (set! WHeight h)
       (set! WWidth w)
       (set! Y1 (+ Y0 WHeight))
       (set! X1 (+ X0 WWidth))
       (set! CurY (min CurY (- WHeight 1))) ; Make sure cursor is not out of bounds.
       (set! CurX (min CurX (- WWidth 1)))
       (set! ALPHA (make-vector-vector WHeight WWidth #t))
       (set! DESC (vector-vector-map! (lambda (x) (vector COLOR #\ ))
                                      (make-vector-vector WHeight WWidth ())))
       (if ENABLED (WindowMaskReset Y0 X0 (max oY1 Y1) (max oX1 X1))) ; Redraw window
       (semaphore-up WindowSemaphore)))
   (define (moveresize y x h w)
     (let ((oY0 Y0)
           (oX0 X0)
           (oY1 Y1)
           (oX1 X1))
       (semaphore-down WindowSemaphore)
       (set! Y0 y)
       (set! X0 x)
       (set! WHeight h)
       (set! WWidth w)
       (set! Y1 (+ Y0 WHeight))
       (set! X1 (+ X0 WWidth))
       (set! CurY (min CurY (- WHeight 1))) ; Make sure cursor is not out of bounds.
       (set! CurX (min CurX (- WWidth 1)))
       (set! ALPHA (make-vector-vector WHeight WWidth #t))
       (set! DESC (vector-vector-map! (lambda (x) (vector COLOR #\ ))
                                      (make-vector-vector WHeight WWidth ())))
       (if ENABLED (WindowMaskReset
                      (min oY0 Y0) (min oX0 X0) ; Redraw window
                      (max oY1 Y1) (max oX1 X1)))
       (semaphore-up WindowSemaphore)))
   (define (delete)
    (set! WINDOWS
      (let ~ ((l WINDOWS))
        (if (null? l) ()
        (if (eq? (car l) self) (cdr l)
        (cons (car l) (~ (cdr l)))))))
    (close-semaphore WindowSemaphore)
    (WindowMaskReset Y0 X0 Y1 X1))

   (set! WINDOWS (cons self WINDOWS))
   (WindowMaskReset Y0 X0 Y1 X1)
   self)
 ;;
 ;; Window subclass.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (ResetTerminal)
 self)
;;
;; Terminal of Windows Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
