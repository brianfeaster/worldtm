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
;; Terminal of Windows Class
;;
(define (Terminal)

 (define self (lambda (msg) (eval msg)))

 (define TerminalSemaphore (open-semaphore 1))
 (define TerminalWindowSemaphore (open-semaphore 1))

 (define GCOLOR #f)

 (define GCURSOR-VISIBLE #t)

 (define Height (cdr (terminal-size)))
 (define Width  (car (terminal-size)))

 (define GY -1) ; Global cursor positions.
 (define GX -1)

 (define WINDOWS ()) ; List of window objects.

 (define (gset-color c)
   (if (!= GCOLOR c)
     (begin (set! GCOLOR c)
            (display (integer->colorstring c)))))

 (define (gtoggle-cursor)
   (display (if GCURSOR-VISIBLE "\e[?25l" "\e[?25h"))
   (set! GCURSOR-VISIBLE (not GCURSOR-VISIBLE)))

 (define (gputc c)
   (display c)
   (set! GX (+ 1 GX))
   (if (>= GX Width)
       (begin
         (set! GX 0)
         (set! GY (+ 1 GY))
         (if (>= GY Height)
             (set! GY (- Height 1))))))

 (define (ggoto y x)
   (if (or (!= y GY)
           (!= x GX))
     (begin (send "\e[" stdout)
            (display (+ y 1))
            (send ";" stdout)
            (display (+ x 1))
            (send "H" stdout)
            (set! GY y) (set! GX x)))
 )

 (define (TopmostWindow y x)
   (let ~ ((w WINDOWS))
     (if (null? w) ()
     (if (((car w) 'InsideWindow?) y x) (car w)
     (~ (cdr w))))))

 ; 2D cache of visible window objects.  () is base window which is never drawn
 ; on;
 (define WindowMask (make-vector-vector Height Width ()))

 ; Reset the terminal's window mask.
 (define (WindowMaskReset y0 x0 y1 x1)
  (semaphore-down TerminalWindowSemaphore)
  (let ~ ((y y0) (x x0))
    (if (< y y1) (if (= x x1) (~ (+ y 1) x0)
      (let ((topwin (TopmostWindow y x)))         ; Get top win at global pos
        (if (not (null? topwin)) (begin
          (vector-vector-set! WindowMask y x topwin); Cache it
          ((topwin 'globalRefresh) y x)
        ))        ; Redraw win's cell at global pos
        (~ y (+ x 1))))))
  (semaphore-up TerminalWindowSemaphore))

 (define (WindowMaskDump)
  (let ~ ((w WINDOWS))
   (or (null? w) (begin (display ((car w) 'ID))
                        (~ (cdr w)))))
  (sleep 2)
  (newline)
  (vector-map (lambda (v) (newline)(vector-map (lambda (v) (display (if (null? v) 0 (v 'ID)))) v))
              WindowMask)
  (sleep 2))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Window subclass.
 ;;
 (define (WindowNew Y0 X0 WHeight WWidth COLOR . switches)
   (define self (lambda (msg) (eval msg)))
   (define WindowSemaphore (open-semaphore 1))
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
   (define (goto y x)
     ;(display `(goto , y ,x))
     (semaphore-down WindowSemaphore)
     (set! needToScroll #f)
     (set! CurY y)
     (set! CurX x)
     (semaphore-up WindowSemaphore)
   )
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
     (semaphore-down WindowSemaphore)
     (set! CurX 0)
     (semaphore-up WindowSemaphore)
   )
   (define (newline)
     (set! CurY (+ 1 CurY))
     (if (>= CurY WHeight) (begin
           (set! CurY (- WHeight 1))
           (scrollUp)
           (set! needToScroll #f))))
   (define (backspace c)
     (if (and (< 0 CurX) (< CurX WWidth))
       (begin (semaphore-down WindowSemaphore)
              (set! CurX (- CurX 1))
              (set! needToScroll #f)
              (semaphore-up WindowSemaphore)
              (putc c)
              (display CHAR-CTRL-H) 
              (semaphore-down WindowSemaphore)
              (set! needToScroll #f)
              (set! CurX (- CurX 1))
              (semaphore-up WindowSemaphore)
       )))
   (define (scrollUp)
     ; Clear top-row which is to become the bottom row.
     ;(display 'scrollUp)
     (let ~ ((x 0))
        (if (< x WWidth)
          (let ((desc (vector-vector-ref DESC topRow x)))
            (vector-set! desc 0 COLOR)
            (vector-set! desc 1 #\ )
            (~ (+ x 1)))))
     (set! topRow (modulo (+ topRow 1) WHeight))
     ; Repaint window.
     ;(semaphore-down WindowSemaphore)
     (let ((originalY CurY)(originalX CurX))
       (repaint)
       (set! CurY originalY)
       (set! CurX originalX))
     ;(semaphore-up WindowSemaphore)
   )
   (define (repaint)
     ;(display `(repaint))
     ;(semaphore-down WindowSemaphore)
     (let ((originalColor COLOR)
           (originalY CurY)
           (originalX CurX))
       (home)
       (let ~ ((y 0) (x 0))
         (if (< y WHeight) (if (>= x WWidth) (~ (+ y 1) 0)
           (let((desc (vector-vector-ref DESC (modulo (+ y topRow) WHeight) x)))
             (if (eq? self (vector-vector-ref WindowMask (+ y Y0) (+ x X0)))
                 (begin
                   (ggoto (+ y Y0) (+ x X0))
                   (gset-color (vector-ref desc 0))
                   (gputc (vector-ref desc 1))))
             (~ y (+ x 1))))))
       (set! COLOR originalColor)
       (set! CurY originalY)
       (set! CurX originalX))
     ;(semaphore-up WindowSemaphore)
   )
   ; Repaint char given global coordinate.
   (define (globalRefresh gy gx)
     ;(display 'globalRefresh)
     ;(semaphore-down WindowSemaphore)
     (let ((y (- gy Y0))
           (x (- gx X0))
           (oy CurY)
           (ox CurX)
           (ocolor COLOR))
       ;(semaphore-up WindowSemaphore)
       (goto y x)
       (let ((desc (vector-vector-ref DESC (modulo (+ y topRow) WHeight) x)))
         (set-color (vector-ref desc 0))
         (putchar (vector-ref desc 1)))
       ;(semaphore-down WindowSemaphore)
       (set! CurY oy)
       (set! CurX ox)
       (set! COLOR ocolor)
       ;(semaphore-up WindowSemaphore)
      ))
   (define (putchar c)
     ;(display `(putchar ,c))
     (if needToScroll (begin (set! needToScroll #f) (return) (newline)))
     (if (!= GCURSOR-VISIBLE CURSOR-VISIBLE) (gtoggle-cursor))
     (let ((gy (+ CurY Y0))
           (gx (+ CurX X0)))
       (if (eq? c NEWLINE) (newline)
       (if (eq? c RETURN) (return)
       (if (eq? c CHAR-CTRL-G) (display c)
       (begin
         ; Display char to terminal.
         (if (eq? self (vector-vector-ref WindowMask gy gx))
           (begin
             (semaphore-down TerminalSemaphore)
             (ggoto gy gx)
             (gset-color COLOR)
             (gputc c)
             (semaphore-up TerminalSemaphore)))
         ; Cache color and char to buffer.
         ;(display `((,CurY , CurX) (,Y0 ,X0) (,WHeight ,WWidth)) stderr)
         ;(display "\n" stderr)
         (let ((desc (vector-vector-ref DESC (modulo (+ CurY topRow) WHeight)
                                             CurX)))
           (vector-set! desc 0 COLOR)
           (vector-set! desc 1 c))
         ; Advance cursor.
         ;(semaphore-down WindowSemaphore)
         (set! CurX (+ 1 CurX))
         ;(semaphore-up WindowSemaphore)
         (if (>= CurX WWidth)
           (if (= CurY (- WHeight 1))
               (set! needToScroll #t)
               (begin (return) (newline)))))))))
   )
   (define (putc c)
     (semaphore-down TerminalWindowSemaphore)
     (putchar c)
     (semaphore-up TerminalWindowSemaphore)
   )
   (define (puts str)
     (semaphore-down TerminalWindowSemaphore)
     (map putchar (string->list str))
     (semaphore-up TerminalWindowSemaphore)
   )
   (define (toggle)
     (set! ENABLED (not ENABLED))
     (WindowMaskReset Y0 X0 Y1 X1))
   (define (delete)
    (set! WINDOWS
      (let ~ ((l WINDOWS))
        (if (null? l) l
        (if (= (car WINDOWS) self) (cdr WINDOWS)
        (cons (car WINDOWS) (~ (cdr (WINDOWS))))))))
    (WindowMaskReset Y0 X0 Y1 X1))
   ; Create transparent 'pixel'.
   (define (alpha y x a)
     (vector-vector-set! ALPHA y x a)
     (WindowMaskReset (+ y Y0) (+ x X0) (+ y Y0 1) (+ x X0 1)))

   (set! WINDOWS (cons self WINDOWS))
   (WindowMaskReset Y0 X0 Y1 X1)
   self)
 ;;
 ;; Window subclass.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 self)
;;
;; Terminal of Windows Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(rem
(define term (Terminal))

;(define w3 ((term 'WindowNew) 0 0 29 80 #x07))

(define w1 ((term 'WindowNew) 4 18 10 20 #x2e))
(define w1put (w1 'putc))
(define w1puts (w1 'puts))

(define w2 ((term 'WindowNew) 5 19 10 14 #x1b))
(define w2put (w2 'putc))
(define w2puts (w2 'puts))

(w1puts 'window1)
(w2puts 'WINDOW2)
(sleep 1000)

(thread (let ~ () ((w1 'putc) #\1) (~)))
(thread (let ~ () ((w2 'toggle)) (~)))
(sleep 8000)
(quit)

(rem
((term 'WindowMaskDump))
(display (w1 'self)) (display (w1 'ID))(newline)
(display (w2 'self)) (display (w2 'ID))(newline)
(quit))

(thread (let ~ ((i 5))
  (w1put #\a) (sleep 500)
  (w1put #\b) (sleep 500)
  (w1put #\c) (sleep 500)
  (if (> i 0) (~ (- i 1)))))

(let ~ ((i 5))
 (w2puts "Long Live Donut       \r\n") 
 (w2puts " Long Live Donut1     \r\n")
 (w2puts "  Long Live Donut2    \r\n")
 (w2puts "   Long Live Donut3   \r\n")
 (w2puts "    Long Live Donut4  \r\n")
 (w2puts "     Long Live Donut5 \r\n")
 (w2puts "      Long Live Donut6\r\n")
 (if (> i 0) (~ (- i 1))))

)
