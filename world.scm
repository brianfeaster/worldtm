(define move list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window stuff
(load "window.scm")
(define Terminal (Terminal))

(define Console ((Terminal 'WindowNew)
 0 0
 (- (Terminal 'THeight) 1) (Terminal 'TWidth)
 #x0f))
(define ConsolePutc (Console 'putc))
(define ConsolePuts (Console 'puts))
(define ConsoleSetColor (Console 'set-color))
(define (ConsoleDisplay . l)
  (map (lambda (o) (ConsolePuts (display->string o))) l))
(define (ConsoleWrite o)
  ((Console 'puts) (write->string o)))

((Console 'goto) 0 0) ;(Console 'WHeight)
;(ConsoleDisplay  "\r\nWelcome to World\r\n")
;(ConsoleDisplay  "\r\nEnter your name>\r\n")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC stuff
(load "ipc.scm")

; Create ipc object with an debug message output port.
(define ipc (Ipc ConsoleDisplay))

; Always read and evaluate everything from IPC.
(thread  (let ~ () (eval ((ipc 'qread))) (~)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glyphs - Two characters including their color.
;;
(define glyphNew vector)
(define (glyphColor0 cell) (vector-ref cell 0))
(define (glyphChar0  cell) (vector-ref cell 1))
(define (glyphColor1 cell) (vector-ref cell 2))
(define (glyphChar1  cell) (vector-ref cell 3))

(define (colorForeground color) (modulo color 16))
(define (colorBackground color) (/ color 16))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cells - For now cells are just glyphs...eventually smarter objects.
;;
(define CELLS (make-vector 128 ))
(define (cell-ref i)
  (if (> i 65536)
    ((entitiesGet i) 'glyph)
    (vector-ref CELLS i)))
(define (cell-set! i c) (vector-set! CELLS i c))

(define AIR 0)        (cell-set! AIR        (glyphNew #x00 CHAR-CTRL-@ #x00 CHAR-CTRL-@))
(define DIRT  1)      (cell-set! DIRT       (glyphNew #x03 #\. #x03 #\.))
(define GRASS 2)      (cell-set! GRASS      (glyphNew #x02 #\, #x02 #\,))
(define XX 3)         (cell-set! XX         (glyphNew #x0f #\X #x0f #\X))
(define BRICK 4)      (cell-set! BRICK      (glyphNew #x19 #\[ #x19 #\]))
(define STONE 5)      (cell-set! STONE      (glyphNew #x07 #\[ #x07 #\]))
(define DOORCLOSED 6) (cell-set! DOORCLOSED (glyphNew #x09 #\- #x09 #\-))
(define DOOROPEN 7)   (cell-set! DOOROPEN   (glyphNew #x09 #\| #x09 #\ ))
(define SIGN 8)       (cell-set! SIGN       (glyphNew #x4b #\| #x0b #\)))
(define AVATAR 9)     (cell-set! AVATAR     (glyphNew #x0f #\/ #x0f #\\))
(define HELP 10)      (cell-set! HELP       (glyphNew #x21 #\? #x21 #\?))
(define CONSTRUCT 11) (cell-set! CONSTRUCT  (glyphNew #x77 #\  #x77 #\ ))
(define TV   12)      (cell-set! TV         (glyphNew #x30 #\[ #x30 #\]))
(define CHAIR 13)     (cell-set! CHAIR      (glyphNew #x19 #\P #x19 #\o))
(define SNAKE 14)     (cell-set! SNAKE      (glyphNew #x6b #\O #x6b #\o))
(define KITTY 15)     (cell-set! KITTY      (glyphNew #x07 #\= #x07 #\^))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Column - Somewhat compact group of objects in one dimension.  Compressed
;;  by including all values within a range with the objects below and above
;;  the range the first and last values in the vector.
;;
;; IE: Position -3 -1 0 1 2|3 4 5 6 7|8...
;;                         |         |
;;     Objects   0  0 0 0 0|0 1 2 2 5|5 5...
;;       default           |  real   |        default
;;    bottom objects_______| objects |_____ top objects
;;
;;  Is stored as #(2  0 1 2 2 5) where the first value in the vector is the
;;  position of the first default object.  The lower and upper default object
;;  are stored in the 2nd and last position.
;;
; Height of first default bottom object.
(define (columnHeightBottom c)
  (vector-ref c 0))

; Height of first default top object.
(define (columnHeightTop c)
  (+ (columnHeightBottom c) (vector-length c)))

(define (columnRef c h)
  (let ((i (- h (columnHeightBottom c))))
    (if (<= i 0)
      (vector-ref c 1)
    (if (>= i (vector-length c))
      (vector-ref c (- (vector-length c) 1))
    (vector-ref c i)))))

; Extend column below based on existing column. #(3  4 5 6) -> #(1  4 4 4 5 6)
(define (columnExtendBelow c bot)
 (let ((k (make-vector (- (columnHeightTop c) bot)             ; vector size
                       (columnRef c (columnHeightBottom c))))) ; default object
   ; Set bottom height
   (vector-set! k 0 bot)
   ; Copy old column here.
   (vector-set-vector! k (- (columnHeightBottom c) bot -1) c 1)
   k))

; Extend column above based on existing column. #(3  4 5 6) -> #(3  4 5 6 6 6)
(define (columnExtendAbove c top)
 (let ((k (make-vector (- top (columnHeightBottom c))       ; vector size
                       (columnRef c (columnHeightTop c))))) ; default object
   ; Copy old column here including bottom height
   (vector-set-vector! k 0 c 0)
   k))

; Returns column (or new column) with object set at specified height.
(define (columnSet c h o)
  (let ((i (- h (columnHeightBottom c)))) ; Column height -> vector index
    (if (<= i 1)
      (begin
        (set! c (columnExtendBelow c (- h 2)))
        (vector-set! c 2 o))
    (if (>= i (- (vector-length c) 1))
      (begin
        (set! c (columnExtendAbove c (+ h 2)))
        (vector-set! c i o))
(vector-set! c i o)))
c))

(rem
(define (fun C)
(displayl "\r\nThe column:" C)
(displayl "\r\nHeight Bottom:" (columnHeightBottom C))
(displayl "\r\nHeight Top" (columnHeightTop C))
(display "\r\nObjects:")
(let ~ ((i 0)) (and (< i 15) (displayl "\c30;1m" i "\c0m" (columnRef C i) " ") (~ (+ i 1))))
(display "\r\n"))
(define C #(2  0 1 2  2 2 5))
(fun C)
(displayl "\r\ncolumn: " C)
(displayl "\r\ncolumnSet 8 99") (display (columnSet C 8 99))
(displayl "\r\ncolumnSet 11 11") (display (columnSet C 11 11))
(displayl "\r\ncolumnSet 3 77") (display (columnSet C 3 77))
(quit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field - Maps will populate the field as if it were a local cache relative
;;         to the avatar's position.  Plan on creating a 2x2 cache.
;;
(define FieldDimension 128) ; Field dimension

; Create default plane.
(define FIELD
 (vector-vector-map! 
   (lambda (i)
    ;(if (random-bool 2) (vector 2 XX GRASS AIR) (vector 2 XX DIRT  AIR))
    (vector 2 XX 	CONSTRUCT  AIR))
  (make-vector-vector FieldDimension FieldDimension ())))

;(vector-vector-map! (lambda (i) (display "\r\n") (display i) i) FIELD)

; Fields are 2d arrays of columns.  Columns are quasi-compressed stacks
; of cells that consist of a start height and specified stack of cells.
; #(3 1 1 2) would be cells (1 1 2) starting at z=4=(3+1). Cells below and
; above are assumed to be the lowest and higest specified cells in the vector.
; Setting a cell outside the explicit stack range expands the actual vector
; and adjusts the start-height value.
(define (field-column y x)
(vector-vector-ref FIELD (modulo y FieldDimension)
                         (modulo x FieldDimension)))

(define (field-ref z y x)
 (letrec ((column   (field-column y x))
          (elements (columnRef column z)))
   (if (pair? elements) (car elements) elements)));1st in improper list of objs


; Scan down map column starting at z for first visibile cell.  Return height.
; BF:  What should this return if z is below the bottom most explicit cell?
(define (field-ref-top z y x)
 (letrec ((column (field-column y x))
          (top    (columnHeightTop column)) ;1st implicit top cell in column
          (bot    (columnHeightBottom column)));1st implicit bottom cell in col
 ; Adjust the z coor down to the first explicit cell in the column
 (let findNonAir ((z (if (>= z top) (set! z (- top 1)))))
   (if (or (not (eq? (columnRef column z) AIR))
           (<= z bot))
       (if (pair? z) (car z) z) ; Return first in improper list of objects.
       (findNonAir (- z 1))))))

(define (field-set! z y x c)
 (letrec ((fy (modulo y FieldDimension))
          (fx (modulo x FieldDimension))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z (cons c (columnRef column z))))))

(define (field-delete! z y x e)
 (letrec ((fy (modulo y FieldDimension))
          (fx (modulo x FieldDimension))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z (list-delete (columnRef column z) e)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas
;;
(define CanvasDimension FieldDimension)

(define CANVAS (make-vector-vector CanvasDimension CanvasDimension ()))

(let ~ ((y 0)(x 0))
 (if (!= y CanvasDimension)
  (if (= x CanvasDimension)
      (~ (+ y 1) 0)
      ; Each canvas entry consists of a map cell, the cell height and
      ; top most visible height.
      (begin
       (vector-vector-set! CANVAS y x 
         (let ((top (field-ref-top 1000 y x)));What'll be higher than 1k?
           (vector (cell-ref (field-ref top y x))
                   top
                   1000)))
       (~ y (+ x 1))))
  (ConsoleDisplay "Initialized Canvas")))

(define (canvasRender y x)
 (let ((top (field-ref-top 1000 y x)))
  (canvasCellSet! y x (cell-ref (field-ref top y x)))
  (canvasCellHeightSet! y x top)))

(define (canvasCellRef y x)
  (vector-ref
     (vector-vector-ref CANVAS
       (modulo y CanvasDimension)
       (modulo x CanvasDimension))
     0))

(define (canvasCellSet! y x c)
  (vector-set! (vector-vector-ref CANVAS
                     (modulo y CanvasDimension)
                     (modulo x CanvasDimension))
               0 c))

(define (canvasCellHeightRef y x)
  (vector-ref (vector-vector-ref CANVAS
                   (modulo y CanvasDimension)
                   (modulo x CanvasDimension))
              1))
(define (canvasCellHeightSet! y x h)
 (vector-set! (vector-vector-ref CANVAS
                 (modulo y CanvasDimension)
                 (modulo x CanvasDimension))
              1 h))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viewport
;;
(define MapWindowSemaphore (open-semaphore 1))

(define PortY 0)
(define PortX 0)
(define PortH 0)
(define PortW 0)

(define (viewportReset y x)
 (set! PortH (WinMap 'WHeight)) ; Adjust Viewport dimensions.
 (set! PortW (/ (WinMap 'WWidth) 2))
 (set! PortY (- y (/ PortH 2)))     ; Center Viewport around Avatar.
 (set! PortX (- x (/ PortW 2)))
 ;((WinMap 'home))
 (let ~dumpGlyphs ((y 0) (x 0))
  (if (!= y PortH) (if (= x PortW)
    (~dumpGlyphs (++ y) 0)
    (begin
     (WinMapPutGlyph (canvasCellRef (+ PortY y) (+ PortX x)) y (* x 2))
     (~dumpGlyphs y (++ x))))))
 ; DEBUG: Plot column
 ((WinColumn 'home))
 (let ~ ((z 11))
  (let ((c (field-ref z y x)))
   (if (eq? AIR c)
    (begin (WinColumnSetColor #x08)
           (WinColumnPuts "()"))
    (begin (set! c (cell-ref c))
           (WinColumnSetColor (glyphColor0 c))
           (WinColumnPutc (glyphChar0 c))
           (WinColumnSetColor (glyphColor1 c))
           (WinColumnPutc (glyphChar1 c)))))
  (if (> z -6) (~ (- z 1)))))

(define (viewportRender gy gx)
 ; The cell position and viewport position (upper left corner) are on a torus
 ; coordinate system (wrap along the two dimensions).  I want to render to
 ; the screen cells plotted within the viewport .  I can do this by shifting
 ; the viewport to the origin and the cell by the same amount then checking if
 ; the cell is between the origin and lower right corner of the viewport.
 ;
 ; IE: (Cell % FieldWidth - Viewport % FieldWidth) % Fieldwidth < ViewportWidth
 ; But it would seem modulo distributes:  (a%m - b%m)%m == (a-b)%m%m == (a-b)%m
 ; so the actual computation is a bit simpler.  Smokin.
 (let ((y (modulo (- gy PortY) FieldDimension)) ; Normalize avatar position.
       (x (modulo (- gx PortX) FieldDimension)))
  (and (< y PortH) (< x PortW)
    (WinMapPutGlyph (canvasCellRef gy gx) y (* x 2)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entites - Simple objects more dynamic than just cells.
;;
(define (Entity dna name z y x glyph)
 (define (self msg) (eval msg))
 (define (setAll name0 z0 y0 x0 glyph0)
  (set! name name0)
  (set! z z0)
  (set! y y0)
  (set! x x0)
  (set! glyph glyph0))
 (define (setNameGlyph name0 glyph0)
  (set! name name0)
  (set! glyph glyph0))
 (define (setLoc z0 y0 x0)
  (set! z z0)
  (set! y y0)
  (set! x x0))
 (define (gps) (list z y x))
 self)

; Association list of entites to their DNA values.
(define EntityDB ())

; Update or create and add a new entity to the entity database.
(define (entitiesSet dna name z y x glyph)
 (let ((ent (assv dna EntityDB)))
  (if (null? ent)
      (set! EntityDB (cons (cons dna (Entity dna name z y x glyph))
                           EntityDB))
      (((cdr ent) 'setAll) name z y x glyph))))

; Lookup entity, or null, in database.
(define (entitiesGet dna)
 (let ((e (assv dna EntityDB)))
   (if (null? e)
       ()
       (cdr e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avatar
;;
(define (Avatar name) ; Inherits Entity
 ((Entity (random) name
   5 15 14
   (glyphNew #x0f (string-ref name 0)
             #x0f (string-ref name 1)))
  `(let ()
    (define (self msg) (eval msg))
    (define dir 0)
    (define (jump s r q) (set! z s) (set! y r) (set! x q))
    (define (face dirNew) (set! dir dirNew))
    (define (move)
      (if (= dir 0) (jump (+ z -1) y      x))
      (if (= dir 1) (jump z        (++ y) (-- x))
      (if (= dir 2) (jump z        (++ y) x)
      (if (= dir 3) (jump z        (++ y) (++ x))
      (if (= dir 4) (jump z        y      (-- x))
      (if (= dir 5) (jump (+ z 1)  y      x)
      (if (= dir 6) (jump z        y      (++ x))
      (if (= dir 7) (jump z        (-- y) (-- x))
      (if (= dir 8) (jump z        (-- y) x)
      (if (= dir 9) (jump z        (-- y) (++ x))))))))))))
    (define (walk dir)
     (face dir)
     (move))
    (define (look)
      (let ((loc (gps)))
        (move)
        (let ((c (cell-ref (field-ref z y x))))
          (apply setLoc loc)
          c)))
    (define cell BRICK)
    self) ))

(define avatar (Avatar "Guest"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incomming IPC messages 
;;
(define (entity dna name z y x glyph)
 (entitiesSet dna name z y x glyph)
;(canvasEntityAdd (entitiesGet dna) y x)
)

(define (who)
 ((ipc 'qwrite)
 `(entity ,(avatar 'dna) ,(avatar 'name) ,@((avatar 'gps)) ,(avatar 'glyph))))

;; Call this 'move.  Entites move.  Back to the original ways?
;; was 'avatar
(define (move dna z y x)
 (let ((entity (entitiesGet dna))
       (thisIsMe (= dna (avatar 'dna))))
  (if (null? entity)
    (begin
      (ConsoleDisplay "\r\nERROR: move: unknown avatar:" dna)
      (entitiesSet dna "??" z y x (glyphNew #x19 #\? #x19 #\?))
      ((ipc 'qwrite) '(who)))
    (begin
      ; Move from here
      (field-delete! (entity 'z) (entity 'y) (entity 'x) dna)
      (if (>= (entity 'z) (canvasCellHeightRef (entity 'y) (entity 'x))) (begin
        (canvasRender (entity 'y) (entity 'x))
        (or thisIsMe (viewportRender (entity 'y) (entity 'x)))))
      ; Place here
      ((entity 'setLoc) z y x)
      (field-set! z y x dna)
      (if (>= (entity 'z) (canvasCellHeightRef y x)) (begin
        (canvasRender y x)
        (or thisIsMe (viewportRender y x))))
      (if thisIsMe (begin
        (viewportReset (avatar 'y) (avatar 'x))
        ;(ConsoleDisplay (display->string ((avatar 'look))))
        ;(ConsoleDisplay "\r")
        ;(displayl "\c28H\c0m" (field-column (avatar 'y) (avatar 'x)) "\cK" )
        ))))))

(define (voice dna z y x level text)
 (let ((entity (entitiesGet dna)))
   (ConsoleDisplay "\r\n")
   (ConsoleSetColor #x0e)
   (if (null? entity)
     (ConsoleDisplay "???>")
     (ConsoleDisplay (entity 'name)))
   (ConsoleSetColor #x06)
   (ConsoleDisplay " ")
   (ConsoleDisplay text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Input
;;
(define AvatarDirectionGlyphs
(vector
   (glyphNew #x07 #\? #x07 #\?)
   (glyphNew #x4f #\\ #x4f #\_)
   (glyphNew #x4f #\\ #x4f #\/)
   (glyphNew #x4f #\_ #x4f #\/)
   (glyphNew #x4f #\< #x4f #\=)
   (glyphNew #x4f #\o #x4f #\o)
   (glyphNew #x4f #\= #x4f #\>)
   (glyphNew #x4f #\/ #x4f #\~)
   (glyphNew #x4f #\/ #x4f #\\)
   (glyphNew #x4f #\~ #x4f #\\)))

; BF: Should this be a thread?  At least move somewhere else.
(define (refreshIfBeyondViewport)
 (let ((y (modulo (- (avatar 'y) PortY) FieldDimension));Normalize avatar position.
   (x (modulo (- (avatar 'x) PortX) FieldDimension)))
   (if (or (<= PortW x) (<= PortH y))
     (viewportReset (avatar 'y) (avatar 'x)))))

(define (walk dir)
; (if (not (eq? BRICK (field-ref (avatar 'z) (avatar 'y) (avatar 'x))))
;   (begin
     ;(ConsoleDisplay (list (avatar 'z)":" (avatar 'y)":"(avatar 'x)"="))
     ;(ConsoleDisplay (list (field-ref 4 15 15) "," (field-ref 4 14 18)))
     ;(ConsoleDisplay (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
     ;(ConsoleDisplay "\r\n")
     ((avatar 'walk) dir)
     ((ipc 'qwrite) `(move ,(avatar 'dna) ,@((avatar 'gps))))
 ; Popup a message to a new window temporarily.
 (if (eq? DOOROPEN (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
  ((lambda ()
     (define WinTemp ((Terminal 'WindowNew) 1 29 1 22 #x1b))
     (define WinTempPutc (WinTemp 'putc))
     (map WinTempPutc (string->list "Are we having fun yet?"))
     (read-char stdin)
     ((WinTemp 'delete)))))
 (if (eq? HELP (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
     (help))
 (if (eq? SNAKE (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
     (thread (snake-random)))
 (if (eq? KITTY (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
     (thread (spawnKitty)))
 ; Dump our coordinates.
 ((WinStatus 'puts) "\r\n")
 ((WinStatus 'puts) (number->string (avatar 'z)))
 ((WinStatus 'puts) " ")
 ((WinStatus 'puts) (number->string (avatar 'y)))
 ((WinStatus 'puts) " ")
 ((WinStatus 'puts) (number->string (avatar 'x))))

;(define (fortune)
; (let ~ (( dv8 (open-socket "dv8.org" 80)))
;  (sleep 10)
;  (if (not (eof-object? dv8))
;    (begin
;      (send "GET /~shroom/fortune.cgi\n\n" dv8) (sleep 10)
;      (let ~ ((c (read-char dv8)))
;           (if (eof-object? c)
;            (close dv8)
;            (begin (speak c)
;                   (~ (read-char dv8)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get the wheels in motion
;;
;; Message window

;; Input Window
(define WinInput
 ((Terminal 'WindowNew)
  (- (Terminal 'THeight) 1) 0 1 (- (Terminal 'TWidth) 1) #x4a))
(define WinInputPutc (WinInput 'putc))
(define WinInputPuts (WinInput 'puts))
(define WinInputSetColor (WinInput 'set-color))

;; Map window
(define MapSize (min 20 (min (- (Terminal 'THeight) 1)
                             (/ (Terminal 'TWidth) 2))))
(define WinMap
  ((Terminal 'WindowNew)
    0 (- (Terminal 'TWidth) (* MapSize 2) 2)
    (+ MapSize 1) (* 2 MapSize)
    #x17 'NOREFRESH))

((WinMap 'cursor-visible) #f) ; Disable cursor in map window.

; Make Map window circular!
(let ~ ((y 0)(x 0))
 (if (< y (WinMap 'WHeight))
 (if (= x (WinMap 'WWidth)) (~ (+ y 1) 0)
  (begin
   (if (> (sqrt (+ (* 4 (^2 (- y (/ (WinMap 'WHeight) 2))))
                (^2 (- x  (/ (WinMap 'WWidth) 2)))))
          (WinMap 'WHeight))
       ((WinMap 'alpha) y x #f))
   (~ y (+ x 1))))))
(define WinMapSetColor (WinMap 'set-color))
(define WinMapPutc (WinMap 'putc))
(define (WinMapPutGlyph glyph y x)
  (semaphore-down MapWindowSemaphore)
  ((WinMap 'goto) y x)
  (WinMapSetColor (glyphColor0 glyph))
  (WinMapPutc     (glyphChar0 glyph))
  (WinMapSetColor (glyphColor1 glyph))
  (WinMapPutc     (glyphChar1 glyph))
  (semaphore-up MapWindowSemaphore))


;; Map column debug window
(define WinColumn ((Terminal 'WindowNew) 1 (- (Terminal 'TWidth) 2) 18 2 #x5b))
(define WinColumnPutc (WinColumn 'putc))
(define WinColumnPuts (WinColumn 'puts))
(define WinColumnSetColor (WinColumn 'set-color))

(define (welcome)
 (define WinMarquee
  ((Terminal 'WindowNew)
    2 (- (/ (Terminal 'TWidth) 1) 32)
    3 22
    #x0f))
 (define WinMarqueePuts (WinMarquee 'puts))
 (define WinMarqueePutc (WinMarquee 'putc))
 (define WinMarqueeSetColor (WinMarquee 'set-color))
 ((WinMarquee 'cursor-visible) #f) ; Disable cursor in map window.
 (WinMarqueePuts "+--------------------+")
 (WinMarqueePuts "|                    |")
 (WinMarqueePuts "+--------------------+")
 (let ~~ ((i -5))
   ((WinMarquee 'goto) 1 1)
   (sleep 200) ; Delay
   (let ~ ((j 0))
     (WinMarqueeSetColor (vector-ref #(07 07 07 07 07 07 07 07   07 07 07
                                       9  11 10 12 13 15 15 15 15 15)
                                     (modulo (+ i j) 21)))
     (WinMarqueePutc (vector-ref #(#\W #\e #\l #\c #\o #\m #\e #\   #\t #\o #\ 
                                   #\W #\o #\r #\l #\d #\  #\  #\  #\  #\ )
                                 (modulo (+ i j) 21)))
     (if (< j 19) (~ (+ j 1)))) ; Marquee area width
   (if (< i 60) ; Msg scroll count
       (~~ (+ i 1))))
 ((WinMarquee 'delete)))

(define (snake y x delay)
 (define Window3 ((Terminal 'WindowNew) y x 3 3 #x6b))
 (define Window3Puts (Window3 'puts))
 ((Window3 'alpha) 1 1 #f) ; Transparent window location.
 ((Window3 'cursor-visible) #f)
 (let ~ ((m 0))
   ((Window3 'home)) (Window3Puts "oO ") (Window3Puts ".  ") (Window3Puts ".  ")
   (sleep delay)
   ((Window3 'home)) (Window3Puts ".oO") (Window3Puts ".  ") (Window3Puts "   ")
   (sleep delay)
   ((Window3 'home)) (Window3Puts "..o") (Window3Puts "  O") (Window3Puts "   ")
   (sleep delay)
   ((Window3 'home)) (Window3Puts " ..") (Window3Puts "  o") (Window3Puts "  O")
   (sleep delay)
   ((Window3 'home)) (Window3Puts "  .") (Window3Puts "  .") (Window3Puts " Oo")
   (sleep delay)
   ((Window3 'home)) (Window3Puts "   ") (Window3Puts "  .") (Window3Puts "Oo.")
   (sleep delay)
   ((Window3 'home)) (Window3Puts "   ") (Window3Puts "O  ") (Window3Puts "o..")
   (sleep delay)
   ((Window3 'home)) (Window3Puts "O  ") (Window3Puts "o  ") (Window3Puts ".. ")
   (sleep delay)
   (if (< m 10) (~ (+ m 1)))) ; Loop 10 times
 ((Window3 'delete)))))

(define (snake-random)
 (snake
   (random (- (Terminal 'THeight) 3))
   (random (- (Terminal 'TWidth) 3))
   200))

(define WinHelp ((Terminal 'WindowNew) 5 20 10 30 #x21))

(map (lambda (x) ((WinHelp 'alpha) 0 x #f))
     '(0 1 2 3 4 5 6 7 8 20 21 22 23 24 25 26 27 28 29))

((WinHelp 'goto) 0 0)
((WinHelp 'puts) "         )) Help! ((")
((WinHelp 'set-color) #x20)
((WinHelp 'puts) "\r\n? = toggle help window\r\nt = talk\r\nesc = exit talk\r\nw = toggle map\r\nq = quit\r\nhjkl = move")
((WinHelp 'toggle))

(define (help)
 ; (WinHelp '(set! Y0 2))
 ; (WinHelp '(set! Y1 (+ Y0 WHeight)))
  ((WinHelp 'toggle)))

(ConsoleSetColor #x0b)
(ConsoleDisplay "\r\nSee http://code.google.com/p/worldtm")
(ConsoleDisplay "\r\nHit ? to toggle the help window")
(ConsoleDisplay "\r\n\r\n   *** Welcome to the construct. ***")



(define replTalk
 (let ((talkInput ""))
  (lambda (c)
   (if (eq? c 'getBuffer) talkInput ; Return input buffer contents.
   ; backspace
   (if (or (eq? c CHAR-CTRL-H)
           (eq? c CHAR-CTRL-_)
           (eq? c CHAR-CTRL-?))
       (begin
        (if (not (eq? "" talkInput))
         (begin ((WinInput 'backspace) #\ )
                (set! talkInput (substring talkInput 0 (- (string-length talkInput) 1)))))
        'talk)
   ; Send Chatter
   (if (or (eq? c RETURN) 
           (eq? c NEWLINE))
     (begin
       ; Set avatar's new name if certain phrase entered
       (let ((strLen (string-length talkInput)))
        (if (and (> strLen 11)
                 (string=? "my name is " (substring talkInput 0 11)))
            (begin
             ((avatar `setNameGlyph)
                (substring talkInput 11 strLen)
                (glyphNew #x0f (string-ref talkInput 11)
                          #x0f (string-ref talkInput (if (> strLen 12) 12 11))))
             (thread (begin (sleep 500) (who))))))
       ; Toggle help window if certain phrase entered
       (if (string=? "?" talkInput) (help))
       ; Send talk chatter to IPC or evaluate expression
       (if (and (not (eq? "" talkInput))
                (eq? #\: (string-ref talkInput 0)))
           (begin (ConsoleDisplay "\r\n")
                  (ConsoleDisplay talkInput)
                  (ConsoleDisplay "=>")
                  (ConsoleDisplay (eval (read-string (cdr-string talkInput)))))
           ((ipc 'qwrite) (list 'voice (avatar 'dna) (avatar 'z) (avatar 'y) (avatar 'x) 10 talkInput)))
       (WinInputPuts "\r\n>")
       (set! talkInput "")
       'talk)
   ; Quit chat mode.
   (if (or (eq? c CHAR-CTRL-ESC) ; Escape char
           (eq? c CHAR-CTRL-I)) ; Tab char
     (begin (WinInputPuts "\r\n")
            'cmd)
   (if (and (>= c #\ )(<= c #\~))
       (begin (WinInputPutc c)
              (set! talkInput (string talkInput c))
              'talk)
   'talk))))))))

(define (replCmd c)
 (define state 'cmd)
 (if (eq? c #\j) (walk 2)
 (if (eq? c #\B) (walk 2)
 (if (eq? c #\k) (walk 8)
 (if (eq? c #\A) (walk 8)
 (if (eq? c #\h) (walk 4)
 (if (eq? c #\D) (walk 4)
 (if (eq? c #\l) (walk 6)
 (if (eq? c #\C) (walk 6)
 (if (eq? c #\b) (walk 1)
 (if (eq? c #\n) (walk 3)
 (if (eq? c #\y) (walk 7)
 (if (eq? c #\u) (walk 9)
 (if (eq? c #\}) (walk 5)
 (if (eq? c #\{) (walk 0)
 (if (eq? c #\S) ((WinMap 'scrollUp))
 (if (eq? c #\a) (begin
                ((WinMap 'alpha) (- (avatar 'y) PortY)
                                 (* 2 (- (avatar 'x) PortX))
                                 #f)
                ((WinMap 'alpha) (- (avatar 'y) PortY)
                                 (+ 1 (* 2 (- (avatar 'x) PortX)))
                                 #f))
 (if (eq? c #\w) (begin
                   ((WinStatus 'toggle))
                   ((WinColumn 'toggle))
                   ((WinMap 'toggle)))
 (if (eq? c #\W) (begin
                   ((WinColumn 'toggle)))
 (if (eq? c #\t) (begin
                   (WinInputPuts (string ">" (replTalk 'getBuffer)))
                   (set! state 'talk))
 (if (eq? c CHAR-CTRL-K) ((ipc 'qwrite) `(set! FIELD ,FIELD)) ; Send my plane out to IPC.
 (if (or (eq? c #\q) (eq? c #\Q) (eof-object? c) (eq? c CHAR-CTRL-Q))
     (set! state 'done)
 (if (eq? c CHAR-CTRL-L)
     (begin
      (viewportReset (avatar 'y) (avatar 'x))
      ((Console 'repaint)))
 (if (eq? c CHAR-CTRL-F) (begin
    (set! AVATAR (glyphNew (+ (* (colorBackground AVATAR) 16)
                             (modulo (+ (glyphColor0 AVATAR) 1) 16))
                          (glyphChar0 AVATAR)
                          (+ (* (colorBackground AVATAR) 16)
                             (modulo (+ (glyphColor0 AVATAR) 1) 16))
                          (glyphChar0 AVATAR)))
    (who))
 (if (eq? c CHAR-CTRL-_) (walk 4)
 (if (eq? c #\d)
    ((ipc 'qwrite) `(dropCell ,(avatar 'y) ,(avatar 'x) BRICK)) ; Send my plane out to IPC.
   ;(field-set! (avatar 'z) (avatar 'y) (avatar 'x) (avatar 'cell))
   ;(canvasRender (avatar 'y) (avatar 'x))
   ;(viewportRender (avatar 'y) (avatar 'x)))
 (if (eq? c #\f) ((Terminal 'WindowMaskDump))
 (if (eq? c #\g)
   (let ((o (field-ref (avatar 'z) (avatar 'y) (avatar 'x))))
     (field-delete!  (avatar 'z) (avatar 'y) (avatar 'x) o)
     (avatar `(set! cell ,o)))
 (if (eq? c #\?) (help)
 (if (eq? c #\-) (begin
                  ((WinMap 'resize)
                     (+ -1 (WinMap 'WHeight))
                     (+ -2 (WinMap 'WWidth)))
                  (rem ConsoleDisplay (WinMap 'WHeight) " " (WinMap 'WWidth)))
 (if (eq? c #\+) (begin
                  ((WinMap 'resize)
                     (+ 1 (WinMap 'WHeight))
                     (+ 2 (WinMap 'WWidth)))
                  (rem ConsoleDisplay (WinMap 'WHeight) " " (WinMap 'WWidth)))
))))))))))))))))))))))))))))))
 state)

(define wrepl (let ((state 'cmd))
 (lambda ()
  (if (not (eq? state 'done))
   (let ((c (read-char stdin)))
    (if (eq? state 'talk) (set! state (replTalk c))
     (if (eq? state 'cmd)  (set! state (replCmd c))))
    (wrepl))))))

;; Stats window
(define WinStatus ((Terminal 'WindowNew)
   (WinMap 'Y0) (- (Terminal 'TWidth) 12)
   1            12
   #x43))

((ipc 'qwrite) '(who))
;((ipc 'qwrite) `(voice ,(avatar 'dna) ,@((avatar 'gps)) 10 "Hello World[tm]!"))
(viewportReset (avatar 'y) (avatar 'x))


;; Walking kitty soldier
(define (spawnKitty)
 (letrec ((kitty (Avatar "Kat"))
          (dir->card (lambda (d) (vector-ref #(6 9 8 7 4 1 2 3) d)))
          (card->dir (lambda (c) (vector-ref #(0 5 6 7 4 0 0 3 2 1) c)))
          (happyVector (vector 0 0 0 0 0 0 0 0))
          (dist 0))
 ; Tell everyone who I am.
 ((ipc 'qwrite) `(entity ,(kitty 'dna) "kitty" ,@((kitty 'gps)) ,(glyphNew #x07 #\K #x0f #\a)))
 (let ~ ((i 0)) ; Main loop
   ; Distance from parent avatar
   (set! dist (distance ((kitty 'gps)) ((avatar 'gps))))
   ; Neuron depletion.
   (if (= i 10) (vector-map! (lambda (x) (/ x 2)) happyVector))
   ; Walk kitty quasi-randomly.
   ((kitty 'walk)
       (dir->card (letrec ((dir (card->dir (kitty 'dir)))
                           (dir1 (modulo (+ dir (random 3) -1) 8))
                           (dir2 (modulo (+ dir (random 3) -1) 8)))
                   (if (> (vector-ref happyVector dir1)
                          (vector-ref happyVector dir2))
                       dir1 dir2))))
   (vector-set! happyVector (card->dir (kitty 'dir))
       (+ (let ((kd (distance ((kitty 'gps))
                              ((avatar 'gps)))))
            (if (< kd dist) 1
            (if (= kd dist) -1
                            -2)))
          (vector-ref happyVector (card->dir (kitty 'dir)))))
   ((ipc 'qwrite) `(move ,(kitty 'dna) ,@((kitty 'gps))))
(rem
   ((Console 'goto) 10 0)
   (ConsoleDisplay (vector-ref happyVector 3))
   (ConsoleDisplay " \t")
   (ConsoleDisplay (vector-ref happyVector 2))
   (ConsoleDisplay " \t")
   (ConsoleDisplay (vector-ref happyVector 1))
   (ConsoleDisplay "   \r\n")
   (ConsoleDisplay (vector-ref happyVector 4))
   (ConsoleDisplay " \t\t")
   (ConsoleDisplay (vector-ref happyVector 0))
   (ConsoleDisplay "   \r\n")
   (ConsoleDisplay (vector-ref happyVector 5))
   (ConsoleDisplay " \t")
   (ConsoleDisplay (vector-ref happyVector 6))
   (ConsoleDisplay " \t")
   (ConsoleDisplay (vector-ref happyVector 7))
   (ConsoleDisplay "   ")
)
   (sleep 800)
   ;(if (equal? ((kitty 'gps))
   ;            ((avatar 'gps)))
   ;    ((ipc 'qwrite) `(voice ,(kitty 'dna) ,@((kitty 'gps)) 10 "Mrrreeeooowww!")))
   (or (= i 100) (~ (+ i 1)))))) ; spawnKitty


;(thread (snake 18 38 500))
;(thread (snake 18 75 200))
;(thread (snake 1 75 100))
(thread (welcome))
;(ConsoleDisplay "\r\nWelcome to World")
;(display "\e[?25h")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drop some cells on the map
(define (dropCell y x cell)
 (let ((z (+ 1 (field-ref-top 1000 y x))))
  (field-set! z y x cell)
  (canvasRender y x)
  (viewportRender y x)))

(define (build-brick-room yy xx)
 (let ~ ((i 0))
    (sleep 5000)
    (dropCell yy         (+ xx i)        BRICK)
    (dropCell (+ yy 10)  (+ 10 xx (- i)) BRICK)
    (dropCell (+ 10 yy (- i)) xx         BRICK)
    (dropCell (+ yy i)   (+ xx 10)       BRICK)
    (if (< i 9) (~ (+ i 1)))))

(thread (build-brick-room 10 10)) ; Sleep since canvas hasn't been created yet

(dropCell 13 17 HELP)
(dropCell 16 18 SNAKE)
(dropCell 19 16 KITTY)
(dropCell 12 13 TV)
(dropCell 17 13 CHAIR)



; Screen redraw signal handler.
(vector-set! SIGNAL-HANDLERS 28 (lambda ()
  (ConsoleDisplay "\r\n" (terminal-size))
  ((Terminal 'ResetTerminal))
  ((Console 'resize) (- (Terminal 'THeight) 1) (Terminal 'TWidth))
  ((WinMap 'move) 0 (- (Terminal 'TWidth) (* MapSize 2) 2))
  ((WinColumn 'move) 1 (- (Terminal 'TWidth) 2) )
  ((WinStatus 'move) (WinMap 'Y0) (- (Terminal 'TWidth) 12))
  ((WinInput 'resize) 1 (- (Terminal 'TWidth) 1))
  ((WinInput 'move) (- (Terminal 'THeight) 1) 0)
  (unthread)))
(signal 28)

;; Example on how to implement a signal handler.
;; This will capture control C spaning a thread which just displays "hi".
;(define (catch) (display 'hi) (unthread))
;(vector-set! SIGNAL-HANDLERS 2 catch)
;(signal 2)

(wrepl)
(display "\e[0m\r\n\e[?25h")
(quit)

