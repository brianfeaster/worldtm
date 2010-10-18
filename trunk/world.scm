;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; World
;;   Windows
;;    Glyphs
;;    Cells
;;    Column
;;    Field
;;    Canvas
;;    Viewport
;;    Map_manipulation
;;   Entites_and_avatar
;;    Window_functions_and_initialization
;;    Incomming_IPC_messages
;;   Button_commands
;;   Buttons
;;   Typing_and_talking
;;    Prototypes_and_fun_things
;;   Start_everything
;;
(load "ipc.scm")
(load "window.scm")
(load "entity.scm")
(define QUIETLOGIN (and (< 2 (vector-length argv)) (eqv? "silent" (vector-ref argv 2))))
(define CELLANIMATION #t)
(define SCROLLINGMAP #t)
(define KITTEHBRAIN  #f)
(define VOICEDELIMETER " ")
(define NAME "Guest")
(define DNA 0)
(define ActivityTime (time))
(define MapBlockSize 32) ; Size of each map file cell in the World map.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;
(define Terminal (Terminal)) ; Force only one instance of the Terminal object by
                             ; setting the object value to an instance of itself.
; Chat window.
(define WinChat ((Terminal 'BufferNew)
  0 0
  (- (Terminal 'Theight) 1)  (Terminal 'Twidth)
  #x0f))
(define WinChatPutc (WinChat 'putc))
(define WinChatPuts (WinChat 'puts))
(define WinChatSetColor (WinChat 'set-color))
(define (WinChatDisplay . l)
  (for-each (lambda (o) (for-each WinChatPuts (display->strings o))) l))
(define (WinChatWrite o)
  (for-each (WinChat 'puts) (write->strings o)))
(WinChat '(set! ScrollbackHack #t))

; Console window
(define WinConsole ((Terminal 'WindowNew)
  (- (Terminal 'Theight) 14) 0
  13  (Terminal 'Twidth)
  #x02))
(define WinConsolePuts (WinConsole 'puts))
(define (WinConsoleDisplay . e) (for-each (lambda (x) (for-each WinConsolePuts (display->strings x))) e))
(define (WinConsoleWrite . e) (for-each (lambda (x) (for-each WinConsolePuts (write->strings x))) e))
((WinConsole 'toggle))

; Input Window
(define WinInput ((Terminal 'WindowNew)
  (- (Terminal 'Theight) 1) 0
  1 (Terminal 'Twidth)
  #x4a))
(define WinInputPutc (WinInput 'putc))
(define WinInputPuts (WinInput 'puts))
(define WinInputSetColor (WinInput 'set-color))

; Map Window. Initial map size is 20 or terminal width/height.
(define WinMap
 (let ((MapSize (min 28 (min (- (Terminal 'Theight) 1)
                             (/ (Terminal 'Twidth) 2)))))
  ((Terminal 'WindowNew)
    0 (- (Terminal 'Twidth) (* MapSize 2)) ; Position of the map window
    (+ MapSize 0) (* 2 MapSize)
    #x0f 'NOREFRESH)))
((WinMap 'toggle))
((WinMap 'cursor-visible) #f) ; Disable cursor in map window
(define WinMapSetColor (WinMap 'set-color))
(define WinMapPutc (WinMap 'putc))

; Help Window.
(define WinHelpBorder ((Terminal 'WindowNew) 4 20 16 32 #x20))
(define WinHelp ((Terminal 'WindowNew) 5 21 14 30 #x0a))
((WinHelpBorder 'toggle))
((WinHelp 'toggle))

;; Stats window
(define WinStatus ((Terminal 'WindowNew)
   (WinMap 'Y0) (- (Terminal 'Twidth) 14)
   3            14
   #x4e))
((WinStatus 'toggle))
(define (WinStatusDisplay . e)
  (for-each (lambda (x) (for-each (WinStatus 'puts) (display->strings x))) e))

;; Map column debug window
(define WinColumn ((Terminal 'WindowNew) 3 (- (Terminal 'Twidth) 4) 18 4 #x5b))
((WinColumn 'toggle))
(define WinColumnPutc (WinColumn 'putc))
(define (WinColumnPuts . l) (for-each (WinColumn 'puts) l))
(define WinColumnSetColor (WinColumn 'set-color))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glyphs - Two multi-colored characters.
;;
(define glyphNew vector)
(define (glyph0bg cell) (vector-ref cell 0))
(define (glyph0fg cell) (vector-ref cell 1))
(define (glyph0ch cell) (vector-ref cell 2))
(define (glyph1bg cell) (vector-ref cell 3))
(define (glyph1fg cell) (vector-ref cell 4))
(define (glyph1ch cell) (vector-ref cell 5))

(define glyphUNKNOWN (glyphNew 0 8 #\? 0 8 #\?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cells - A table of vectors #(symbol solidFlag glyph)
;;
(define (cellSymbol cell) (vector-ref cell 0))
(define (cellSolid  cell) (vector-ref cell 1))
(define (cellGlyph  cell) (vector-ref cell 2))

(define (cellMake symb glyph . flags)
  (vector symb
          (if (null? (memq 'solid flags)) #f 'solid)
          glyph))

(define CellMax 1023)
(define Cells (make-vector (+ 1 CellMax) (cellMake 'unknown glyphUNKNOWN)))

; Get the cell index given its symbol
; Return #f if nonexistant
(define (cellIndex sym)
  (let ~ ((index 0))
    (if (< CellMax index) #f ; Not found
    (let ((cell (vector-ref Cells index)))
      (if (and cell (eq? (cellSymbol cell) sym)) index ; Found
      (~ (+ index 1)))))))

; Get the cell given a symbol or index
; Return #f if index out of range of symbol nonexistent
(define (cellRef o)
  (if (integer? o)
    (if (and (<= 0 o) (<= o CellMax)) (vector-ref Cells o) #f)
    (let ~ ((i 0)) ; Find the cell via symbol
      (if (< CellMax i) #f
      (let ((cell (vector-ref Cells i)))
        (if (and cell (eq? (cellSymbol cell) o)) cell ; Return the cell
        (~ (+ i 1))))))))

; Create a new cell and save in Cells table
(define (cellSet i symb glyph . flags)
  (vector-set! Cells i
    (if (null? (memq 'solid flags))
      (cellMake symb glyph)
      (cellMake symb glyph 'solid))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Column
;;  Somewhat compact group of objects in one dimension.  Compressed
;;  by including all values within a range with the objects below and above
;;  the range the first and last values in the vector.
;;
;;  Z coordinate   -2 -1  0  1  2| 3  4  5  6  7| 8  9 10 11
;;                ---------------+--------------+-------------
;;                  0  0  0  0  0| 0  5  7  4  9| 9  9  9  9
;;       Implied bottom objects  | Real objects | Implied top objects
;;                               |              |
;;    Internal Vector data    #(2  0  5  7  4  9)
;;
;;  Is stored as #(2 0 5 7 4 9) where the first value in the vector is the
;;  position of the first default object. The lower and upper default object
;;  are stored in the 2nd and last position. The position of the top most
;;  default object is derived by adding the vector length to the first vector
;;  value.
;;
; Create a column given an initial height and a stack of cells
(define (columnMake bottomHeight . cells)
  (letrec ((len (length cells))
           (col (make-vector (+ len 1))))
    (vector-set! col 0 (- bottomHeight 1)) ; Set bottom height of cells
    (vector-set-list! col 1 cells) ; Copy cells
    col))

; Height of first default implicit bottom object
(define (columnHeightBottom c)
  (vector-ref c 0))

; Height of first default implicit top object
(define (columnHeightTop c)
  (+ (vector-ref c 0)
     (vector-length c)))

(define (columnRef c h)
 (vector-ref c
  (let ((i (- h (columnHeightBottom c))))
    (if (<= i 1) 1
    (if (<= (vector-length c) i) (- (vector-length c) 1)
    i)))))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field
;;
; Create default plane.
(define FieldSize 256) ; Field grid is 256x256
(define FIELD (make-vector-vector FieldSize FieldSize #f))

; Initialize the canvas which is a vector of pairs (cellGlyph . cellHeight)
(define (resetField defaultColumn)
  (loop2 0 FieldSize 0 FieldSize (lambda (y x)
    ; Each canvas entry consists of a map cell and its height.
    (vector-vector-set! FIELD y x defaultColumn))))

; Fields are 2d arrays of columns.  Columns are quasi-compressed stacks
; of cells that consist of a start height and specified stack of cells.
; #(3 1 1 2) would be cells (1 1 2) starting at z=4=(3+1). Cells below and
; above are assumed to be the lowest and higest specified cells in the vector.
; Setting a cell outside the explicit stack range expands the actual vector
; and adjusts the start-height value.
(define (fieldColumn y x)
 (vector-vector-ref FIELD (modulo y FieldSize)
                          (modulo x FieldSize)))

; Query the first cell at this location.  Mainly used as the object to display.
(define (field-ref z y x)
 (letrec ((column   (fieldColumn y x))
          (elements (columnRef column z)))
   (if (pair? elements) (car elements) elements)));1st in improper list of objs

; Query the last cell at this location.  Used to get the base/non-entity object.
(define (field-base-ref z y x)
 (letrec ((column   (fieldColumn y x))
          (elements (columnRef column z)))
   (last elements))) ; Last in improper list of objs

; Scan down map column starting at z for first visibile cell.  Return height.
; TODO  What should this return if z is below the bottom most explicit cell?
(define (field-ref-top z y x)
 (letrec ((column (fieldColumn y x))
          (top    (columnHeightTop column)) ;1st implicit top cell in column
          (bot    (columnHeightBottom column)));1st implicit bottom cell in col
 ; Adjust the z coor down to the first explicit cell in the column
 (let findNonAir~ ((z (if (<= top z) (- top 1) z)))
   (if (or (!= (columnRef column z) cellAIR)
           (<= z bot))
       (if (pair? z) (car z) z) ; Return first in improper list of objects.
       (findNonAir~ (- z 1))))))

; Replace all cells at this Z location with a single cell.
(define (field-set! z y x c)
 (letrec ((fy (modulo y FieldSize))
          (fx (modulo x FieldSize))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z c ))))

; Insert the cell at this Z location.  Creates a malformed list.
(define (field-add! z y x c)
 (letrec ((fy (modulo y FieldSize))
          (fx (modulo x FieldSize))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z (cons c (columnRef column z))))))

; Remove the cell from the malformed list at this Z location.
(define (field-delete! z y x e)
 (letrec ((fy (modulo y FieldSize))
          (fx (modulo x FieldSize))
          (column (vector-vector-ref FIELD fy fx)))
  (vector-vector-set! FIELD fy fx
     (columnSet column z (list-delete (columnRef column z) e)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas
;; A canvas entry is a vector consisting of a glyph and it's Z coordinate (the
;; top most visible usually).
;;
(define CANVAS (make-vector-vector FieldSize FieldSize #f))

; Initialize the canvas which is a vector of pairs (cellGlyph . cellHeight)
(define (resetCanvas . defaultGlyph)
  (set! defaultGlyph (if (null? defaultGlyph) #f (car defaultGlyph)))
  (loop2 0 FieldSize 0 FieldSize (lambda (y x)
    ; Each canvas entry consists of a map cell and its height.
    (vector-vector-set! CANVAS y x
      (if defaultGlyph
        (cons defaultGlyph 0)
        (let ((top (field-ref-top 10 y x)))
          (cons (cellGlyph (cellRef (field-base-ref top y x))) top)))))))

(define (canvasGlyph y x)
  (car (vector-vector-ref CANVAS
         (modulo y FieldSize)
         (modulo x FieldSize))))

(define (canvasHeight y x)
  (cdr (vector-vector-ref CANVAS
         (modulo y FieldSize)
         (modulo x FieldSize))))

(define (canvasCellSet y x c)
  (set-car! (vector-vector-ref CANVAS
              (modulo y FieldSize)
              (modulo x FieldSize))
            c))

(define (canvasHeightSet y x h)
 (set-cdr! (vector-vector-ref CANVAS
             (modulo y FieldSize)
             (modulo x FieldSize))
           h))

(define (canvasRender y x)
 (let ((top (field-ref-top 100 y x)))
  (let ((celli (field-ref top y x))) ; Field might contain an entity's dna
    (canvasCellSet y x (if (< CellMax celli) ((entitiesGet celli) 'glyph) (cellGlyph (cellRef celli)))))
  (canvasHeightSet y x top)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viewport - Dumps to terminal window visible glyphs pre-rendered on the
;;            canvas grid.
;;
(define PortCY 0) ; Center of map location
(define PortCX 0)
(define PortY 0) ; Upper left corner loation
(define PortX 0)
(define PortH 0)
(define PortW 0)

(define (viewportPlot glyph y x)
  ((WinMap 'goto) y x)
  ; The glyph index is based on the time which cycles to the next one every second.
  (let ((t (time)) ; Glyph index based on time
        (l (- (vector-length glyph) 5))) ; Number of animation glyphs ignoring the first
    (if (> (modulo t l) 0) (set! glyph (vector-ref glyph (+ 5 (modulo t l)))))
    ; 1st char
    (WinMapSetColor (glyph0bg glyph) (glyph0fg glyph))
    (WinMapPutc (glyph0ch glyph))
    ; 2nd char
    (WinMapSetColor (glyph1bg glyph) (glyph1fg glyph))
    (WinMapPutc (glyph1ch glyph))))

(define (viewportReset y x)
 ((Terminal 'lock)) ; This shouldn't be such an all encompasing lock.
 (set! PortCY y)
 (set! PortCX x)
 (set! PortH (WinMap 'Wheight)) ; Adjust Viewport dimensions
 (set! PortW (/ (WinMap 'Wwidth) 2))
 (set! PortY (- y (/ PortH 2)))     ; Center Viewport around Avatar
 (set! PortX (- x (/ PortW 2)))
 (loop2 0 PortH 0 PortW (lambda (y x) ; Render glyphs in viewport
   (viewportPlot (canvasGlyph (+ PortY y) (+ PortX x))
                 y (* x 2))))
 ((Terminal 'unlock)) ; This shouldn't be such an all encompasing lock.
 (if (WinColumn 'ENABLED) (dumpColumnInfo y x)))


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
 ((Terminal 'lock)) ; This shouldn't be such an all encompasing lock.
 (let ((y (modulo (- gy PortY) FieldSize)) ; Normalize avatar position.
       (x (modulo (- gx PortX) FieldSize)))
  (and (< y PortH) (< x PortW) (begin
    (viewportPlot (canvasGlyph gy gx) y (* x 2))))
 ((Terminal 'unlock)))) ; This shouldn't be such an all encompasing lock.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map_manipulation
;;
; Drop a cell on the map
(define (dropCell y x cell)
 (let ((z (+ 1 (field-ref-top 1000 y x))))
  (field-set! z y x cell)
  (canvasRender y x)
  (viewportRender y x)))

; Set map cell and force rendering of cell through entire pipeline.
(define (setCell z y x cell)
  (field-set! z y x cell)
  (canvasRender y x)
  (viewportRender y x))

; Given a cell index or entity DNA value, move it in the field, canvas and viewport.
(define (moveCell cell zo yo xo z y x centerMap)
  ; Old location removal
  (field-delete! zo yo xo cell)
  (if (>= zo (canvasHeight yo xo)) (begin
    (canvasRender yo xo)
    (or centerMap (viewportRender yo xo))))
  ; New location added
  (field-add! z y x cell)
  (if (>= z (canvasHeight y x)) (begin
    (canvasRender y x)
    (if centerMap
      (viewportReset (avatar 'y) (avatar 'x))
      (viewportRender y x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entites_and_avatar
;; Simple objects more dynamic than just cells.
;;

; Association list of entites to their DNA values.
(define EntityDB ())

; Create a default "nobody" entity.
(define nobody (Entity 42 0 "nobody" 0 0 0 (glyphNew 0 7 #\n 0 7 #\o)))

(define (entityAdd entity)
 (set! EntityDB (cons (cons (entity 'dna) entity) EntityDB)))

; Update or create and add a new entity to the entity database.
(define (entitiesSet dna port name z y x glyph)
 (let ((ent (assv dna EntityDB)))
  (if (null? ent)
      (entityAdd (Entity dna port name z y x glyph)) ; Clone the remote entity locally.
      (((cdr ent) 'setAll) port name z y x glyph))))

; Lookup entity, or null, in database.
(define (entitiesGet dna)
 (let ((e (assv dna EntityDB)))
   (if (null? e)
       nobody
       (cdr e))))

; The user's avatar
(define (Avatar port name) ; Inherits Entity
 ((Entity (random) port name
   1 108 86
   (glyphNew 0 15 (string-ref name 0) 0 15 (string-ref name 1)))
  `(let ()
    (define (self msg) (eval msg))
    (define dir 0)
    (define (face dirNew)
      (set! dir dirNew))
    (define (move)
      (if (= dir 0) (setLoc (+ z -1) y      x)
      (if (= dir 1) (setLoc z        (++ y) (-- x))
      (if (= dir 2) (setLoc z        (++ y) x)
      (if (= dir 3) (setLoc z        (++ y) (++ x))
      (if (= dir 4) (setLoc z        y      (-- x))
      (if (= dir 5) (setLoc (+ z 1)  y      x)
      (if (= dir 6) (setLoc z        y      (++ x))
      (if (= dir 7) (setLoc z        (-- y) (-- x))
      (if (= dir 8) (setLoc z        (-- y) x)
      (if (= dir 9) (setLoc z        (-- y) (++ x)))))))))))))
    (define (walk dir)
      (face dir)
      (move))
    (define (look)
      (apply field-ref (gpsLook)))
    (define cell 19)
    (define (gpsLook)
      (if (= dir 0) (list (+ z -1) y      x)
      (if (= dir 1) (list z        (++ y) (-- x))
      (if (= dir 2) (list z        (++ y) x)
      (if (= dir 3) (list z        (++ y) (++ x))
      (if (= dir 4) (list z        y      (-- x))
      (if (= dir 5) (list (+ z 1)  y      x)
      (if (= dir 6) (list z        y      (++ x))
      (if (= dir 7) (list z        (-- y) (-- x))
      (if (= dir 8) (list z        (-- y) x)
      (if (= dir 9) (list z        (-- y) (++ x)))))))))))))
    self)))



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
((WinHelp 'puts) "\r\nC  color for talking")
((WinHelp 'puts) "\r\nW  who is here list")
((WinHelp 'puts) "\r\nM  map toggle")
((WinHelp 'puts) "\r\nS  scrolling map toggle")
((WinHelp 'puts) "\r\nA  animation toggle")
((WinHelp 'puts) "\r\n>  increase map size")
((WinHelp 'puts) "\r\n<  decrease map size")
((WinHelp 'puts) "\r\nQ  quit World[tm]")
((WinHelp 'puts) "\r\nHJKL move map")
((WinHelp 'puts) "\r\n* To walk use arrows keys")
((WinHelp 'puts) "\r\n  or 'nethack' keys")

; Make Map window circular
(define (circularize . val)
 (set! val (not (null? val))) ; Default to disabling circular corners of map.
 (let ~ ((y 0)(x 0))
  (if (< y (/ (WinMap 'Wheight) 2))
  (if (= x (/ (WinMap 'Wwidth) 2)) (~ (+ y 1) 0)
   (begin
    (if (> (sqrt (+ (* 4 (^2 (- y (/ (WinMap 'Wheight) 2))))
                    (^2 (- x  (/ (WinMap 'Wwidth) 2)))))
           (+ 0 (WinMap 'Wheight)))
        (begin
          ((WinMap 'alpha) y x val)
          ((WinMap 'alpha) y (- (WinMap 'Wwidth) x 1) val)
          ((WinMap 'alpha) (- (WinMap 'Wheight) y 1) x val)
          ((WinMap 'alpha) (- (WinMap 'Wheight) y 1)
                           (- (WinMap 'Wwidth) x 1) val)
          ))
    (~ y (+ x 1)))))))
(circularize)

; Plot column of cells.
(define (dumpColumnInfo y x)
 ((WinColumn 'home))
 (let ~ ((z 11))
  (let ((c (field-ref z y x)))
   (if (eqv? cellAIR c)
    (begin (WinColumnSetColor 0 8)
           (WinColumnPuts "()  "))
    (begin (set! c (if (< CellMax c) ((entitiesGet c) 'glyph)
                                     (cellGlyph (cellRef c)))) ; Dump the glyph
           (WinColumnSetColor (glyph0bg c) (glyph0fg c))
           (WinColumnPutc (glyph0ch c))
           (WinColumnSetColor (glyph1bg c) (glyph1fg c))
           (WinColumnPutc (glyph1ch c))
           (WinColumnSetColor 0 7)
           (set! c (field-base-ref z y x)) ; Display base cell's hex value.
           (if (and (<= 0 c) (< c 256))
             (begin
               (if (< c 16) (WinColumnPuts "0"))
               (WinColumnPuts (number->string c 16)))
             (WinColumnPuts "  ") ))))
  (if (> z -6) (~ (- z 1)))))


; Screen redraw function and signal handler
(define (handleTerminalResize)
  (WinConsoleDisplay "\r\nSIGWINCH::TerminalSize=" (terminal-size))
  ((Terminal 'ResetTerminal))
  ((WinChat 'resize)  (- (Terminal 'Theight) 1) (Terminal 'Twidth))
  ((WinMap 'move)     0 (- (Terminal 'Twidth) (WinMap 'Wwidth) 2))
  ((WinColumn 'move)  1 (- (Terminal 'Twidth) 2) )
  ((WinStatus 'move)  (WinMap 'Y0) (- (Terminal 'Twidth) 14))
  ((WinInput 'resize) 1 (Terminal 'Twidth))
  ((WinInput 'move)   (- (Terminal 'Theight) 1) 0))

(define sigwinch
 (let ((sig28Semaphore (open-semaphore 1)))
  (lambda ()
   (semaphore-down sig28Semaphore)
   (handleTerminalResize)
   (semaphore-up sig28Semaphore))))

(signal-set 28 (lambda () (sigwinch) (unthread)))


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
 ;((box 'cursor-visible) #f)
 (puts "+------------------+")
 (puts "|                  |")
 (puts "+------------------+")
 ((box 'goto) 0 2) (puts title)
 ((box 'goto) 1 1)
 (let ~ ((c (getKey)))
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
     (~ (getKey)))))
 ((box 'delete))
 str)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incomming_IPC_messages
;;
(define (who)
  ((ipc 'qwrite)
    `(entity ,DNA ,(avatar 'port) ,(avatar 'name) ,@((avatar 'gps)) ,(avatar 'glyph))))

;; Entitity movement
(define (move dna z y x)
 (or (= dna DNA)
   (let ((entity (entitiesGet dna)))
     (if (null? entity)
       (begin
         (entitiesSet dna 0 "??" z y x (glyphNew 1 9 #\? 1 9 #\?))
         ((ipc 'qwrite) '(who)))
       (begin
         (moveCell dna (entity 'z) (entity 'y) (entity 'x) z y x #f)
         ((entity 'setLoc) z y x))))))

(define (entity dna port name z y x glyph)
  (entitiesSet dna port name z y x glyph)
  (move dna z y x))

(define (die dna)
 (let ((entity (entitiesGet dna))
       (thisIsMe (= dna DNA)))
  (if (not (null? entity)) ; Ignore unknown entities
    (begin
      ; Remove from here
      (field-delete! (entity 'z) (entity 'y) (entity 'x) dna)
      (if (>= (entity 'z) (canvasHeight (entity 'y) (entity 'x))) (begin
        (canvasRender (entity 'y) (entity 'x))
        (or thisIsMe (viewportRender (entity 'y) (entity 'x)))))))))

(define (force z y x dir str)
 (if (and (= z (avatar 'z)) (= y (avatar 'y)) (= x (avatar 'x)))
   (walk dir)))

(define (voice dna level text)
 (if (= dna 0)
  (begin ; Message from the system
    (WinChatDisplay "\r\n")
    (WinChatSetColor 0 9) (WinChatDisplay "W")
    (WinChatSetColor 0 11) (WinChatDisplay "O")
    (WinChatSetColor 0 10) (WinChatDisplay "R")
    (WinChatSetColor 0 12) (WinChatDisplay "L")
    (WinChatSetColor 0 13) (WinChatDisplay "D")
    (WinChatSetColor 0 8) (WinChatDisplay VOICEDELIMETER)
    (WinChatSetColor 0 7) (WinChatDisplay text))
  (let ((entity (entitiesGet dna)))
    (WinChatSetColor (glyph0bg (entity 'glyph)) (glyph0fg (entity 'glyph)))
    (WinChatDisplay "\r\n" (if (null? entity) "???" (entity 'name)) VOICEDELIMETER)
    (WinChatSetColor (glyph1bg (entity 'glyph)) (glyph0fg (entity 'glyph)))
    (WinChatDisplay text)))
 (if (and (!= dna DNA) (eqv? text "unatco")) (say "no Savage")))

; The list of columns will most likely come from a map agent
; The map coordinate and block size passed
(define (mapUpdateColumns y x blockSize lst)
  ;(WinChatDisplay "\r\nBlock " (list (/ y 32) (/ x 32)))
  (loop2 y (+ y blockSize) x (+ x blockSize) (lambda (y x)
    (vector-vector-set! FIELD (modulo y FieldSize) (modulo x FieldSize) (car lst))
    (canvasRender y x)
    (set! lst (cdr lst)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Button_commands
;;
; Toggle the help window
(define (help)
 ((WinHelp 'toggle))
 ((WinHelpBorder 'toggle)))

(define deltaMoveTime (utime)) ; Double click 1/16 sec.

(define (winMapBigger)
 (if (< (WinMap 'Wheight) (Terminal 'Theight)) (begin
  ((Terminal 'lock))
  ((WinMap 'home))
  (if (< (utime) deltaMoveTime)
    ((WinMap 'moveresize) ; Full resize
       0 (- (Terminal 'Twidth) (WinMap 'Wwidth) 2)
             (min (/ (Terminal 'Twidth) 2) (- (Terminal 'Theight) 1))
       (* 2  (min (/ (Terminal 'Twidth) 2) (- (Terminal 'Theight) 1)))))
    ((WinMap 'moveresize) ; Resize by one
       0 (- (Terminal 'Twidth) (WinMap 'Wwidth) 2)
       (+ 1 (WinMap 'Wheight))
       (+ 2 (WinMap 'Wwidth)))
  (circularize)
  ((Terminal 'unlock))
  (viewportReset (avatar 'y) (avatar 'x))
  (set! deltaMoveTime (+ 125 (utime))))))

(define (winMapSmaller)
 (if (< 5 (WinMap 'Wheight)) (begin
  ((Terminal 'lock))
  ((WinMap 'home))
  (if (< (utime) deltaMoveTime)
    ((WinMap 'moveresize) ; Full resize
       0 (- (Terminal 'Twidth) (WinMap 'Wwidth) -10)
       (+ -5 (WinMap 'Wheight))
       (+ -10 (WinMap 'Wwidth)))
    ((WinMap 'moveresize) ; Resize by one
       0 (- (Terminal 'Twidth) (WinMap 'Wwidth) -2)
       (+ -1 (WinMap 'Wheight))
       (+ -2 (WinMap 'Wwidth))))
  (circularize)
  ((Terminal 'unlock))
  (viewportReset (avatar 'y) (avatar 'x))
  (set! deltaMoveTime (+ 125 (utime))))))

(define (winMapUp)  ((WinMap 'move)  (+ -1 (WinMap 'Y0)) (WinMap 'X0)))
(define (winMapDown) ((WinMap 'move)  (+  1 (WinMap 'Y0)) (WinMap 'X0)))
(define (winMapLeft)    ((WinMap 'move) (WinMap 'Y0) (+ -1 (WinMap 'X0))))
(define (winMapRight)  ((WinMap 'move) (WinMap 'Y0) (+  1 (WinMap 'X0))))

; TODO Implement coor+dir use new dir to field-ref location and if > MAX_CELL ipc-write (force blah blah)

(define (walk dir)
 ((avatar 'face) dir) ; Turn avatar
 (let ((nextCell ((avatar 'look)))) ; Consider cell I'm walking into
   (if (< CellMax nextCell) ; Cell I'm walking into is probably an entity
    ; Push the entity that's in my way
    ((ipc 'qwrite) `(force ,@((avatar 'gpsLook)) ,dir 10))
    ; Walk normally
    (or (cellSolid (cellRef nextCell)) (begin
      ((avatar 'move)) ; Update avatar's state
      ((ipc 'qwrite) (list 'move DNA (avatar 'z)(avatar 'y)(avatar 'x))) ; Update avatar over IPC
      ; Update avatar in map
      (moveCell DNA (avatar 'oz) (avatar 'oy) (avatar 'ox) (avatar 'z) (avatar 'y) (avatar 'x)
         (or SCROLLINGMAP (< 10 (distance (list 0 (avatar 'y)           (avatar 'x))
                                          (list 0 (+ PortY (/ PortH 2)) (+ PortX (/ PortW 2)))))))
      ;(if (eq? 'help  (cellSymbol (field-ref (avatar 'z) (avatar 'y) (avatar 'x))))  (help))
      ;(if (eq? 'snake (cellSymbol (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))) (thread (snake-random)))
      ;(if (eq? 'brit2 (cellSymbol (field-base-ref (avatar 'z) (avatar 'y) (avatar 'x)))) (thread (spawnKitty)))
      ;(WinConsoleWrite (fieldColumn (avatar 'y) (avatar 'x)) (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))
      ; Dump our coordinates.
      (WinStatusDisplay "\r\n"
         (number->string (avatar 'z)) " "
         (number->string (avatar 'y)) " "
         (number->string (avatar 'x)) "\r\n"
         (number->string (modulo (avatar 'y) MapBlockSize)) " "
         (number->string (modulo (avatar 'x) MapBlockSize)) "\r\n"
         (number->string (/ (avatar 'y) MapBlockSize)) " "
         (number->string (/ (avatar 'x) MapBlockSize))))))))

; Change avatar color.  Will just cycle through all 16 avatar colors.
(define (avatarColor)
 (let ((glyph (avatar 'glyph)))
  ((avatar 'setGlyph) (glyphNew
    (glyph0bg glyph)  (modulo (+ (glyph0fg glyph) 1) 16)  (glyph0ch glyph)
    (glyph1bg glyph)  (modulo (+ (glyph1fg glyph) 1) 16)  (glyph1ch glyph))))
  (who))

(define (rollcall)
 ((ipc 'qwrite) ; Force all to evaluate the following
  `(if (!= DNA ,DNA) ; Skip if I sent this message
   ((ipc 'qwrite) ; Force all (except me) to evaluate the following
    `(if (= DNA ,,DNA) ; If me, evaluate this expression from the other peer
     (voice 0 10
      (string ,(avatar 'name) " "
              ,(let ((t (- (time) ActivityTime)))
                (if (< t 60)   (string (number->string t) "s")
                (if (< t 3600) (string (number->string (/ t 60)) "m")
                (if (< t 86400)(string (number->string (/ t 3600)) "h")
                (string (number->string (/ t 86400)) "d"))))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buttons
;;
;; Buttons keep track of expresions to keyboard buttons in either a vector of
;; character indexes and a symbol association list.
;;
(define Buttons (make-vector 257 ()))
(define ButtonsSymbols ())

(define (setButton ch exp)
 (if (symbol? ch)
   (let ((bs (assq ch ButtonsSymbols)))
     (if (pair? bs)
       (set-cdr! bs exp)
       (set! ButtonsSymbols (cons (cons ch exp) ButtonsSymbols))))
   (vector-set! Buttons ch exp))) ; For now characters are also integer constants.

(define ShowButtons #f)
(define (showButtons) (set! ShowButtons (not ShowButtons)))

; Consider the button value.  If data just return the expression.  Otherwise
; assume a closure.  Consider the closure -> closure's code -> the code's pre-compiled
; expression and return it (hack).
(define (getButton ch)
 (let ((val
         (if (symbol? ch)
          (let ((a (assq ch ButtonsSymbols)))
            (if (pair? a) (cdr a) ()))
          (vector-ref Buttons ch))))
   (if (pair? val)  val
   (if (procedure? val) (cons 'lambda (vector-ref (vector-ref (vector-ref Buttons ch) 0) 2))
   val))))

(setButton 'down '(walk 2))
(setButton #\j '(walk 2))
(setButton 'up '(walk 8))
(setButton #\k '(walk 8))
(setButton 'left '(walk 4))
(setButton #\h '(walk 4))
(setButton 'right '(walk 6))
(setButton #\l '(walk 6))
(setButton #\b '(walk 1))
(setButton #\n '(walk 3))
(setButton #\y '(walk 7))
(setButton #\u '(walk 9))
(setButton #\+ '(walk 5))
(setButton #\- '(walk 0))
(setButton #\A '(begin
  (set! CELLANIMATION (not CELLANIMATION))
  (WinChatDisplay "\r\nCell animation " CELLANIMATION)))
(setButton #\C '(avatarColor))
(setButton #\W '(rollcall))
(setButton #\H '(winMapLeft))
(setButton #\J '(winMapDown))
(setButton #\K '(winMapUp))
(setButton #\L '(winMapRight))
(setButton #\S '(begin
  (set! SCROLLINGMAP (not SCROLLINGMAP))
  (WinChatDisplay "\r\nalwaysScroll " SCROLLINGMAP)))
(setButton #\M '((WinMap 'toggle)))
(setButton #\t '(begin
  (WinInputPuts (string ">" (replTalk 'getBuffer)))
  (set! state 'talk)))
(setButton CHAR-CTRL-D '(ipc '(set! Debug (not Debug))))
(setButton CHAR-CTRL-F '(walkForever))
(setButton CHAR-CTRL-L '(begin (viewportReset (avatar 'y) (avatar 'x)) ((WinChat 'repaint))))
(setButton CHAR-CTRL-M '(begin ((WinStatus 'toggle)) ((WinColumn 'toggle))))
(setButton CHAR-CTRL-Q '(set! state 'done))
(setButton #\d '((ipc 'qwrite) `(dropCell ,(avatar 'y) ,(avatar 'x) (cellRef 'campfire))))
(setButton #\g
   '(let ((o (field-ref (avatar 'z) (avatar 'y) (avatar 'x))))
     (field-delete!  (avatar 'z) (avatar 'y) (avatar 'x) o)
     (avatar `(set! cell ,o))))
(setButton #\? '(help))
(setButton #\< '(winMapSmaller))
(setButton #\> '(winMapBigger))
(setButton #\z '(circularize))
(setButton #\Z '(circularize #t))
(setButton #\Q '(set! state 'done))
(setButton #eof '(set! state 'done))
(setButton CHAR-CTRL-C '((WinConsole 'toggle)))
;(setButton CHAR-CTRL-K '((ipc 'qwrite) `(set! FIELD ,FIELD))) ; Send my plane out to IPC.
;(setButton #\1 '(thread (spawnKitty 1000)))
;(setButton #\1 '((WinChat 'resize) (WinChat 'Wheight) (WinChat 'Wwidth)))
(setButton #\1 '((WinChat 'scrollUp)))
;(setButton CHAR-CTRL-_ '(walk 4)) ; Sent by backspace?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typing_and_talking
;;

; Continuously read stdin and append to the "keyboard" FIFO
(define getKeySemaphore (open-semaphore 0))
(define keyQueue (QueueCreate))

(define (KeyAgent)
 (let ((c (read-char stdin)))
   (if (eq? c CHAR-ESC)
    (keyAgentEsc) ; Escape char so attempt to read an escape sequence
    (begin
      (QueueAdd keyQueue c) ; Add new keyboard character to queue
      (semaphore-up getKeySemaphore))) ; flag semaphore
   (KeyAgent))) ; rinse repeat

; Read an escape sequence
(define (keyAgentEsc)
 (let ((c (read-char stdin)))
   (if (eq? c #\[)
     (let ((c (read-char stdin)))
       (if (eq? c #\A)
         (begin
           (QueueAdd keyQueue 'up) 
           (semaphore-up getKeySemaphore))
       (if (eq? c #\B)
         (begin
           (QueueAdd keyQueue 'down) 
           (semaphore-up getKeySemaphore))
       (if (eq? c #\C)
         (begin
           (QueueAdd keyQueue 'right) 
           (semaphore-up getKeySemaphore))
       (if (eq? c #\D)
         (begin
           (QueueAdd keyQueue 'left) 
           (semaphore-up getKeySemaphore))
       (begin
           (QueueAdd keyQueue CHAR-ESC) 
           (QueueAdd keyQueue #\[) 
           (QueueAdd keyQueue c) 
           (semaphore-up getKeySemaphore)
           (semaphore-up getKeySemaphore)
           (semaphore-up getKeySemaphore)))))))
     (begin
       (QueueAdd keyQueue CHAR-ESC)
       (QueueAdd keyQueue c)
       (semaphore-up getKeySemaphore)
       (semaphore-up getKeySemaphore)))))

(define (getKey)
 (semaphore-down getKeySemaphore)
 (QueueGet keyQueue))


(define (say talkInput . level)
 ((ipc 'qwrite) (list 'voice DNA (if (null? level) 10 (car level)) talkInput)))

(define (saySystem talkInput)
 ((ipc 'qwrite) (list 'voice 0 10 talkInput)))

(define (sayHelloWorld)
 (saySystem (string
     (avatar 'name)
     " says "
     (vector-random #("*PUSH* *SQUIRT* *SPANK* *WAAAAAAAAA*" "*All Worldlians Want to Get Borned*" "*Happy Birthday*" "*I thought you were in Hong Kong*")))))

(define (sayByeBye)
  (saySystem (string (avatar 'name) " exits")))

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
       ; Perform actions based on talk phrases.
       (tankTheOperator talkInput)
       ; Toggle help window if certain phrase entered
       (if (string=? "?" talkInput) (help))
       ; Send talk chatter to IPC or evaluate expression
       (if (and (not (eq? "" talkInput))
                (eq? #\: (string-ref talkInput 0)))
           (begin (WinChatDisplay "\r\n")
                  (WinChatDisplay talkInput)
                  (WinChatDisplay "=>")
                  (WinChatDisplay
                    (call/cc (lambda (c) ; Return here if an error occurs
                       (vector-set! ERRORS (tid) c)
                       (eval (read-string (cdr-string talkInput)))))))
           (say talkInput))
       (WinInputPuts "\r\n>")
       (set! talkInput "")
       'talk)
   ; Quit chat mode.
   (if (or (eq? c CHAR-ESC) ; Escape char
           (eq? c CHAR-CTRL-I)) ; Tab char
     (begin (WinInputPuts "\r\n")
            'cmd)
   (if (and (>= c #\ )(<= c #\~))
       (begin (WinInputPutc c)
              (set! talkInput (string talkInput c))
              'talk)
   'talk))))))))

(define (replCmd c)
 (define state 'cmd) ; state might be changed to 'done or 'talk.
 (let ((button (getButton c)))
   (if ShowButtons (WinChatDisplay "\r\n" c " " button))
   (if (pair? button) (eval button)
    (if (procedure? button) (button)
     (WinChatDisplay "\r\nButton " c " undefined " button))))
 state)

(define wrepl
 (let ((state 'cmd))
  (lambda ()
   (if (not (eq? state 'done)) ; Exit if state is done.
     (let ((c (getKey)))
       (set! ActivityTime (time))
       (if (eq? state 'talk) (set! state (replTalk c))
         (if (eq? state 'cmd) (set! state (replCmd c))))
       (wrepl))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prototypes_and_fun_things
;;
;; Walking kitty soldier
(define (spawnKitty . cycles)
 (set! cycles (if (null? cycles) 128 (car cycles))) ; Set max cycles
 (letrec ((kitty (Avatar "Kat"))
          (dir->card (lambda (d) (vector-ref #(6 9 8 7 4 1 2 3) d)))
          (card->dir (lambda (c) (vector-ref #(0 5 6 7 4 0 0 3 2 1) c)))
          (happyVector (vector 0 0 0 0 0 0 0 0))
          (dist 0))
 ((kitty 'setLoc) (avatar 'z) (avatar 'y) (avatar 'x))
 ; Tell everyone who this kitteh is.
 ((ipc 'qwrite) `(entity ,(kitty 'dna) "kitty" ,@((kitty 'gps)) ,(glyphNew 0 7 #\K 0 15 #\a)))
 (let ~ ((i 0)) ; Main loop
   ; Distance from parent avatar
   (set! dist (distance ((kitty 'gps)) ((avatar 'gps))))
   ; Neuron depletion.
   (if (= 0 (modulo i 10)) (vector-map! (lambda (x) (/ x 2)) happyVector))
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
            (if (= kd dist) -1 -2)))
          (vector-ref happyVector (card->dir (kitty 'dir)))))
   ((ipc 'qwrite) `(move ,(kitty 'dna) ,@((kitty 'gps))))
   (if KITTEHBRAIN (begin
     ((WinChat 'goto) 10 5) (WinChatDisplay (vector-ref happyVector 3) "  ")
     ((WinChat 'goto) 10 8) (WinChatDisplay (vector-ref happyVector 2) "  ")
     ((WinChat 'goto) 10 11)(WinChatDisplay (vector-ref happyVector 1) "  ")
     ((WinChat 'goto) 11 5) (WinChatDisplay (vector-ref happyVector 4) "  ")
     ((WinChat 'goto) 11 11)(WinChatDisplay (vector-ref happyVector 0) "  ")
     ((WinChat 'goto) 12 5) (WinChatDisplay (vector-ref happyVector 5) "  ")
     ((WinChat 'goto) 12 8) (WinChatDisplay (vector-ref happyVector 6) "  ")
     ((WinChat 'goto) 12 11)(WinChatDisplay (vector-ref happyVector 7) "  ")))
   (sleep 200)
   ;(if (equal? ((kitty 'gps))
   ;            ((avatar 'gps)))
   ;    ((ipc 'qwrite) `(voice ,(kitty 'dna) 10 "Mrrreeeooowww!")))
   (if (> i (+ cycles (random 30)))
       ((ipc 'qwrite) `(die ,(kitty 'dna))) ; kill entity
       (~ (+ i 1))))))


(define walkForever (let ((walkForeverFlag #f)) (lambda ()
 (if walkForeverFlag
  (begin
   (set! walkForeverFlag #f)
   (WinChatSetColor 0 10)
   (WinChatDisplay "\r\nThus ends the journey"))
  (begin
   (WinChatSetColor 0 10)
   (WinChatDisplay "\r\nThe journey begins")
   (set! walkForeverFlag #t)
   (thread (let ~ ()
     (for-each
       (lambda (x) (or walkForeverFlag (unthread)) (walk x) (sleep 400))
       '(6 6 6 6 8 8 8 8 4 4 4 4 2 2 2 2))
     (sleep 500)
     (~))))))))


; Tank agent - The first interactive user agent.
(define (tankTalk str) (thread (begin
  (sleep 700)
  (WinChatSetColor 0 15)
  (WinChatDisplay "\r\nTank ")
  (WinChatSetColor 0 7)
  (WinChatDisplay str))))

(define tankHangupTime 0)
(define tankIsListening #f)

(define (tankStartListening)
  (set! tankHangupTime (+ 10 (time)))
  (tankTalk "Operator")
  (if (not tankIsListening) (begin (thread (let ~ ()
    (set! tankIsListening #t)
    (sleep 12000)
    (if (< (time) tankHangupTime)
      (~)
      (begin
       (tankTalk "*CLICK*")
       (set! tankIsListening #f))))))))

(define (tankTheOperator talkInput)
 (if (string=? talkInput "tank") (tankStartListening)
 (let ((strLen (string-length talkInput)))
  (if (and (> strLen 11)
           (string=? "my name is " (substring talkInput 0 11)))
     (begin
      ((avatar `setNameGlyph)
         (substring talkInput 11 strLen)
         (glyphNew (glyph0bg (avatar 'glyph))
                   (glyph0fg (avatar 'glyph))
                   (string-ref talkInput 11)
                   (glyph1bg (avatar 'glyph))
                   (glyph1fg (avatar 'glyph))
                   (string-ref talkInput (if (> strLen 12) 12 11))))
      (thread (begin (sleep 500) (who))))))) ; Wait before whoing so I say my new name using my old name
 (if tankIsListening (begin
   (if (string=? "who" talkInput) ((ipc 'qwrite) '(say "I'm here!")))
   (if (string=? "load the jump program" talkInput) (tankTalk "I can't find the disk")
   (if (string=? "load underworld" talkInput) (thread (load-ultima-underworld))
   (if (string=? "load ultima5" talkInput) (thread (load-ultima-world5))))))))


; Display the same string repeatedly with colors of increasing inensity.
(define (fancyDisplay c s)
 (for-each
   (lambda (c)
        (WinChatSetColor 0 c)
        (WinChatDisplay "\r" s)
        (sleep 1))
   (list 8 4 12 6 7 14 15 14 7 6 12 4 8 c))
 "")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start_everything
;;

; Initial cell indices and glyphs
(define cellWATER1 1)
(define cellBRICK  19)
(define cellXX     (- CellMax 1))
(define cellAIR    CellMax)

(define glyphXX  (glyphNew 7  0 #\X  7  0 #\X))
(define glyphAIR (glyphNew 3 12 #\A  4  3 #\r))

(cellSet cellXX  'xx  glyphXX 'solid)
(cellSet cellAIR 'air glyphAIR)

(load "ultima4.cells")

; Initialize field and canvas structures
(resetField (columnMake 0 cellXX cellAIR))
(resetCanvas glyphXX)
;(resetCanvas)

; Start keyboard reader agent
(thread (KeyAgent))

(or QUIETLOGIN (begin
 ; Welcome marquee
 (thread (welcome))
 ; Ask for name via text box
 (let ((name (boxInput "Enter your name")))
   (or (eq? name "") (set! NAME name)))))

; Create ipc object.  Pass in a debug message output port (can be empty lambda)
(define ipc (Ipc WinConsoleDisplay))
(ipc '(set! Debug #f))

; Create avatar object and add to entity list
(define avatar (Avatar (ipc 'PrivatePort) NAME))
((avatar 'setGlyph)
   (glyphNew 0 15 (string-ref NAME 0)
             0 15 (string-ref NAME 1)))
(set! DNA (avatar 'dna))
(entityAdd avatar)

; Move avatar to entrance of Lord British's castle
((avatar 'setLoc) 1  (* 108 MapBlockSize)  (+ (* 86 MapBlockSize) (/ MapBlockSize 2) -1))

; Display some initial information
(or QUIETLOGIN (begin
 (fancyDisplay 9 "Welcome to World")
 (WinChatDisplay "\n")
 (fancyDisplay 10 "See http://code.google.com/p/worldtm")
 (WinChatDisplay "\n")
 (fancyDisplay 12 "Hit ? to toggle the help window")
 (WinChatDisplay "\n")
 (fancyDisplay 5 (string "Your name is " (avatar 'name)))))

; Always read and evaluate everything from IPC.
(thread
 ; Set the thread's error handler to a continuation so any user or IPC scheme error is caught.
 (let ((s (call/cc (lambda (c) (vector-set! ERRORS (tid) c) 'starting))))
    (or (eq? s 'starting) (WinChatDisplay "\r\nIPC-REPL-ERROR::" s)))
 (let ~ ()
  (let ((sexp ((ipc 'qread))))
     (eval sexp)
     (~))))

; Redraw map resulting in animated cells.
(thread (let ~ ()
   (sleep 1000)
   (if CELLANIMATION (viewportReset PortCY PortCX))
   (~)))

((WinMap 'toggle))

(define (shutdown)
  (or QUIETLOGIN (sayByeBye))
  ((ipc 'qwrite) `(die ,DNA)) ; Kill avatar's entity
  (sleep 1000) ; wait for ipc to flush
  (displayl "\e[" (Terminal 'Theight) "H\r\n\e[0m\e[?25h")
  (quit))

; Catch some signal so that normal shutdown can occur
; TODO buggy repeated calls to the same handler occurs with I/O signals
(signal-set 1 (lambda () (say "signal 1 HUP")  (shutdown)))
(signal-set 2 (lambda () (say "signal 2 INT")  (shutdown)))
(signal-set 3 (lambda () (say "signal 3 QUIT")  (shutdown)))
(signal-set 6 (lambda () (say "signal 6 ABRT")  (shutdown)))
;(signal-set 13 (lambda () (say "signal 13 PIPE")  (shutdown)))
(signal-set 15 (lambda () (say "signal 15 TERM")  (shutdown)))

(or QUIETLOGIN (sayHelloWorld))
((ipc 'qwrite) '(who))
(wrepl)
(shutdown)
