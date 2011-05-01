;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The World Client
;;
;;   Cells_object
;;   Column_object
;;   Field_object
;;   Canvas_object
;;   Viewport_object
;;   Map_object
;;    Avatar_object
;;

(load "ipc.scm")
(load "window.scm")
(load "entity.scm") ; Glyph Sprite Entity EntityDb objects

(define SHUTDOWN #f) ; Signals to avatar's that the process is going to shutdown
(define QUIETLOGIN (and (< 2 (vector-length argv)) (eqv? "ADMINISTRATOR" (vector-ref argv 2))))
(define VIEWPORTANIMATION #t)
(define MAPSCROLL 'always) ; always edge never
(define VOICEDELIMETER " ")
(define MapBlockSize 32) ; Size of each map file cell in the World map.
(define PortMapAgent #f)
(define EDIT #f)
(define WhisperTalkThreshold 4)
(define TalkScreamThreshold  64)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cells_object - A table of vectors #(symbol solidFlag glyph)
;;
(define (cellSymbol cell) (vector-ref cell 0))
(define (cellSolid? cell) (vector-ref cell 1))
(define (cellGlyph  cell) (vector-ref cell 2))

(define (cellMake symb glyph . flags)
  (vector symb
          (if (null? (memq 'solid flags)) #f 'solid)
          glyph))

(define CellMax 1023) ; Max cell index
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
; Return #f if index out of range or the symbol is non-existent
(define (cellRef o)
  (if (integer? o)
    (if (cellValidIndex? o) (vector-ref Cells o) #f)
    (let ~ ((i 0)) ; Find the cell via symbol
      (if (< CellMax i) #f
      (let ((cell (vector-ref Cells i)))
        (if (and cell (eq? (cellSymbol cell) o)) cell ; Return the cell
        (~ (+ i 1))))))))

; Create a new cell and save in Cells table
; Currently used by ultima4.cells
(define (cellSet i symb glyph . flags)
  (vector-set! Cells i
    (if (null? (memq 'solid flags))
      (cellMake symb glyph)
      (cellMake symb glyph 'solid))))

; Is the cell index a visible cell?  Anything but an entity and air.
; Air is CellMax value.  Maybe it should be 0?
(define (cellVisible? c)
 (and (<= 0 c) (< c CellMax)))

(define (cellValidIndex? o) (and (<= 0 o) (<= o CellMax)))

(define cellxx  (- CellMax 2))
(define cellXX  (- CellMax 1))
(define cellAIR CellMax)

(cellSet cellxx  'xx  (Glyph 7  0 #\x  7  0 #\x) 'solid)
(cellSet cellXX  'XX  (Glyph 7  0 #\X  7  0 #\X) 'solid)
(cellSet cellAIR 'air (Glyph 3 12 #\A  4  3 #\r))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Column_object
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

(define (columnRef column z)
 (vector-ref column (let ((i (- z (columnHeightBottom column)))) ; normalize z coordinate
                      (cond ((<= i 1) 1) ; default bottom cell
                            ((<= (vector-length column) i) (- (vector-length column) 1)) ; default top cell
                            (else i)))))

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
    (cond ((<= i 1)
            (set! c (columnExtendBelow c (- h 2)))
            (vector-set! c 2 o))
          ((>= i (- (vector-length c) 1))
            (set! c (columnExtendAbove c (+ h 2)))
            (vector-set! c i o))
          (else
            (vector-set! c i o)))
    c))

(define (containsVisibleCell? l)
  (if (pair? l)
      (or (containsVisibleCell? (car l))
          (containsVisibleCell? (cdr l)))
      (cellVisible? l)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field_object
;;
;; Fields are arrays of columns.  Columns are quasi-compressed stacks of cells
;; and/or entity DNA values (could be any object).  The actual content of the
;; column is the start height-1 and actual stack of cells.  #(3 1 1 99887 1024)
;; would be cells (1 1 99887 1024) starting at z=4=(3+1). Cells below and above
;; are assumed to be the lowest and higest specified cells in the vector.
;; Setting a cell outside the explicit stack range expands the actual vector and
;; adjusts the start-height value.
(define (Field size) ; Field dimension is currently 256x256
  (define (self msg) (eval msg))
  (define field (make-vector-vector size size #f))
  (define (column y x)
   (vector-vector-ref field (modulo y size) (modulo x size)))
  ; Query the first cell at this location.  Mainly used as the object to display.
  (define (firstRef z y x)
   (let ((elements (columnRef (column y x) z)))
     (if (pair? elements) (car elements) elements))) ; 1st in improper list of objs
  ; Query the last cell at this location.  Used to get the base/non-entity object.
  (define (baseRef z y x)
   (letrec ((col   (column y x))
            (elements (columnRef col z)))
     (last elements))) ; Last in improper list of objs
  ; Scan down map column starting at z for first visibile cell.  Return height.
  (define (topHeight z y x)
   (letrec ((col (column y x))
            (top (columnHeightTop col)) ;1st implicit top cell in column
            (bot (+ (columnHeightBottom col) 1)));1st implicit bottom cell in col
    ; Adjust the z coor down to the first explicit cell in the column
    (let findNonAir~ ((z (if (<= top z) (- top 1) z)))
      (if (or (!= (columnRef col z) cellAIR)
              (<= z bot))
          z ; Return first in improper list of objects.
          (findNonAir~ (- z 1))))))
  ; Scan up map column starting at z for first visibile cell.  Return height.
  (define (ceiling z y x)
   (letrec ((col (column y x))
            (top (topHeight 100 y x))) ; top most visible cell in column
   (let ~ ((i (+ z 1)))
     (cond ((containsVisibleCell? (columnRef col i))  i)
           ((<= top i) 100)
           (else (~ (+ i 1)))))))
  ; Replace all cells at this Z location with a single cell.
  (define (fieldSet! z y x c)
   (letrec ((fy (modulo y size))
            (fx (modulo x size))
            (col (vector-vector-ref field fy fx)))
    (vector-vector-set! field fy fx
       (columnSet col z c ))))
 ; Insert the cell at this Z location.  Creates a malformed list.
  (define (add z y x c)
   (letrec ((fy (modulo y size))
            (fx (modulo x size))
            (col (vector-vector-ref field fy fx)))
    (vector-vector-set! field fy fx
       (columnSet col z (cons c (columnRef col z))))))
  ; Remove the cell from the malformed list at this Z location.
  (define (delete z y x e)
   (letrec ((fy (modulo y size))
            (fx (modulo x size))
            (col (vector-vector-ref field fy fx)))
    (vector-vector-set! field fy fx
       (columnSet col z (list-delete (columnRef col z) e)))))
  (define (updateColumns fieldy fieldx blockSize cellAry)
   (loop2 0 blockSize 0 blockSize
     (lambda (y x)
       (vector-vector-set! field (+ y fieldy) (+ x fieldx)
          (vector-copy (vector-vector-ref cellAry y x))))))
  (define (reset defaultColumn)
    (loop2 0 size 0 size (lambda (y x)
      (vector-vector-set! field y x defaultColumn))))
  (define (setField newField) ; TODO debugging
    (set! field newField)
    (WinChatDisplay "\r\nOK vector-length " (vector-length newField)))
  ; MAIN
  ; Initialize the field with a default column
  ;(WinChatDisplay "\r\nInitializing field...")
  (reset (columnMake 0 cellXX cellAIR)) ; Initialize the canvas with a bogus cell and height to speed up initializing
  self) ; Field



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas_object
;; A canvas entry is a vector-vector consisting of a glyph and it's Z
;; coordinate (the top most visible cell relative to the user usually).
;;
(define (Canvas aField anEntityDB)
  (define (self msg) (eval msg))
  ; Useful field values
  (define size (aField 'size))
  (define fieldCell (aField 'firstRef))
  (define fieldTopHeight (aField 'topHeight))
  ; The canvas array.  An array of (cellGlyph . cellHeight)
  (define canvas (make-vector-vector size size #f))
  (define (resetArray ceilingHeight . defaultGlyph)
    (set! defaultGlyph (if (null? defaultGlyph) #f (car defaultGlyph)))
    (loop2 0 size 0 size (lambda (y x)
      ; Each canvas entry consists of a map cell and its height
      ; Create pair either from defaultGlyph or based on the first visible cell
      (vector-vector-set! canvas y x
        (if defaultGlyph
          (cons defaultGlyph 0) ; Default
          (letrec ((t (fieldTopHeight ceilingHeight y x)) ; First visible
                   (celli (fieldCell t y x)))
            (cons (if (cellValidIndex? celli)
                    (cellGlyph (cellRef celli)) ; A cell's glyph
                    (((anEntityDB 'get) celli) 'glyph)) ; An entity's glyph
                  t)))))))
  (define (glyph y x)
    (car (vector-vector-ref canvas
           (modulo y size)
           (modulo x size))))
  (define (height y x)
    (cdr (vector-vector-ref canvas
           (modulo y size)
           (modulo x size))))
  (define (glyphSet y x c)
    (set-car! (vector-vector-ref canvas
                (modulo y size)
                (modulo x size))
              c))
  (define (heightSet y x h)
   (set-cdr! (vector-vector-ref canvas
               (modulo y size)
               (modulo x size))
             h))
  (define (render top y x)
   (let ((z (fieldTopHeight top y x))) ; Get z of first cell starting at top
     (let ((celli (fieldCell z y x))) ; Field might contain an entity's dna
       (glyphSet y x (if (< CellMax celli)
                               (letrec ((ent ((anEntityDB 'get) celli))
                                        (sprite (ent 'sprite))
                                        (ey (ent 'y))
                                        (ex (ent 'x)))
                                 ((sprite 'glyphRef) (- y ey) (- x ex)))
                               (cellGlyph (cellRef celli)))))
     (heightSet y x z)))
  ; MAIN
  ;(WinChatDisplay "\r\nInitializing canvas...")
  ; Initialize the canvas
  (resetArray 10 (cellGlyph (cellRef cellxx)))
  self) ; Canvas



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viewport_object
;;   Parent is window.
;;   Dumps to a window visible glyphs pre-rendered on the canvas array.
;;   Handles other requests such as refreshing the entire viewport
;;   and redrawing individual cells.
;;
; Initial map window size is 25 or terminal width/height fit.
(define (Viewport y x h w clr aCanvas . ChildStack)
 (apply (Terminal 'WindowNew) y x h w clr
  (list aCanvas)
  (macro (parent aCanvas . ChildStack) ; Child
    (define (self msg) (eval msg))
    (define canvasSize (aCanvas 'size))
    (define canvasGlyph (aCanvas 'glyph))
    (define my 0) ; Location of viewport origin in map coordinates
    (define mx 0) ;
    (define winHeight 0)
    (define winWidth 0)
    (define mapCenterY (/ canvasSize 2)) ; Location of viewport center in map coordinates
    (define mapCenterX (/ canvasSize 2)) ;
    (define NextDrawTime (utime))
    (define (plot glyph y x)
      ; The glyph index is based on the time which cycles to the next one every second.
      (let ((t (time)) ; Glyph index based on time
            (l (- (vector-length glyph) 5))) ; Number of animation glyphs ignoring the first
        (if (> (modulo t l) 0) (set! glyph (vector-ref glyph (+ 5 (modulo t l)))))
        (goto y x)
        ; 1st and 2nd glyph characters
        (set-color (glyph0bg glyph) (glyph0fg glyph)) (putc (glyph0ch glyph))
        (set-color (glyph1bg glyph) (glyph1fg glyph)) (putc (glyph1ch glyph))))
    ; Time of last viewport refresh used by the refresh thread to
    ; skip if an avatar movement has caused one within a set time
    ; Continuously redraw the viewport for cell animation.  Should be called in its own thread.
    (define (recenterReset y x)
       ; Reset next draw time to less than a second so the aniamtion loop triggers a recenterRedraw
      (set! NextDrawTime (+ 300 (utime)))
      (lock) ; This shouldn't be such an all encompasing lock.
      (set! mapCenterY y)
      (set! mapCenterX x)
      (set! winHeight Wheight) ; Adjust Viewport dimensions
      (set! winWidth (/ Wwidth 2))
      (set! my (- y (/ winHeight 2)))     ; Center Viewport around Avatar
      (set! mx (- x (/ winWidth 2)))
      (loop2 0 winHeight 0 winWidth (lambda (y x) ; Render glyphs in viewport
        (plot (canvasGlyph (+ my y) (+ mx x))
                      y (* x 2))))
      (unlock)) ; TODO This shouldn't be such an all encompasing lock.
    ; The cell position and viewport position (upper left corner) are on a torus
    ; coordinate system (wrap along the two dimensions).  I want to render to
    ; the screen cells plotted within the viewport .  I can do this by shifting
    ; the viewport to the origin and the cell by the same amount then checking if
    ; the cell is between the origin and lower right corner of the viewport.
    ;
    ; IE: (Cell % FieldWidth - Viewport % FieldWidth) % Fieldwidth < ViewportWidth
    ; But it would seem modulo distributes:  (a%m - b%m)%m == (a-b)%m%m == (a-b)%m
    ; so the actual computation is a bit simpler.  Smokin.
    (define (render gy gx)
      (let ((y (modulo (- gy my) canvasSize)) ; Normalize avatar position.
            (x (modulo (- gx mx) canvasSize)))
       (and (< y winHeight) (< x winWidth) (begin
         ((Terminal 'lock)) ; This shouldn't be such an all encompasing lock.
         (plot (canvasGlyph gy gx) y (* x 2))
         ((Terminal 'unlock)))))) ; This shouldn't be such an all encompasing lock.
    (define (animationLoop)
      (and VIEWPORTANIMATION
           (< NextDrawTime (utime))
           (recenterReset mapCenterY mapCenterX))
      (sleep 1000)
      (animationLoop))
    (define (main) ())
    ; MAIN
    ; Disable cursor in map window
    (cursor-visible #f)
    ;(WinChatDisplay "\r\nInitializing viewport...")
    (if (pair? ChildStack)
      ; ChildStack = ((child parameters) child-macro . reset of child stack)
      (apply (cadr ChildStack) self (append (car ChildStack) (cddr ChildStack)))
      self))
  ChildStack)) ; Viewport



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map_object
;;
;;  Composed of:  glyphs cells columns field canvas viewport IPC
;;
(define (Map ipc avatar size NOVIEWPORT . ChildStack)
  (define (self msg) (eval msg))
  (define initialMapSize (or NOVIEWPORT (min 25 (min (- (Terminal 'Theight) 1) (/ (Terminal 'Twidth) 2)))))
  ; Composition
  (define myEntityDB (EntityDB))
  (define myField (Field size))
  (define myCanvas (or NOVIEWPORT (Canvas myField myEntityDB)))
  (define myViewport (or NOVIEWPORT
                       (Viewport 0              (- (Terminal 'Twidth) (* initialMapSize 2))
                                 initialMapSize (* 2 initialMapSize)
                                 #x000f myCanvas)))
  ; Keep track of block coordinates (blockY . blockX) stored in the field.  Currently a 4x4 array
  (define blockDescSize (/ size MapBlockSize))
  (define blockDesc (make-vector-vector blockDescSize blockDescSize #f))
  (define (blockDescRegister my mx)
    (let ((by (/ my MapBlockSize))
          (bx (/ mx MapBlockSize)))
      (vector-vector-set! blockDesc (modulo by blockDescSize) (modulo bx blockDescSize) (cons by bx))))
  (define (inBlockInField? my mx)
    (let ((by (/ my MapBlockSize))
          (bx (/ mx MapBlockSize)))
      (equal? (cons by bx)
              (vector-vector-ref blockDesc (modulo by blockDescSize) (modulo bx blockDescSize)))))
  ; IPC aliases
  ;(define ipcRead ((ipc 'newReader)))
  (define ipcWrite (ipc 'qwrite))
  ; Entity DB aliases
  (define entityDBGet (myEntityDB 'get))
  ; Field DB aliases
  (define column      (myField 'column))
  (define firstCell   (myField 'firstRef))
  (define baseCell    (myField 'baseRef))
  (define fieldAdd    (myField 'add))
  (define fieldDelete (myField 'delete))
  ; Canvas aliases
  (define canvasHeight    (or NOVIEWPORT (myCanvas 'height)))
  (define canvasRender    (or NOVIEWPORT (myCanvas 'render)))
  (define canvasResetArray(or NOVIEWPORT (myCanvas 'resetArray)))
  ; Viewport/window aliases
  (define viewportRecenterReset(or NOVIEWPORT (myViewport 'recenterReset)))
  (define viewportRender     (or NOVIEWPORT (myViewport 'render)))
  (define toggleWindow       (or NOVIEWPORT (myViewport 'toggle)))
  (define ViewportSetColor   (or NOVIEWPORT (myViewport 'set-color)))
  (define ViewportPutc       (or NOVIEWPORT (myViewport 'putc)))
  (define (ViewportPuts . l) (or NOVIEWPORT (for-each (myViewport 'puts) l)))
  (define (ViewportDisplay . e) (or NOVIEWPORT (for-each (lambda (x) (for-each (myViewport 'puts) (display->strings x))) e)))
  ; Members
  (define DebugDumpFlag #f)
  (define circularize #t)
  (define (debugDumpMapInfoToggle) (set! DebugDumpFlag (not DebugDumpFlag )))
  (define (debugDumpInfo y x) ; Arguments specify the field column cells to dump
    ((myViewport 'set-color) 0 15)
    ((myViewport 'home))
    (let ((az (avatar 'z))
          (ay (avatar 'y))
          (ax (avatar 'x)))
      (ViewportDisplay "map("
         (hex az) " "
         (hex ay) " "
         (hex ax) ")"
         " field("
         (hex (/ ay size)) " " 
         (hex (/ ax size)) ")("
         (hex (/ (modulo ay size) MapBlockSize)) " " 
         (hex (/ (modulo ax size) MapBlockSize)) ")("
         (hex (modulo ay size)) " " 
         (hex (modulo ax size)) ")"
         " block("
         (hex (/ ay MapBlockSize)) " " 
         (hex (/ ax MapBlockSize)) ")("
         (hex (modulo ay MapBlockSize)) " " 
         (hex (modulo ax MapBlockSize)) ")"
         " C=" ceiling))
    ((myViewport 'goto) 0 0)
    (let ~ ((z 11))
     (ViewportPuts "\r\n")
     (let ((c (firstCell z y x)))
      (if (eqv? cellAIR c)
       (begin (ViewportSetColor 0 8)
              (ViewportPuts "()   "))
       (begin (set! c (if (< CellMax c) (((myEntityDB 'get) c) 'glyph)
                                        (cellGlyph (cellRef c)))) ; Dump the glyph
              (ViewportSetColor (glyph0bg c) (glyph0fg c))
              (ViewportPutc (glyph0ch c))
              (ViewportSetColor (glyph1bg c) (glyph1fg c))
              (ViewportPutc (glyph1ch c))
              (ViewportSetColor 0 7)
              (set! c (baseCell z y x)) ; Display base cell's hex value.
              (if (and (<= 0 c) (< c CellMax))
                (begin
                  (if (< c 256) (ViewportPuts "0"))
                  (if (< c 16) (ViewportPuts "0"))
                  (ViewportPuts (hex c)))
                (ViewportPuts "   ") ))))
     (if (> z -6) (~ (- z 1))))) ; debugDumpInfo
  (define (circularizeToggle) (set! circularize (not circularize)))
  ; Drop a cell on the map
  (define (mapDropCell y x cell)
    (if (inBlockInField? y x) (begin
      (let ((z (+ 1 (fieldTopHeight 100 y x))))
       ((myField 'fieldSet!) z y x cell)
       (or NOVIEWPORT
         (begin
           (canvasRender 100 y x)
           (viewportRender y x)))))))
  ; Set map cell and force rendering of cell through entire pipeline.
  (define (setCell z y x cell)
    (if (inBlockInField? y x) (begin
      ((myField 'fieldSet!) z y x cell)
      (or NOVIEWPORT
        (begin
          (canvasRender 100 y x)
          (viewportRender y x))))))
  ; Delete a cell from the field.  Update canvas if needed.
  (define (delCell cell z y x)
    (if (inBlockInField? y x) (begin
      (fieldDelete z y x cell)
      (or NOVIEWPORT 
          (if (>= z (canvasHeight y x))
            (begin
              (canvasRender ceiling y x)
              (viewportRender y x)))))))
  ; Add a cell to the field.  Update canvas if needed.
  (define (addCell cell z y x)
    (if (inBlockInField? y x) (begin
      (fieldAdd z y x cell)
      (or NOVIEWPORT 
          (if (>= z (canvasHeight y x)) (begin
            (canvasRender ceiling y x)
            (viewportRender y x)))))))
  (define (moveCell cell zo yo xo z y x)
    ; Old location removal
    (delCell cell zo yo xo)
    ; New location added
    (addCell cell z y x))
  (define (delEntitySprite dna zo yo xo)
    (letrec ((entity ((myEntityDB 'get) dna))
             (sprite (entity 'sprite))
             (h (sprite 'height))
             (w (sprite 'width))
             (coordinates (sprite 'coordinates))) ; List of the sprites glyph relative coordinates
      ; Second update canvas and viewport
      (each-for coordinates
        (lambda (c)
          (if c (let ((m (car c))
                      (n (cdr c)))
            ; First update field
            (fieldDelete zo (+ m yo) (+ n xo) dna)
            ; Render old deleted location
            (or NOVIEWPORT
                (if (>= zo (canvasHeight (+ m yo) (+ n xo)))
                 (begin
                   (canvasRender ceiling (+ m yo) (+ n xo))
                   (viewportRender (+ m yo) (+ n xo))))))))))) ; Don't render cell if viewport to be reset
  (define (moveEntitySprite dna zo yo xo z y x centerMap)
    (if (eq? MAPSCROLL 'never) (set! centerMap #f)) ; Global toggle to disable map scrolling
    (if (inBlockInField? y x) ; TODO this should allow one or the other old/new coordinate.  Also moveCell moveEntity seem to be overlapping in functionality.
    (letrec ((entity ((myEntityDB 'get) dna))
             (sprite (entity 'sprite))
             (h (sprite 'height))
             (w (sprite 'width))
             (coordinates (sprite 'coordinates))) ; List of the sprites glyph coordinates
      ; TODO can I combine the two loops?  What was my original thinking?
      ; Update field
      (each-for coordinates
        (lambda (c) (if c (let ((m (car c))
                                (n (cdr c)))
                      (fieldDelete zo (+ m yo) (+ n xo) dna) ; Old
                      (fieldAdd z (+ m y) (+ n x) dna))))) ; New
      ; Update canvas and viewport
      (or NOVIEWPORT
        (begin
          (each-for coordinates
            (lambda (c)
              (if c (let ((m (car c))
                          (n (cdr c)))
                ; Render old deleted location
                (if (>= zo (canvasHeight (+ m yo) (+ n xo))) (begin
                  (canvasRender ceiling (+ m yo) (+ n xo))
                  (or centerMap (viewportRender (+ m yo) (+ n xo))))) ; Don't render cell if viewport to be reset
                ; Render new added location
                (if (>= z (canvasHeight (+ m y) (+ n x))) (begin
                  (canvasRender ceiling (+ m y) (+ n x))
                  (or centerMap (viewportRender (+ m y) (+ n x))))))))) ; Don't render cell if vewport to be reset
          ; If the map needs to be recentered, then this is when the viewport is finally updated.
          (if centerMap (viewportRecenterReset y x)))))))
  ; Given a cell index or entity DNA value, move it in the field, canvas and viewport.
  (define moveObject
    (let ((MAP-MOVE-CELL-SEMAPHORE (open-semaphore 1)))
    (lambda (cell zo yo xo z y x centerMap)
      (semaphore-down MAP-MOVE-CELL-SEMAPHORE)
      (if (cellValidIndex? cell)
        (moveCell         cell zo yo xo z y x) ; Move cell
        (moveEntitySprite cell zo yo xo z y x centerMap)) ; Move entity
      (semaphore-up MAP-MOVE-CELL-SEMAPHORE))))
  (define (moveEntity entity z y x)
    (let ((oz (entity 'z))
          (oy (entity 'y))
          (ox (entity 'x)))
     ((entity 'setLoc) z y x)
     (moveObject (entity 'dna)
               oz oy ox
               z y x
               (and (not NOVIEWPORT)
                    (eq? (entity 'dna) (avatar 'dna)) ; Bool expr to determine if the map should be centered and redrawn
                    (or (eq? MAPSCROLL 'always)
                        (and (eq? MAPSCROLL 'edge)
                             (< (- (/ (myViewport 'Wheight) 2) 2)
                                (distance (list 0 y x)
                                          (list 0 (+ (myViewport 'my) (/ (myViewport 'winHeight) 2)) (+ (myViewport 'mx) (/ (myViewport 'winWidth) 2)))))))))))
  (define ceiling 100)
  (define (setCeiling z) (set! ceiling z))
  (define (walkDetails entity)
    ; Update avatar locally in the field/canvas/viewport and via IPC
    (apply moveEntity entity ((entity 'gpsFace)))
    (ipcWrite (list 'move (entity 'dna) (entity 'z) (entity 'y) (entity 'x)))
    ; Special case to handle cells that change the Avatar's sprite
    (letrec ((cellNum (apply baseCell ((entity 'gps))))
             (baseSym (cellSymbol (cellRef cellNum))))
      (cond ((and (<= 512 cellNum) (<= cellNum 612)) (WinChatDisplay "\r\n" (twoLetterDefinition baseSym)))
            ((eq? baseSym 'sprite0) (createSprite 0))
            ((eq? baseSym 'sprite1) (createSprite 1))
            ((eq? baseSym 'sprite2) (createSprite 2))))
    ; If ceiling changes, repaint canvas using new ceiling height
    (or NOVIEWPORT 
        (begin
          (let ((oldCeiling ceiling))
            (setCeiling (- (apply (myField 'ceiling) ((entity 'gps))) 1))
            (if (!= oldCeiling ceiling)
              (thread (canvasResetArray ceiling))))
          (if DebugDumpFlag (debugDumpInfo (entity 'y) (entity 'x))))))
  ; Make map window circular
  (define (maskCorners . val)
   (or NOVIEWPORT
    (begin
     (set! val (not (if (pair? val) (car val) circularize)))
     ;(set! val (not (null? val))) ; Default to disabling circular corners of map.
     (let ~ ((y 0)(x 0))
      (if (< y (/ (myViewport 'Wheight) 2))
      (if (= x (/ (myViewport 'Wwidth) 2)) (~ (+ y 1) 0)
       (begin
        (if (> (sqrt (+ (* 4 (^2 (- y (/ (myViewport 'Wheight) 2))))
                        (^2 (- x  (/ (myViewport 'Wwidth) 2)))))
               (+ 0 (myViewport 'Wheight)))
            (begin
              ((myViewport 'alpha) y x val)
              ((myViewport 'alpha) y (- (myViewport 'Wwidth) x 1) val)
              ((myViewport 'alpha) (- (myViewport 'Wheight) y 1) x val)
              ((myViewport 'alpha) (- (myViewport 'Wheight) y 1)
                               (- (myViewport 'Wwidth) x 1) val)
              ))
        (~ y (+ x 1)))))))))
  ; Viewport/window resizing
  (define viewportResizeClickTime (utime)) ; Double click 1/16 sec.
  (define (bigger)
   (or NOVIEWPORT 
    (if (< (myViewport 'Wheight) (Terminal 'Theight)) (begin
     ((Terminal 'lock))
     ((myViewport 'home))
     (if (< (utime) viewportResizeClickTime)
       ((myViewport 'moveresize) ; Full resize
          0 (- (Terminal 'Twidth) (myViewport 'Wwidth) 2)
                (min (/ (Terminal 'Twidth) 2) (- (Terminal 'Theight) 1))
          (* 2  (min (/ (Terminal 'Twidth) 2) (- (Terminal 'Theight) 1)))))
       ((myViewport 'moveresize) ; Resize by one
          0 (- (Terminal 'Twidth) (myViewport 'Wwidth) 2)
          (+ 1 (myViewport 'Wheight))
          (+ 2 (myViewport 'Wwidth)))
     (maskCorners)
     ((Terminal 'unlock))
     (viewportRecenterReset (avatar 'y) (avatar 'x))
     (set! viewportResizeClickTime (+ 125 (utime)))))))
  (define (smaller)
   (or NOVIEWPORT 
    (if (< 5 (myViewport 'Wheight)) (begin
     ((Terminal 'lock))
     ((myViewport 'home))
     (if (< (utime) viewportResizeClickTime)
       ((myViewport 'moveresize) ; Shrink by 10
          0 (- (Terminal 'Twidth) (myViewport 'Wwidth) -10)
          (+ -5 (myViewport 'Wheight))
          (+ -10 (myViewport 'Wwidth)))
       ((myViewport 'moveresize) ; Resize by one
          0 (- (Terminal 'Twidth) (myViewport 'Wwidth) -2)
          (+ -1 (myViewport 'Wheight))
          (+ -2 (myViewport 'Wwidth))))
     (maskCorners)
     ((Terminal 'unlock))
     (viewportRecenterReset (avatar 'y) (avatar 'x))
     (set! viewportResizeClickTime (+ 125 (utime)))))))
  ; Start viewport animation loop
  (define (startAnimationLoop)
    (or NOVIEWPORT (thread ((myViewport 'animationLoop)))))
  (define (dieIPC dnaIPC)
     (let ((entity (entityDBGet dnaIPC))
           (thisIsMe (= (avatar 'dna) dnaIPC)))
      (if entity
        (begin ; Ignore unknown entities
            ; Remove from here
            (delEntitySprite dnaIPC (entity 'z) (entity 'y) (entity 'x)) ; Remove it first from the map
            (or NOVIEWPORT ; TODO doesn't delEntitySprite rerender the map so the following block is redundant?
              (if (>= (entity 'z) (canvasHeight (entity 'y) (entity 'x))) (begin
                (canvasRender 100 (entity 'y) (entity 'x))
                (or thisIsMe (viewportRender (entity 'y) (entity 'x))))))
            ((myEntityDB 'del) entity)))))
  ; The 2d vector of columns will most likely come from a map agent.
  ; The map block coordinate and map block size is also passed.
  (define (updateColumnsIPC dna my mx blockSize cellAry) ; Called from map agent via IPC
    (let ((fy (modulo my size))
          (fx (modulo mx size)))
    (if (= dna (avatar 'dna)) (begin ; Make sure map agent sent specifically to me
      ; Update the field block
      ((myField 'updateColumns) fy fx blockSize cellAry)
      ; Register the block
      (blockDescRegister my mx)
      ; The field has just been updated with new columns so all entities
      ; in this range need to be added into the field columns TODO slow
      (for-each
        (lambda (e) (let ((ey (e 'y))
                          (ex (e 'x)))
          (if (and (<= my ey) (< ey (+ my blockSize))
                   (<= mx ex) (< ex (+ mx blockSize)))
            (moveObject (e 'dna)  (e 'z) ey ex  (e 'z) ey ex  #f))))
        ((myEntityDB 'getAll)))
      ; Render map block
      (or NOVIEWPORT
        (loop2 fy (+ fy blockSize)
               fx (+ fx blockSize)
               (lambda (y x) (canvasRender 100 y x))))))))
    ; Update one or more of an entity's attribute: dna port name glyph z y x
  (define (IPCentity dna . args)
    (let ((entity ((myEntityDB 'get) dna)))
      (if entity
        (begin ; Modify entity attributes
          (delEntitySprite dna (entity 'z) (entity 'y) (entity 'x)) ; Remove it first from the map
          (apply (myEntityDB 'set) dna args)
          (moveObject (entity 'dna) (entity 'z) (entity 'y) (entity 'x)
                                    (entity 'z) (entity 'y) (entity 'x) #f)
          (or NOVIEWPORT
            (begin
              (canvasRender ceiling (entity 'y) (entity 'x))
              (viewportRender (entity 'y) (entity 'x)))))
        (begin ; Create new entity with all args
          (set! entity (apply (myEntityDB 'set) dna args))
          (moveObject (entity 'dna) (entity 'z) (entity 'y) (entity 'x)
                                    (entity 'z) (entity 'y) (entity 'x) #f)))
      (if (= dna 17749) (set! PortMapAgent (entity 'port))))) ; The map agent's DNA number
  (define (moveIPC dna z y x)
   (or (= dna (avatar 'dna)) ; Skip if this is me since map rendering is handled during movement handling
       (let ((entity ((myEntityDB 'get) dna)))
         (if entity (moveEntity entity z y x)))))
  ; MAIN
  ;(WinChatDisplay "\r\nInitializing map...")
  ((myEntityDB 'add) avatar)
  (or NOVIEWPORT
    (begin
      (maskCorners)
      (viewportRecenterReset (avatar 'y) (avatar 'x))
      (startAnimationLoop)))
  (if (pair? ChildStack)
    (apply (cadr ChildStack) self (append (car ChildStack) (cddr ChildStack)))
    self)) ; Map



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avatar_object
;;
;; The user's avatar.  An extended entity object that includes positioning
;; and directional observation vectors
;;
;; TODO does the private port make sense when there will be more than one entity/IPCreader?
;;
(define (Avatar name z y x ipc NOVIEWPORT . ChildStack)
 (apply Entity ; Inherits Entity
  ; Args to entity class
  (random) (if ipc (ipc 'PrivatePort) 0) name z y x
  ; Args to child class
  (list ipc NOVIEWPORT)
  ; Child class definition
  (macro (parent ipc NOVIEWPORT . ChildStack) ; Child
    (define (self msg) (eval msg))
    (define (info) (list 'Avatar name z y x 'hasChild (pair? ChildStack)))
    (define fieldSize 128)
    ; Composition
    (define myMap (Map ipc self fieldSize NOVIEWPORT))
    ; Members
    (define alive #t) ; Only (die) sould set this to false
    (define Stop #t) ; Used by action macros
    (define speakLevel 20) ; Whisper=2  Talk=20 Scream=500 
    (define climb #f)
    (define cell 19) ; TODO replace with a generalize item container
    (define ipcRead (if ipc ((ipc 'newReader)) #f)) ; This needs to be destroyed.
    (define ipcWrite (if ipc (ipc 'qwrite) #f))
    (define (stop) (set! Stop #t))
    ; Kill this avatar and IPC thread after announcing death
    (define (die)
      (stop) ; Halt any currently running macro
      (or QUIETLOGIN (saySystem name " exits"))
      (ipcWrite `(die ,dna)) ; Announce that I'm leaving to other avatars
      (set! alive #f)) ; This will stop the thread from reading the IPC
    (define (jump z y x)
      (setLoc z y x)
      (mapWalkDetails self))
    (define walkSemaphore (open-semaphore 1)) ; This needs to be destroyed
    (define mapWalkDetails (myMap 'walkDetails))
    (define (walk dir)
      (semaphore-down walkSemaphore)
        ; Consider cell I'm walking into.  If cell is entity push it.
        ; Otherwise move to facing cell or on top of obstructing cell.
        (look dir) ; Look where I want to walk
        (face dir) ; Face where I want to walk.  Used for actual motion.
        (let ((nextCell (apply (myMap 'firstCell) (gpsFace))))
          (cond ((< CellMax nextCell)
                  ; Case 1 push entity
                  (ipcWrite `(force ,@(gpsFace) ,dir 10)))
                ((or EDIT (not (cellSolid? (cellRef nextCell))))
                  ; Case 2 walk normally
                  (mapWalkDetails self))
                (climb
                  ; Case 3 step up
                  (face dir 1) ; Peek at the cell above the one in front of me
                  (set! nextCell (apply (myMap 'baseCell) (gpsFace)))
                  (or (cellSolid? (cellRef nextCell))
                      (mapWalkDetails self)))))
        ; Gravity
        (or EDIT (fall))
      (semaphore-up walkSemaphore)) ; walk
    ; Fall down one cell if a non-entity and non-solid cell below me
    (define (fall)
      (face 8) ; Look down
      (let ((nextCell (apply (myMap 'firstCell) (gpsFace))))
        (if (= nextCell cellAIR) (mapWalkDetails self))))
    (define (setSpeakLevel level)
      (set! speakLevel level))
    (define (speak talkInput . level)
      (ipcWrite (list 'voice dna
                      (if (null? level) speakLevel (car level))
                      (apply string (display->strings talkInput)))))
    (define (lookHere) (apply (avatarMap 'baseCell) ((avatar 'gps))))
    (define (lookAt) (apply (avatarMap 'baseCell) ((avatar 'gpsLook))))
    (define (setIPCvoice f) (set! IPCvoice f))
    (define (IPCvoice dna level text)
     (or NOVIEWPORT
       (if (= dna 0) ; System messages
         (begin
           (WinChatDisplay "\r\n")
           (WinChatSetColor 0 9) (WinChatDisplay "W")
           (WinChatSetColor 0 11) (WinChatDisplay "O")
           (WinChatSetColor 0 10) (WinChatDisplay "R")
           (WinChatSetColor 0 12) (WinChatDisplay "L")
           (WinChatSetColor 0 13) (WinChatDisplay "D")
           (WinChatSetColor 0 8) (WinChatDisplay VOICEDELIMETER)
           (WinChatSetColor 0 7) (WinChatDisplay text))
         (letrec ((entity ((myMap 'entityDBGet) dna))
                  (dist (if entity (distance ((entity 'gps)) ((avatar 'gps))))))
           (if entity
             (if (< dist level) ; Hear only things within the distance level
               (let ((glyph (entity 'glyph))
                     (name (entity 'name)))
                 (if (<= level WhisperTalkThreshold) (set! name (string "(" name ")"))
                  (if (<= TalkScreamThreshold level) (set! name (string "{" name "}"))))
                 ; Color of the name and text based on the entity's glyph
                 (WinChatSetColor (glyph0bg glyph) (glyph0fg glyph))
                 (WinChatDisplay "\r\n" name VOICEDELIMETER)
                 (WinChatSetColor (glyph1bg glyph) (glyph1fg glyph))
                 (WinChatDisplay text)))
             (begin
               (WinChatSetColor 0 7)
               (WinChatDisplay "\r\n???" VOICEDELIMETER text)))))
       (if (and (!= dna (self 'dna)) (eqv? text "unatco"))
           (speak "no Savage"))))
    (define (IPCforce fz fy fx dir mag)
     (and (= fz z) (= fy y) (= fx x) (walk dir)))
    (define (IPCact fz fy fx dir mag)
     (and (= fz z) (= fy y) (= fx x) (begin
       (speak (string "I'm " name) 2)))) ; Whisper
    (define (IPCwho . dnaRequestor) ; TODO handle explicit request from specified peer
      (ipcWrite
        `(entity ,dna ,port ,name ,(gps)))
      (ipcWrite `(entity ,dna ,glyph)))
    (define setField ((myMap 'myField) 'setField)) ; TODO debugging
    (define (getField) ((myMap 'myField) 'field))
    ; The soul of this avatar.  For now an IPC message handler which loops
    ; as long as local 'alive is #t and global 'SHUTDOWN is false
    (define IPCHandlerLoop (macro () ; TODO new framework which will fire up this thread in the child's environment
      (if ipc (thread
      (let ~ ((e (ipcRead)))
        (if (or alive SHUTDOWN)
          (begin
            (if (pair? e) (let ((a (car e)) (d (cdr e)))
              (cond ((eq? a 'voice) (apply IPCvoice d)) ; Avatar messages
                    ((eq? a 'force) (apply IPCforce d))
                    ((eq? a 'act) (apply IPCact d))
                    ((eq? a 'who) (apply IPCwho d))
                    ((eq? a 'mapSetCell) (apply (myMap 'setCell) d))
                    ((eq? a 'die) (apply (myMap 'dieIPC) d)) ; Map messages
                    ((eq? a 'mapUpdateColumns) (apply (myMap 'updateColumnsIPC) d))
                    ((eq? a 'entity) (apply (myMap 'IPCentity) d))
                    ((eq? a 'move) (apply (myMap 'moveIPC) d)))))
            (and SHUTDOWN alive (die)) ; If shutdown signaled quasi-kill myself but continue to handle msgs
            (~ (ipcRead)))
          (begin ; Shutdown avatar
            ((ipc 'delReader) ipcRead)
            (close-semaphore walkSemaphore))))))))
    ; MAIN
    (define main (macro ()
      (IPCHandlerLoop)
      (ipcWrite '(who)))) ; Ask the IPC for a rollcall TODO should existing avatars notice a new entity has appeard and reveal themselves automatically?  Maybe an entity doesn't care about other people.
    ; WOOEE
    (if (pair? ChildStack)
      ; childstack = ((child parameters) child-macro . reset of child stack)
      (apply (cadr ChildStack) self (append (car ChildStack) (cddr ChildStack)))
      (begin
        (let ~ ((obj self))
          (if (obj 'parent) (~ (obj 'parent)))
          ((obj 'main)))
        self)))
  ChildStack)) ; Avatar

(define (createSprite x)
 (IpcWrite (list 'entity (avatar 'dna)
    (if (= x 0)
      `(Sprite 1 1 ,(vector (avatar 'glyph)))
    (if (= x 1)

      '(Sprite 3 2 #(#(0 15 #\  0 15 #\() #(0 15 #\) 0 15 #\ )  ; ()
                     #(0 15 #\- 0 15 #\[) #(0 15 #\] 0 15 #\-)  ;-[]-
                     #(0 15 #\_ 0 15 #\/) #(0 15 #\\ 0 15 #\_)));_/\_
    (if (= x 2)
      '(Sprite 7 1 #(#(0 15 #\| 0 15 #\|)
                     #(0 15 #\| 0 15 #\|)
                     #(0 15 #\| 0 15 #\|)
                     #(0 15 #\| 0 15 #\|)
                     #(0 15 #\| 0 15 #\|)
                     #(0 15 #\| 0 15 #\|)
                     #(0 15 #\| 0 15 #\|)))))))))
