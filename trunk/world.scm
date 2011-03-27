;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The World Client
;;
;;   Terminal_and_Windows
;;   Cells_object
;;   Column_object
;;   Field_object
;;   Canvas_object
;;   Viewport_object
;;    Entity_DB
;;   Map_object
;;    Avatar_object
;;   Window_functions_and_initialization
;;   Button_commands
;;   Buttons
;;   Typing_and_talking
;;    Prototypes_and_fun_things
;;   Genesis

(load "ipc.scm")
(load "window.scm")
(load "entity.scm") ; Glyph Sprite Entity EntityDb objects

(define SHUTDOWN #f) ; Signals to avatar's that the process is going to shutdown
(define QUIETLOGIN (and (< 2 (vector-length argv)) (eqv? "silent" (vector-ref argv 2))))
(define VIEWPORTANIMATION #t)
(define MAPSCROLL 'always) ; always edge never
(define KITTEHBRAIN  #f)
(define VOICEDELIMETER " ")
(define ActivityTime (time))
(define MapBlockSize 32) ; Size of each map file cell in the World map.
(define PortMapAgent #f)
(define SHOWBUTTONS #f)
(define EDIT #f)
(define WhisperTalkThreshold 4)
(define TalkScreamThreshold  64)


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
(define WinConsole ((Terminal 'WindowNew)
  (- (Terminal 'Theight) 14) 0
  13  (Terminal 'Twidth)
  #x0002))
(define WinConsolePuts (WinConsole 'puts))
(define (WinConsoleDisplay . l)
  (for-each
    (lambda (x)
     (if (not (and (pair? x) (eq? (car x) 'mapUpdateColumns)))
         (for-each WinConsolePuts (display->strings x))))
    l))
(define (WinConsoleWrite . e) (for-each (lambda (x) (for-each WinConsolePuts (write->strings x))) e))
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
  (define (setField newField)
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
(define (Viewport y x h w clr . childArgs)
 ((((Terminal 'WindowNew) y x h w clr) 'inherit) childArgs ; Parent and args to child
  (macro (aCanvas) ; Child
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
    ; MAIN
    ; Disable cursor in map window
    (cursor-visible #f)
    ;(WinChatDisplay "\r\nInitializing viewport...")
    self))) ; Viewport



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map_object
;;
;;  Composed of:  glyphs cells columns field canvas viewport IPC
;;
(define initialMapSize (min 25 (min (- (Terminal 'Theight) 1) (/ (Terminal 'Twidth) 2))))

(define (Map ipc avatar size . args)
  (define (self msg) (eval msg))
  (define NOVIEWPORT (pair? (memq 'NOVIEWPORT args)))
  ; Composition
  (define myEntityDB (EntityDB))
  (define myField (Field size))
  (define myCanvas (or NOVIEWPORT (Canvas myField myEntityDB)))
  (define myViewport (or NOVIEWPORT
                       (Viewport 0              (- (Terminal 'Twidth) (* initialMapSize 2))
                                 initialMapSize (* 2 initialMapSize)
                                 #x000f myCanvas)))
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
  (define (debugDumpMapInfoToggle) (set! DebugDumpFlag (not DebugDumpFlag )))
  (define (debugDumpInfo y x) ; Arguments specify the field column cells to dump
    ((myViewport 'set-color) 0 15)
    ((myViewport 'home))
    (let ((az (avatar 'z))
          (ay (avatar 'y))
          (ax (avatar 'x)))
      (ViewportDisplay "map("
         (number->string az) " "
         (number->string ay) " "
         (number->string ax) ")"
         " field("
         (number->string (/ ay size)) " " 
         (number->string (/ ax size)) ")("
         (number->string (/ (modulo ay size) MapBlockSize)) " " 
         (number->string (/ (modulo ax size) MapBlockSize)) ")("
         (number->string (modulo ay size)) " " 
         (number->string (modulo ax size)) ")"
         " block("
         (number->string (/ ay MapBlockSize)) " " 
         (number->string (/ ax MapBlockSize)) ")("
         (number->string (modulo ay MapBlockSize)) " " 
         (number->string (modulo ax MapBlockSize)) ")"
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
                  (ViewportPuts (number->string c 16)))
                (ViewportPuts "   ") ))))
     (if (> z -6) (~ (- z 1))))) ; debugDumpInfo
  ; Drop a cell on the map
  (define (mapDropCell y x cell)
   (let ((z (+ 1 (fieldTopHeight 100 y x))))
    ((myField 'fieldSet!) z y x cell)
    (or NOVIEWPORT
      (begin
        (canvasRender 100 y x)
        (viewportRender y x)))))
  ; Set map cell and force rendering of cell through entire pipeline.
  (define (setCell z y x cell)
    ((myField 'fieldSet!) z y x cell)
    (or NOVIEWPORT
      (begin
        (canvasRender 100 y x)
        (viewportRender y x))))
  ; Delete a cell from the field.  Update canvas if needed.
  (define (delCell cell z y x)
    (fieldDelete z y x cell)
    (or NOVIEWPORT 
        (if (>= z (canvasHeight y x))
          (begin
            (canvasRender ceiling y x)
            (viewportRender y x)))))
  ; Add a cell to the field.  Update canvas if needed.
  (define (addCell cell z y x)
    (fieldAdd z y x cell)
    (or NOVIEWPORT 
        (if (>= z (canvasHeight y x)) (begin
          (canvasRender ceiling y x)
          (viewportRender y x)))))
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
          (if centerMap (viewportRecenterReset y x))))))
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
  (define (circularize . val)
   (or NOVIEWPORT
     (begin
     (set! val (not (null? val))) ; Default to disabling circular corners of map.
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
     (circularize)
     ((Terminal 'unlock))
     (viewportRecenterReset (avatar 'y) (avatar 'x))
     (set! viewportResizeClickTime (+ 125 (utime)))))))
  (define (smaller)
   (or NOVIEWPORT 
    (if (< 5 (myViewport 'Wheight)) (begin
     ((Terminal 'lock))
     ((myViewport 'home))
     (if (< (utime) viewportResizeClickTime)
       ((myViewport 'moveresize) ; Full resize
          0 (- (Terminal 'Twidth) (myViewport 'Wwidth) -10)
          (+ -5 (myViewport 'Wheight))
          (+ -10 (myViewport 'Wwidth)))
       ((myViewport 'moveresize) ; Resize by one
          0 (- (Terminal 'Twidth) (myViewport 'Wwidth) -2)
          (+ -1 (myViewport 'Wheight))
          (+ -2 (myViewport 'Wwidth))))
     (circularize)
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
  (define (updateColumnsIPC by bx blockSize cellAry) ; Called from map agent via IPC
    (let ((fieldy (modulo by size))
          (fieldx (modulo bx size)))
      ; Update the field block
      ((myField 'updateColumns) fieldy fieldx blockSize cellAry)
      ; The field has just been updated with new columns so all entities
      ; in this range need to be added into the field columns
      (for-each
        (lambda (e) (let ((y (e 'y))
                          (x (e 'x)))
          (if (and (<= by y) (< y (+ by blockSize))
                   (<= bx x) (< x (+ bx blockSize)))
            (moveObject (e 'dna)  (e 'z) (e 'y) (e 'x)  (e 'z) (e 'y) (e 'x)  #f))))
        ((myEntityDB 'getAll)))
      ; Render map block
      (or NOVIEWPORT
        (loop2 fieldy (+ fieldy blockSize)
               fieldx (+ fieldx blockSize)
               (lambda (y x) (canvasRender 100 y x))))))
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
      (circularize)
      (viewportRecenterReset (avatar 'y) (avatar 'x))
      (startAnimationLoop)))
  self) ; Map



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avatar_object
;;
;; The user's avatar.  An extended entity object that includes positioning
;; and directional observation vectors
;;
;; TODO does the private port make sense when there will be more than one entity/IPCreader?
;;
(define (Avatar name z y x ipc . args) ; Inherits Entity
 (((Entity (random) (if ipc (ipc 'PrivatePort) 0) name z y x) 'inherit)
  (cons ipc args) ; Args to child class
  (macro (ipc . args) ; Child
    (define (self msg) (eval msg))
    (define NOVIEWPORT (pair? (memq 'NOVIEWPORT args)))
    (define fieldSize 128)
    ; Composition
    (define myMap (apply Map ipc self fieldSize args))
    ; Members
    (define alive #t)
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
    (define (IPCwho . dnaRequestor) ; TODO handle explicit request from specified peer
      (ipcWrite
        `(entity ,dna ,port ,name ,(gps)))
      (ipcWrite `(entity ,dna ,glyph)))
    ; TODO debugging
    (define setField ((myMap 'myField) 'setField))
    (define (getField) ((myMap 'myField) 'field))
    ; MAIN
    ; The soul of this avatar.  For now an IPC message handler which loops
    ; as long as local 'alive is #t and global 'SHUTDOWN is false
    (if ipc (thread (let ~ ()
      (let ((e (ipcRead)))
        (if (or alive SHUTDOWN)
          (begin
            (if (pair? e) (let ((a (car e)) (d (cdr e)))
              (cond ((eq? a 'voice) (apply IPCvoice d)) ; Avatar messages
                    ((eq? a 'force) (apply IPCforce d))
                    ((eq? a 'who) (apply IPCwho d))
                    ((eq? a 'die) (apply (myMap 'dieIPC) d)) ; Map messages
                    ((eq? a 'move) (apply (myMap 'moveIPC) d))
                    ((eq? a 'mapUpdateColumns) (apply (myMap 'updateColumnsIPC) d))
                    ((eq? a 'mapSetCell) (apply (myMap 'setCell) d))
                    ((eq? a 'entity) (apply (myMap 'IPCentity) d)))))
            (and SHUTDOWN alive (die)) ; If shutdown signaled quasi-kill myself but continue to handle msgs
            (~))
          (begin ; Shutdown avatar
            ((ipc 'delReader) ipcRead)
            (close-semaphore walkSemaphore)))))))
    (ipcWrite '(who)) ; Ask the IPC for a rollcall
    self))) ; Avatar


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
(WinPaletteGoto 0 0) (loop 8  (lambda (i) (WinPaletteColor i 0) (WinPaletteDisplay #\ )))
(loop 12 (lambda (i) (WinPaletteColor (+ 232 i) 0) (WinPaletteDisplay #\ )))
(WinPaletteGoto 1 0) (loop 8  (lambda (i) (WinPaletteColor (+ 8 i) 0) (WinPaletteDisplay #\ )))
(loop 12 (lambda (i) (WinPaletteColor (+ 232 12 i) 0) (WinPaletteDisplay #\ )))
(WinPaletteDisplay "\r\n")
(loop 216 (lambda (i)
  (WinPaletteColor (+ 16 i) 0)
  (WinPaletteDisplay #\ )))


; Screen redraw and signal handler
(define handlerCount 0)
(define (handleTerminalResize . forcedSize)
  ; TODO Temporary assertion
  (if (!= handlerCount 0) (WinChatDisplay "\r\nWARNING: handleTerminalResize is not reentrant"))
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
     (begin
       (set! count (+ count 1))
       (semaphore-up sem)) ; Done
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Button_commands
;;
; Toggle the help window
(define (help)
 ((WinHelp 'toggle))
 ((WinHelpBorder 'toggle)))


(define (winMapUp)   ((avatarViewport 'move) (+ -1 (avatarViewport 'Y0))      (avatarViewport 'X0)))
(define (winMapDown) ((avatarViewport 'move) (+  1 (avatarViewport 'Y0))      (avatarViewport 'X0)))
(define (winMapLeft) ((avatarViewport 'move)       (avatarViewport 'Y0) (+ -1 (avatarViewport 'X0))))
(define (winMapRight)((avatarViewport 'move)       (avatarViewport 'Y0) (+  1 (avatarViewport 'X0))))

(define (walk d) ((avatar 'walk) d))

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

;;;;;;;;;;;;;;;;;;;;;;;;
; Avatar_color_chooser
(define eventQueue (QueueCreate))
(define LastColors (make-list 32 0))
(define CursorYX (cons 0 0))

(mouseQueueRegister WinPalette eventQueue)

; Called by the keyboard and mouse handler to
; update the cursor in the palette window.
(define (updatePaletteCursor y x)
  (let ((oy (car CursorYX))
        (ox (cdr CursorYX)))
    (WinPaletteColor (/ ((WinPalette 'getColor) oy ox) 256) 0)
    (WinPaletteGoto oy ox)
    (WinPaletteDisplay #\ )
    (WinPaletteColor (/ ((WinPalette 'getColor) y x) 256) 0)
    (WinPaletteGoto y x)
    (WinPaletteDisplay #\X))
    (set! CursorYX (cons y x)))

(define (keyColorsAction c)
 (if (pair? (memv c (list CHAR-ESC TAB #\C #\c #\q #\Q)))
   #f ; Return done.  Everything else returns true signalling we want to keep reading the keyboard
 (if (pair? (memv c (list RETURN NEWLINE SPACE)))
   (begin
     (mouseColorsActionHandler 'mouse0 (car CursorYX) (cdr CursorYX))
     #t)
 (let ((y (car CursorYX))
       (x (cdr CursorYX)))
   (if (eq? c #\j)
     (set! y (modulo (+ y 1)  (- (WinPalette 'Wheight) 1)))
   (if (eq? c #\k)
     (set! y (modulo (- y 1)  (- (WinPalette 'Wheight) 1)))
   (if (eq? c #\h)
     (set! x (modulo (- x 1)  (WinPalette 'Wwidth)))
   (if (eq? c #\l)
     (set! x (modulo (+ x 1)  (WinPalette 'Wwidth)))
   (if (eq? c #\K)
     ((WinPalette 'move) (+ -1 (WinPalette 'Y0))      (WinPalette 'X0))
   (if (eq? c #\J)
     ((WinPalette 'move) (+  1 (WinPalette 'Y0))      (WinPalette 'X0))
   (if (eq? c #\H)
     ((WinPalette 'move)       (WinPalette 'Y0) (+ -1 (WinPalette 'X0)))
   (if (eq? c #\L)
     ((WinPalette 'move)       (WinPalette 'Y0) (+  1 (WinPalette 'X0)))))))))))
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
  ((WinPalette 'toggle))
  (keyQueueStackRegister eventQueue)
  (let ~ ()
    (let ((e (QueueGet eventQueue)))
      (if (pair? e)
        (begin
           (apply mouseColorsActionHandler e)
           (~))
        (if (keyColorsAction e)
            (~)))))
  (keyQueueStackUnRegister eventQueue)
  ((WinPalette 'toggle)))
; Avatar_color_chooser
;;;;;;;;;;;;;;;;;;;;;;;;



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

; Consider the expression associated with this button value.  If data just return the expression.  Otherwise
; assume a closure.  Consider the closure -> closure's code -> the code's pre-compiled
; expression and return it (hack).
(define (getButton ch)
  (if (symbol? ch)
    (let ((a (assq ch ButtonsSymbols)))
      (if (pair? a) (cdr a) ()))
    (vector-ref Buttons ch)))

(setButton 'down '(walk 6))
(setButton #\j (lambda () (walk 6)))
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
  (WinChatDisplay "\r\nMap animation " VIEWPORTANIMATION)))
(setButton #\C '(avatarColor))
;(setButton #\W '(rollcall))
(setButton #\H '(winMapLeft))
(setButton #\J '(winMapDown))
(setButton #\K '(winMapUp))
(setButton #\L '(winMapRight))
(setButton #\S '(begin
  (set! MAPSCROLL
    (if (eq? MAPSCROLL 'always) 'edge
    (if (eq? MAPSCROLL 'edge) 'never
    'always)))
  (WinChatDisplay "\r\nScroll mode set to:" MAPSCROLL)))
(setButton #\M '((avatarMap 'toggleWindow)))
(setButton #\s '(focusTalk 'scream))
(setButton #\t '(focusTalk 'talk))
(setButton #\w '(focusTalk 'whisper))
(setButton CHAR-CTRL-D '(begin
                          (ipc '(set! Debug (not Debug)))
                          (((avatar 'myMap) 'debugDumpMapInfoToggle))
                          (set! SHOWBUTTONS (not SHOWBUTTONS))))
(setButton CHAR-CTRL-E '(begin (set! EDIT (not EDIT)) (WinChatDisplay "\r\nEDIT " EDIT)))
(setButton CHAR-CTRL-L '(begin ((avatarMap 'canvasResetArray) (avatarMap 'ceiling))
                               ((avatarMap 'viewportRecenterReset) (avatar 'y) (avatar 'x))
                               ((WinChat 'repaint))))
(setButton #\d '(buttonSetCell (avatar 'cell)))
(setButton #\g
   '(let ((o (apply (avatarMap 'baseCell) ((avatar 'gps)))))
     (WinChatDisplay "\r\nGrabbed " o)
     (buttonSetCell cellAIR)
     (avatar `(set! cell ,o))))
(setButton #\? '(help))
(setButton #\< '((avatarMap 'smaller)))
(setButton #\> '((avatarMap 'bigger)))
(setButton #\z '((avatarMap 'circularize)))
(setButton #\Z '((avatarMap 'circularize) #t))
(setButton CHAR-CTRL-@ '(shutdown))
(setButton CHAR-CTRL-Q '(shutdown))
(setButton #\Q         '(shutdown))
(setButton #eof        '(shutdown))
(setButton CHAR-CTRL-C '((WinConsole 'toggle)))
(setButton #\  '(begin (avatar '(stop)) (kat '(stop))))
(if QUIETLOGIN (begin
   (setButton #\1 '(WinChatDisplay "\r\n" (cellSymbol (cellRef ((avatar 'lookHere))))
                                      " " (cellSymbol (cellRef ((avatar 'lookAt))))))
   ;(setButton #\2 '(handleTerminalResize (cons 600 400)))
   (setButton #\3 '(thread (spawnKitty)))
   (setButton #\4 '(ghosts))
   (setButton #\5 '(pong))
   (setButton #\6 '(WinChatDisplay "\r\n" ((avatarMap 'column) (avatar 'y) (+(avatar 'x)1))))
))

; Perform button's action
(define (button . buttonList)
 (each-for buttonList (lambda (b)
  (let ((expr (getButton b)))
    (if SHOWBUTTONS (WinConsoleDisplay "BUTTON(" b " " expr ")"))
    ; Evaluate the button's command value
    (cond ((procedure? expr)  (expr))
          ((not (null? expr)) (eval expr))
          (else (WinConsoleDisplay "\r\nButton " b " undefined")))))))



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
             (begin (WinChatDisplay "\r\n")
                    (WinChatDisplay talkInput)
                    (WinChatDisplay "=>")
                    (WinChatDisplay
                      (call/cc (lambda (c) ; Return here if an error occurs
                         (vector-set! ERRORS (tid) c)
                         (eval (read-string (cdr-string talkInput)))))))
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
    (WinInputPuts 
      (if (eq? type 'scream) (string "}}" (replTalk 'getBuffer))
      (if (eq? type 'whisper)(string "(" (replTalk 'getBuffer) ")")
                             (string ">" (replTalk 'getBuffer)))))
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
        (lambda (x) (and Stop (unthread)) (walk x) (sleep 400))
        '(0 0 0 0 2 2 2 2 4 4 4 4 6 6 6 6))
      (sleep 500)
      (~))))
   (begin
     (WinChatSetColor 0 10) (WinChatDisplay "\r\n*Thus ends the journey*")
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
     (WinChatSetColor 0 10) (WinChatDisplay "\r\n*Thus ends the aimlessness*")
     (set! Stop #t))))) ; walkAround


(define pongPower #f)

(define pongActionMacro (macro ()
  (let ((oy (* (/ (avatar 'y) MapBlockSize) MapBlockSize)) ; Origin of this map block
        (ox (* (/ (avatar 'x) MapBlockSize) MapBlockSize))
        (m 0) ; Map location ball is walking to
        (n 0))
   (set! pongPower #t)
   (WinChatDisplay "\rnPong starts " dna " " name)
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
   (WinChatDisplay "\r\nPong ends " dna " " name)
   (die)))) ; pong

(define (pong)
 (if pongPower
   (set! pongPower #f)
   (thread
     ((Avatar "()PongBall" (avatar 'z) (avatar 'y) (+ (avatar 'x) 1) ipc 'NOVIEWPORT)
      '(pongActionMacro)))))


; Tank agent - The first interactive user agent.
(define (tankTalk str)
  (thread
    (sleep 500)
    (WinChatSetColor 0 15) (WinChatDisplay "\r\nTank ")
    (WinChatSetColor 0 7)  (WinChatDisplay str)))

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
  (set! tankHangupTime (+ 10 (time))))

(define (tankTheOperator talkInput)
 (if (string=? "?" talkInput) (help))
 (if (string=? talkInput "tank")
   (tankStartListening)
   (let ((strLen (string-length talkInput)))
    (if (and (> strLen 11) (string=? "my name is " (substring talkInput 0 11)))
      (thread (changeName (substring talkInput 11 strLen))))))
 (if tankHangupTime (cond
   ((string=? "who" talkInput) (IpcWrite '(say "I'm here!")))
   ((string=? "load the jump program" talkInput) (tankTalk "I can't find the disk"))
   ((string=? "sex" talkInput) (tankTalk (satc)))
   ((string=? "march" talkInput) (avatar '(march)))
   ((string=? "walk around" talkInput) (avatar '(walkAround)))
   ((string=? "edit" talkInput) (begin (set! EDIT (not EDIT)) (tankTalk "Edit mode " EDIT)))
   ((string=? "island" talkInput) ((avatar 'jump) 1 4150 5602))
   ((string=? "theoffice" talkInput) ((avatar 'jump) 1 3869 1053))
   ((string=? "scrabble" talkInput)  ((avatar 'jump) 1 3338 3244))
   ((string=? "britania" talkInput)  ((avatar 'jump) 1 3456 2751)))))

; Sex and the City episode recommender
(define (satc)
 (let ((lst ()))
  (loop 6 (lambda (season)
     (loop (vector-ref #(12 18 18 18 8 20) season)
      (lambda (episode)
       (set! lst (cons (list 's (+ season 1) 'e (+ episode 1)) lst))))))
  (vector-random (list->vector lst))))

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
      (let ((y (avatar 'y)) (x (avatar 'x)))
        (set! g1 (or ghostsOn (Avatar "G1" 0 (- y 1) (- x 1) ipc 'NOVIEWPORT)))
        (set! g2 (or ghostsOn (Avatar "G2" 0 (- y 1) (+ x 1) ipc 'NOVIEWPORT)))
        (set! g3 (or ghostsOn (Avatar "G3" 0 (+ y 1) (- x 1) ipc 'NOVIEWPORT)))
        (set! g4 (or ghostsOn (Avatar "G4" 0 (+ y 1) (+ x 1) ipc 'NOVIEWPORT)))
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
 (thread (let ~ ((dir dir)) ; Use this local symbol and not the Avatar class'.
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
      (WinChatDisplay "\r\nYour pacman game is over."))
    (begin
      (WinChatSetColor 0 15)
      (WinChatDisplay "\r\nWelcome to pacman mode")
      (WinChatSetColor 0 7)
      (WinChatDisplay "\r\n q.......quit")
      (WinChatDisplay "\r\n g.......act like a ghost")
      (WinChatDisplay "\r\n arrows..move pacman")))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Genesis
;;
(load "ultima4.cells")
(load "scrabble.scm") ; TODO temporary

; Create ipc object.  Pass in a serializer which prints to the console window.
(define ipc (Ipc WinConsoleDisplay))
(ipc '(set! Debug #f))

; TODO an often used call by the un-refactored code
(define IpcWrite (ipc 'qwrite))

; Animated welcome marquee
(or QUIETLOGIN (thread (welcome)))

; Get username.  Create avatar object.
(define avatar (if QUIETLOGIN "Administrator" (boxInput "Enter your name")))
(if (eq? "" avatar) (set! avatar "Guest"))
(set! avatar (Avatar avatar 1 3464 2767 ipc))
;(set! avatar (Avatar avatar 1 3438 2735 ipc)) ; Pacman arena
(avatar '(set! climb #t))

; Consider the avatar's map object
(define avatarMap (avatar 'myMap))

; TODO still used in handleTerminalResize winmapUp/Down/Left/Right mouseWalkActionHandlerLoop
(define avatarViewport (avatarMap 'myViewport))

; Start map mouse action handler
(mouseWalkActionHandlerLoop)

; Catch some signal so that normal shutdown can occur
; TODO buggy repeated calls to the same handler occurs with I/O signals
(signal-set 1 (lambda () (say "signal 1 HUP")  (shutdown 'now)))
(signal-set 2 (lambda () (say "signal 2 INT")  (shutdown 'now)))
(signal-set 3 (lambda () (say "signal 3 QUIT")  (shutdown 'now)))
(signal-set 6 (lambda () (say "signal 6 ABRT")  (shutdown 'now)))
;(signal-set 13 (lambda () (say "signal 13 PIPE")  (shutdown 'now)))
(signal-set 15 (lambda () (say "signal 15 TERM")  (shutdown 'now)))

; Display welcome information an announce my presence
(or QUIETLOGIN (begin
 (fancyDisplay 13 (string "Welcome to World, " (avatar 'name)))
 (WinChatSetColor 0 10) (WinChatDisplay "\r\nHit ? to toggle the help window")
 (WinChatSetColor 0 6) (WinChatDisplay "\r\nSee http://code.google.com/p/worldtm")
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
(define (shutdown . now)
  (if (or (pair? now) QUIETLOGIN (boxBool "Quit?")) (begin
    (set! SHUTDOWN #t)
    ((avatar 'die)) ; Force an IPC message so avatar's IPC reader thread calls die method
    (sleep 400)
    (displayl "\e[" (Terminal 'Theight) "H\r\n\e[0m\e[?25h\e[?1000lgc=" (fun) "\r\n")
    (quit))))

; Spawn a second avatar.  Your free kitteh.
(define kat (Avatar (string "katO'" (avatar 'name)) 1 (+ (random 10) 3458) (+ (random 10) 2764) ipc 'NOVIEWPORT))

; Keyboard command loop
(let ~ () (let ((b (getKey)))
  (set! ActivityTime (time))
  (button b)
  (~)))
