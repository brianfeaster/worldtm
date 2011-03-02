;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The World Client
;;
;;   Windows
;;    Keyboard_mouse
;;   Cells_object
;;   Column_object
;;   Field_object
;;   Canvas_object
;;   Viewport_object
;;   Map_object
;;    Entity_DB
;;    Avatar_object
;;   Window_functions_and_initialization
;;   Button_commands
;;   Buttons
;;   Typing_and_talking
;;    Prototypes_and_fun_things
;;   Genesis
;;
(load "ipc.scm")
(load "window.scm")
(load "entity.scm")
(define QUIETLOGIN (and (< 2 (vector-length argv)) (eqv? "silent" (vector-ref argv 2))))
(define VIEWPORTANIMATION #t)
(define MAPSCROLL 'always) ; always edge never
(define KITTEHBRAIN  #f)
(define VOICEDELIMETER " ")
(define NAME "Guest")
(define DNA 0)
(define ActivityTime (time))
(define MapBlockSize 32) ; Size of each map file cell in the World map.
(define PortMapAgent #f)
(define EDIT #f)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;

; Force only one instance of the Terminal object by assigning the object instance to its symbol.
(define Terminal (Terminal))

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

; Help Window.
(define WinHelpBorder ((Terminal 'WindowNew) 4 20 16 32 #x0200))
(define WinHelp ((Terminal 'WindowNew) 5 21 14 30 #x000a))
((WinHelpBorder 'toggle))
((WinHelp 'toggle))

;; Stats window
(define WinStatus ((Terminal 'WindowNew)
   0 (- (Terminal 'Twidth) 14)
   3            14
   #x040e))
((WinStatus 'toggle))
(define (WinStatusDisplay . e)
  (for-each (lambda (x) (for-each (WinStatus 'puts) (display->strings x))) e))

;; Map column debug window
(define WinColumn ((Terminal 'WindowNew) 3 (- (Terminal 'Twidth) 5) 18 5 #x050b))
((WinColumn 'toggle))
(define WinColumnPutc (WinColumn 'putc))
(define (WinColumnPuts . l) (for-each (WinColumn 'puts) l))
(define WinColumnSetColor (WinColumn 'set-color))

(define WinPalette ((Terminal 'WindowNew) 2 15 9 36 #x000f))
(define (WinPaletteDisplay . e) (for-each (lambda (x) (for-each (WinPalette 'puts) (display->strings x))) e))
(define WinPaletteColor (WinPalette 'set-color))
(define WinPaletteGoto (WinPalette 'goto))
((WinPalette 'cursor-visible) #f)
((WinPalette 'toggle))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard_mouse
;;
(define keyDispatcherStack ())
(define (keyDispatcherRegister fn) (set! keyDispatcherStack (cons fn keyDispatcherStack)))
(define (keyDispatcherUnRegister fn) (set! keyDispatcherStack (list-delete keyDispatcherStack fn)))
(define (keyDispatcher c)
  ;(WinChatDisplay "\r\n keyDispatcher  c=" c)
  (if (pair? keyDispatcherStack)
      ((car keyDispatcherStack) c)))

; Vector of fuctions accepting (action y x).  TODO only 128 windows supported
(define mouseDispatchVector (make-vector 128 #f))

(define (mouseDispatcherRegister win fn)
  (vector-set! mouseDispatchVector (win 'id) fn))

(define (mouseDispatcherUnRegister win)
  (mouseDispatcherRegister win #f))

(define (mouseDispatcher event y x)
  (letrec ((win ((Terminal 'topWin) y x))
           (id (if win (win 'id) #f))
           (fn (if id (vector-ref mouseDispatchVector id) #f)))
  ;(WinChatDisplay "\r\n mouseDispatcher  event=" event " y=" y " x=" x " winid=" id "  fn=" fn)
  (if fn (fn event (- y (win 'Y0)) (- x (win 'X0))))))

; This needs to be called in a separate thread
(define (keyScannerAgentLoop)
 (let ((c (read-char #f stdin)))
   (if (eq? c CHAR-ESC)
     (keyScannerEsc) ; Escape char so attempt to read an escape sequence
     (keyDispatcher c)) ; Add new keyboard character to queue
   (keyScannerAgentLoop))) ; rinse and repeat

; An "\e" scanned
(define (keyScannerEsc)
 (let ((c (read-char 500 stdin))) ; Timeout after half a second.  Will return #f.
   (if (eq? c #\[)
     (keyScannerEscBracket)
   (if (eq? c CHAR-ESC)
     (begin
       (keyDispatcher CHAR-ESC)
       (keyScannerEsc))
   (if (not c) ; Timed out so accept escape char and start over
       (keyDispatcher CHAR-ESC)
   (begin ; Not a recognized escape sequence, so send escape and the [ character
     (keyDispatcher CHAR-ESC)
     (keyDispatcher c)))))))

; An "\e[" scanned
(define (keyScannerEscBracket)
  (let ((c (read-char #f stdin)))
    (if (eq? c #\A) (keyDispatcher 'up)
    (if (eq? c #\B) (keyDispatcher 'down)
    (if (eq? c #\C) (keyDispatcher 'right)
    (if (eq? c #\D) (keyDispatcher 'left)
    (if (eq? c #\M) (keyScannerEscBracketM)
    (begin ; Not an arrow key sequence, so send all the character to the key queue
      (keyDispatcher CHAR-ESC)
      (keyDispatcher #\[)
      (keyDispatcher c)))))))))

; An "\e[M" has been scanned
(define (keyScannerEscBracketM)
 (letrec ((c (read-char #f stdin))
          (action (if (eq? c #\ ) 'mouse0
                  (if (eq? c #\!) 'mouse2
                  (if (eq? c #\") 'mouse1
                  (if (eq? c #\#) 'mouseup
                  'mouse)))))
          (x (- (read-char #f stdin) #\  1))
          (y (- (read-char #f stdin) #\  1)))
  (mouseDispatcher action y x)))

; Base keyboard read queue.  Continuously read stdin and append to a FIFO.
; Call getKey to read it.  It is possible that another dispatcher has been
; registered and captures characters before this.
; TODO move this to terminal class
(define getKeyQueue (QueueCreate))
(define (getKeyQueueAdd c) (QueueAdd getKeyQueue c)) ; List of characters and button symbols
(define (getKey) (QueueGet getKeyQueue))
(keyDispatcherRegister getKeyQueueAdd)



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
                      (if (<= i 1) 1 ; default bottom cell
                      (if (<= (vector-length column) i) (- (vector-length column) 1) ; default top cell
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

(define (containsVisibleCell? l)
  (if (pair? l)
      (or (containsVisibleCell? (car l))
          (containsVisibleCell? (cdr l)))
      (cellVisible? l)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field_object
;;
;; Fields are arrays of columns.  Columns are quasi-compressed stacks of cells
;; that consist of a start height and specified stack of cells.  #(3 1 1 2)
;; would be cells (1 1 2) starting at z=4=(3+1). Cells below and above are
;; assumed to be the lowest and higest specified cells in the vector.  Setting
;; a cell outside the explicit stack range expands the actual vector and
;; adjusts the start-height value.
(define (Field size) ; Field dimension is currently 256x256
  (define (self msg) (eval msg))
  (define field (make-vector-vector size size #f))
  (define (reset defaultColumn)
    (loop2 0 size 0 size (lambda (y x)
      (vector-vector-set! field y x defaultColumn))))
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
     (if (containsVisibleCell? (columnRef col i)) i
     (if (<= top i) 100
     (~ (+ i 1)))))))
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
          (vector-vector-ref cellAry y x)))))
  ; Initialize the field with a default column
  ;(WinChatDisplay "\r\nInitializing field...")
  (reset (columnMake 0 cellXX cellAIR)) ; Initialize the canvas with a bogus cell and height to speed up initializing
  self) ; Field



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas_object
;; A canvas entry is a vector-vector consisting of a glyph and it's Z
;; coordinate (the top most visible cell relative to the user usually).
;;
(define (Canvas field)
  (define (self msg) (eval msg))
  (define size (field 'size))
  (define fieldCell (field 'firstRef))
  (define fieldTopHeight (field 'topHeight))
  (define canvas (make-vector-vector size size #f))
  ; Initialize the canvas which is a vector of pairs (cellGlyph . cellHeight)
  ; walkDetails Genesis
  (define (reset ceilingHeight . defaultGlyph)
    (set! defaultGlyph (if (null? defaultGlyph) #f (car defaultGlyph)))
    (loop2 0 size 0 size (lambda (y x)
      ; Each canvas entry consists of a map cell and its height.
      (vector-vector-set! canvas y x
        (if defaultGlyph
          ; default pair
          (cons defaultGlyph 0)
          ; pair based on visible cell in field
          (letrec ((t (fieldTopHeight ceilingHeight y x))
                   (celli (fieldCell t y x)))
            (cons (if (< CellMax celli)
                      ((entitiesGet celli) 'glyph) ; An entity
                      (cellGlyph (cellRef celli))) ; A cell
                  t)))))))
  (define (glyph y x)
    (car (vector-vector-ref canvas
           (modulo y size)
           (modulo x size))))
  (define (height y x) ; Map ipc::die
    (cdr (vector-vector-ref canvas
           (modulo y size)
           (modulo x size))))
  (define (glyphSet y x c) ; Canvas
    (set-car! (vector-vector-ref canvas
                (modulo y size)
                (modulo x size))
              c))
  (define (heightSet y x h)
   (set-cdr! (vector-vector-ref canvas
               (modulo y size)
               (modulo x size))
             h))
  (define (render top y x) ; Map ipc::entity ipc::die !Field!
   (let ((z (fieldTopHeight top y x))) ; Get z of first cell starting at top
     (let ((celli (fieldCell z y x))) ; Field might contain an entity's dna
       (glyphSet y x (if (< CellMax celli)
                               (letrec ((ent (entitiesGet celli))
                                        (sprite (ent 'sprite))
                                        (ey (ent 'y))
                                        (ex (ent 'x)))
                                 ((sprite 'glyphRef) (- y ey) (- x ex)))
                               (cellGlyph (cellRef celli)))))
     (heightSet y x z)))
  ;(WinChatDisplay "\r\nInitializing canvas...")
  ; Initialize the canvas
  (reset 10 (cellGlyph (cellRef cellxx)))
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
  (macro (canvas) ; Child
    (define (self msg) (eval msg))
    (define canvasSize (canvas 'size))
    (define canvasGlyph (canvas 'glyph))
    (define mapY 0) ; Map location of viewport origin
    (define mapX 0) ;
    (define winHeight 0)
    (define winWidth 0)
    (define mapCenterY 0) ; Map location of viewport center
    (define mapCenterX 0) ;
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
    (define (reset y x)
       ; Reset next draw time to less than a second so the agent loop triggers a redraw.
      (set! NextDrawTime (+ 300 (utime)))
      (lock) ; This shouldn't be such an all encompasing lock.
      (set! mapCenterY y)
      (set! mapCenterX x)
      (set! winHeight Wheight) ; Adjust Viewport dimensions
      (set! winWidth (/ Wwidth 2))
      (set! mapY (- y (/ winHeight 2)))     ; Center Viewport around Avatar
      (set! mapX (- x (/ winWidth 2)))
      (loop2 0 winHeight 0 winWidth (lambda (y x) ; Render glyphs in viewport
        (plot (canvasGlyph (+ mapY y) (+ mapX x))
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
      (let ((y (modulo (- gy mapY) canvasSize)) ; Normalize avatar position.
            (x (modulo (- gx mapX) canvasSize)))
       (and (< y winHeight) (< x winWidth) (begin
         ((Terminal 'lock)) ; This shouldn't be such an all encompasing lock.
         (plot (canvasGlyph gy gx) y (* x 2))
         ((Terminal 'unlock)))))) ; This shouldn't be such an all encompasing lock.
    (define (animationAgentLoop)
      (sleep 1000)
      (and VIEWPORTANIMATION (< NextDrawTime (utime)) (reset mapCenterY mapCenterX))
      (animationAgentLoop))
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

(define (Map fieldSize ipc)
  (define (self msg) (eval msg))
  ; The objects which make up a map object
  (define myField (Field fieldSize))
  (define myCanvas (Canvas myField))
  (define myViewport (Viewport 0   (- (Terminal 'Twidth) (* initialMapSize 2))
                               initialMapSize   (* 2 initialMapSize)  #x000f
                               myCanvas))
  ; Often used aliases
  (define ipcRead ((ipc 'newReader)))
  (define ipcWrite (ipc 'qwrite))
  (define fieldCeiling (myField 'ceiling)) ; walkDetails
  (define fieldAdd (myField 'add))
  (define fieldDelete (myField 'delete))
  ; Drop a cell on the map  *TODO this references nonexistant global function*
  (define (mapDropCell y x cell)
   (let ((z (+ 1 (fieldTopHeight 100 y x))))
    ((myField 'fieldSet!) z y x cell)
    (canvasRender 100 y x)
    (viewportRender y x)))
  ; Set map cell and force rendering of cell through entire pipeline.
  (define (setCell z y x cell)
    ((myField 'fieldSet!) z y x cell)
    (canvasRender 100 y x)
    (viewportRender y x))
  ; Delete a cell from the field.  Update canvas if needed.
  (define (delCell cell z y x)
    (fieldDelete z y x cell)
    (if (>= z (canvasHeight y x)) (begin
      (canvasRender (avatar 'ceiling) y x)
      (viewportRender y x))))
  ; Add a cell to the field.  Update canvas if needed.
  (define (addCell cell z y x)
    (fieldAdd z y x cell)
    (if (>= z (canvasHeight y x)) (begin
      (canvasRender (avatar 'ceiling) y x)
      (viewportRender y x))))
  (define (moveCell cell zo yo xo z y x)
    ; Old location removal
    (delCell cell zo yo xo)
    ; New location added
    (addCell cell z y x))
  (define (delEntitySprite dna zo yo xo)
    (letrec ((entity (entitiesGet dna))
             (sprite (entity 'sprite))
             (h (sprite 'height))
             (w (sprite 'width))
             (coordinates (sprite 'coordinates))) ; List of the sprites glyph coordinates
      ; Second update canvas and viewport
      (each-for coordinates
        (lambda (c)
          (if c (let ((m (car c))
                      (n (cdr c)))
            ; First update field
            (fieldDelete zo (+ m yo) (+ n xo) dna)
            ; Render old deleted location
            (if (>= zo (canvasHeight (+ m yo) (+ n xo)))
             (begin
               (canvasRender (avatar 'ceiling) (+ m yo) (+ n xo))
               (viewportRender (+ m yo) (+ n xo)))))))))) ; Don't render cell if viewport to be reset
  (define (moveEntitySprite dna zo yo xo z y x centerMap)
    (if (eq? MAPSCROLL 'never) (set! centerMap #f)) ; Global toggle to disable map scrolling
    (letrec ((entity (entitiesGet dna))
             (sprite (entity 'sprite))
             (h (sprite 'height))
             (w (sprite 'width))
             (coordinates (sprite 'coordinates))) ; List of the sprites glyph coordinates
      ; First update field
      (each-for coordinates
        (lambda (c) (if c (let ((m (car c))
                                (n (cdr c)))
                      (fieldDelete zo (+ m yo) (+ n xo) dna) ; Old
                      (fieldAdd z (+ m y) (+ n x) dna))))) ; New
      ; Second update canvas and viewport
      (each-for coordinates
        (lambda (c)
          (if c (let ((m (car c))
                      (n (cdr c)))
            ; Render old deleted location
            (if (>= zo (canvasHeight (+ m yo) (+ n xo))) (begin
              (canvasRender (avatar 'ceiling) (+ m yo) (+ n xo))
              (or centerMap (viewportRender (+ m yo) (+ n xo))))) ; Don't render cell if viewport to be reset
            ; Render new added location
            (if (>= z (canvasHeight (+ m y) (+ n x))) (begin
              (canvasRender (avatar 'ceiling) (+ m y) (+ n x))
              (or centerMap (viewportRender (+ m y) (+ n x)))))))))) ; Don't render cell if vewport to be reset
    (if centerMap (viewportReset y x))
    (if (WinColumn 'ENABLED) (dumpColumnInfo y x)))
  ; Given a cell index or entity DNA value, move it in the field, canvas and viewport.
  (define moveObject
    (let ((MAP-MOVE-CELL-SEMAPHORE (open-semaphore 1)))
    (lambda (cell zo yo xo z y x centerMap)
      (semaphore-down MAP-MOVE-CELL-SEMAPHORE)
      (if (cellValidIndex? cell)
        (moveCell         cell zo yo xo z y x) ; Move cell
        (moveEntitySprite cell zo yo xo z y x centerMap)) ; Move entity
      (semaphore-up MAP-MOVE-CELL-SEMAPHORE))))
  ; Make Map window circular
  (define (circularize . val)
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
      (~ y (+ x 1)))))))
  ; The 2d vector of columns will most likely come from a map agent.
  ; The map block coordinate and map block size is also passed.
  (define (updateColumnsIPC y x blockSize cellAry) ; Called from map agent via IPC
    (let ((fieldy (modulo y fieldSize))
          (fieldx (modulo x fieldSize)))
     ; Update the field block
     ((myField 'updateColumns) fieldy fieldx blockSize cellAry)
     ; Render map block
     (loop2 fieldy (+ fieldy blockSize)
            fieldx (+ fieldx blockSize)
            (lambda (y x) (canvasRender 100 y x)))))
  (define (moveIPC dna z y x)
   (or (= dna DNA) ; Skip if this is me since map rendering is handled during movement handling
       (let ((entity (entitiesGet dna)))
         (if entity (moveEntity entity z y x)))))
  (define fieldFirstCell (myField 'firstRef))
  (define baseCell       (myField 'baseRef))
  (define viewportReset  (myViewport 'reset))
  (define viewportRender (myViewport 'render))
  (define toggleWindow   (myViewport 'toggle))
  (define canvasReset    (myCanvas 'reset))
  (define canvasHeight   (myCanvas 'height))
  (define canvasRender   (myCanvas 'render))
  (define canvasGlyph    (myCanvas 'glyph)) ; Debugging
  ;(WinChatDisplay "\r\nInitializing map...")
  (circularize)
  ; IPC message handler
  (thread (let ~ ()
    (let ((e (ipcRead)))
      (if (pair? e) (begin
        (if (eq? (car e) 'move)             (apply moveIPC (cdr e))
        (if (eq? (car e) 'mapUpdateColumns) (apply updateColumnsIPC (cdr e))
        (if (eq? (car e) 'mapSetCell)       (apply setCell (cdr e)))))))
      (~))))
  self) ; Map



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entity_DB
;;

; Association list of entites to their DNA values.
(define EntityDB ())

(define (entitiesAdd entity) ; Genesis:add avatar  entitiesSet
 (set! EntityDB (cons (cons (entity 'dna) entity) EntityDB)))

; Lookup entity in database. TODO use hastable
(define (entitiesGet dna)
  (let ((entity (assv dna EntityDB)))
    (if (null? entity) #f (cdr entity))))

(define (entitiesSet dna . args) ; IPC:Entity
  (let ((entity (entitiesGet dna)))
    (if entity
      ; Update only name and glyph if entity already exists
      (each-for args
        (lambda (a)
          (if (integer? a)   ((entity 'setPort) a)
          (if (string? a)    ((entity 'setName) a)
          (if (vector? a)
            (begin ((entity 'setGlyph) a)
                   (if (= 1 ((entity 'sprite) 'glyphCount))
                       ((entity 'setSprite) (Sprite 1 1 (vector a)))))
          (if (procedure? a) (begin
                (mapDelEntitySprite dna (entity 'z) (entity 'y) (entity 'x)) ; Remove it first from the map
                ((entity 'setSprite) a)))))))) ; TODO BF sprite update mechanism stinks
      ; Create a new entity.  Massage the arguments (port name glyph (x y z))->(port name glyph x y z)
      (begin
        (set! entity (apply Entity dna (let ~ ((args args))
                                         (if (pair? (car args))
                                             (car args)
                                             (cons (car args) (~ (cdr args)))))))
        (entitiesAdd entity)))
    ; Return the new or modified entity
    entity))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Avatar_object
;;
;; The user's avatar.  An extended entity object that includes positioning
;; and directional observation vectors

(define (Avatar ipc name z y x . childArgs) ; Inherits Entity
 (((Entity (random) (if ipc (ipc 'PrivatePort) 0) name z y x) 'inherit) (append (list ipc) childArgs) ; Parent and args to child
  (macro (ipc) ; Child
    (define (self msg) (eval msg))
    (define cell 19) ; TODO replace with a generalize item container
    (define dir 0)
    (define tz 0) ; Translation coordinates
    (define ty 0)
    (define tx 0)
    (define ipcRead (if ipc ((ipc 'newReader)) #f))
    (define ipcWrite (if ipc (ipc 'qwrite) #f))
    (define (look ndir . rloc) ; Look in a direction plus a relative (not rotational) zyx location
      (set! dir ndir)
      (if (pair? rloc)
        (begin
          (set! tz (car rloc))
          (set! rloc (cdr rloc)))
        (set! tz 0))
      (if (pair? rloc)
        (begin
          (set! ty (car rloc))
          (set! rloc (cdr rloc)))
        (set! ty 0))
      (if (pair? rloc)
        (set! tx (car rloc))
        (set! tx 0)))
    (define (gpsLook . rloc) ; Return location avatar is looking at
      (if (pair? rloc) (begin
        (set! tz (car rloc))
        (set! ty (cadr rloc))
        (set! tx (caddr rloc))))
      (if (= dir 0) (list (+ z    tz) (+ y    ty) (+ x  1 tx))
      (if (= dir 1) (list (+ z    tz) (+ y -1 ty) (+ x  1 tx))
      (if (= dir 2) (list (+ z    tz) (+ y -1 ty) (+ x    tx))
      (if (= dir 3) (list (+ z    tz) (+ y -1 ty) (+ x -1 tx))
      (if (= dir 4) (list (+ z    tz) (+ y    ty) (+ x -1 tx))
      (if (= dir 5) (list (+ z    tz) (+ y  1 ty) (+ x -1 tx))
      (if (= dir 6) (list (+ z    tz) (+ y  1 ty) (+ x    tx))
      (if (= dir 7) (list (+ z    tz) (+ y  1 ty) (+ x  1 tx))
      (if (= dir 8) (list (+ z -1 tz) (+ y    ty) (+ x    tx))
      (if (= dir 9) (list (+ z  1 tz) (+ y    ty) (+ x    tx)))))))))))))
    (define ceiling z)
    (define (setCeiling z) (set! ceiling z))
    (define (die) (ipcWrite `(die ,dna))) ; Announce that I'm leaving
    (define (jump z y x)
     (vatar 'setLoc z y x)
     (walkDetails))
    (define walkSemaphore (open-semaphore 1))
    (define (walkDetails)
      ; Update avatar locally in the field/canvas/viewport and via IPC
      (apply moveEntity self (gpsLook))
      (IpcWrite (list 'move DNA z y x))
      ; Special case to handle cells that change the Avatar's sprite
      (letrec ((cellNum (apply mapBaseCell (gps)))
               (baseSym (cellSymbol (cellRef cellNum))))
        (if (and (<= 512 cellNum) (<= cellNum 612)) (WinChatDisplay "\r\n" (twoLetterDefinition baseSym))
        (if (eq? baseSym 'sprite0) (setSprite 0)
        (if (eq? baseSym 'sprite1) (setSprite 1)
        (if (eq? baseSym 'sprite2) (setSprite 2))))))
      ; If ceiling changes, repaint canvas using new ceiling height
      (let ((oldCeiling ceiling))
        (setCeiling (- (apply (myMap 'fieldCeiling) (gps)) 1))
        (if (!= oldCeiling ceiling)
          (thread (mapCanvasReset ceiling))))) ; walkDetails
    (define (walk dir)
      (semaphore-down walkSemaphore)
        ; Consider cell I'm walking into.  If cell is entity push it.
        ; Otherwise move to facing cell or on top of obstructing cell.
        (look dir)
        (let ((nextCell (apply mapFirstCell (gpsLook))))
          ; Case 1 push entity
          (if (< CellMax nextCell)
            (IpcWrite `(force ,@(gpsLook) ,dir 10))
          ; Case 2 walk normally
          (if (or EDIT (not (cellSolid? (cellRef nextCell)))) ; TODO this always true
            (walkDetails)
          ; Case 3 step up
          (begin
            (look dir 1) ; Peek at the cell above the one in front of me
            (set! nextCell (apply mapBaseCell (gpsLook)))
            (or (cellSolid? (cellRef nextCell))
              (walkDetails))))))
        ; Gravity
        (or EDIT (fall))
      (semaphore-up walkSemaphore)) ; walk
    ; Fall down one cell if a non-entity and non-solid cell below me
    (define (fall)
     (look 8) ; Look down
     (let ((nextCell (apply mapFirstCell (gpsLook))))
       (if (= nextCell cellAIR)
         (begin
           (walkDetails)))))
    (define (say talkInput . level)
      (ipcWrite (list 'voice dna
                      (if (null? level) 10 (car level))
                      (apply string (display->strings talkInput)))))
    (define IPCvoice list)
    (define (IPCvoiceRegister fn) (set! IPCvoice fn))
    (define (IPCforce fz fy fx dir mag)
     (if (and (= fz z) (= fy y) (= fx x))
       (walk dir)))
    (define (IPCwho . dnaRequestor) ; TODO handle explicit request from specified peer
      (ipcWrite
        `(entity ,dna ,port ,name ',(gps))))
    ; Update one or more of an entity's attribute: dna port name glyph z y x
    (define (entity dna . args)
      (let ((entity (entitiesGet dna)))
        (if entity
          (begin ; Modify entity attributes
            (apply entitiesSet dna args)
            (mapMoveObject (entity 'dna) (entity 'z) (entity 'y) (entity 'x)
                                         (entity 'z) (entity 'y) (entity 'x) #f)
            (mapCanvasRender (avatar 'ceiling) (entity 'y) (entity 'x))
            (mapViewportRender (entity 'y) (entity 'x)))
          (begin ; Create new entity with all args
            (set! entity (apply entitiesSet dna args))
            (mapMoveObject (entity 'dna) (entity 'z) (entity 'y) (entity 'x)
                                         (entity 'z) (entity 'y) (entity 'x) #f)))
        (if (= dna 17749) (set! PortMapAgent (entity 'port))))) ; The map agent's DNA number
    (define (IPCdie dna)
     (let ((entity (entitiesGet dna))
           (thisIsMe (= dna DNA)))
      (if entity ; Ignore unknown entities
        (begin
          ; Remove from here
          (mapDelEntitySprite dna (entity 'z) (entity 'y) (entity 'x)) ; Remove it first from the map
          (if (>= (entity 'z) (mapCanvasHeight (entity 'y) (entity 'x))) (begin
            (mapCanvasRender 100 (entity 'y) (entity 'x))
            (or thisIsMe (mapViewportRender (entity 'y) (entity 'x)))))))))
    ; IPC message handler
    (if ipc (thread (let ~ ()
      (let ((e (ipcRead)))
        (if (pair? e) (begin
          (if (eq? (car e) 'voice) (apply IPCvoice (cdr e))
          (if (eq? (car e) 'force) (apply IPCforce (cdr e))
          (if (eq? (car e) 'who)  (apply IPCwho (cdr e))
          (if (eq? (car e) 'entity)(eval e)
          (if (eq? (car e) 'die)   (apply IPCdie  (cdr e)))))))))
        (~)))))
    (ipcWrite '(who))
    self))) ; Avatar


; Update an avatar's postion internaly and in the map.  Also handle map redrawing and centering.
(define (moveEntity entity z y x)
  (let ((oz (entity 'z))
        (oy (entity 'y))
        (ox (entity 'x)))
   ((entity 'setLoc) z y x)
   (mapMoveObject (entity 'dna)
             oz oy ox
             z y x
             (and (eq? entity avatar) ; If me then might have to redraw the map
                  (or (eq? MAPSCROLL 'always)
                      (and (eq? MAPSCROLL 'edge)
                           (< (- (/ (myViewport 'Wheight) 2) 2)
                              (distance (list 0 y x)
                                        (list 0 (+ (myViewport 'mapY) (/ (myViewport 'winHeight) 2)) (+ (myViewport 'mapX) (/ (myViewport 'winWidth) 2)))))))))))



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

; Plot column of cells.
(define (dumpColumnInfo y x)
 (WinStatusDisplay "\r\n"
    (number->string (avatar 'z)) " "
    (number->string (avatar 'y)) " "
    (number->string (avatar 'x)) "\r\n"
    (number->string (modulo (avatar 'y) MapBlockSize)) " "
    (number->string (modulo (avatar 'x) MapBlockSize)) "\r\n"
    (number->string (/ (avatar 'y) MapBlockSize)) " "
    (number->string (/ (avatar 'x) MapBlockSize)) " C" (avatar 'ceiling))
 ((WinColumn 'home))
 (let ~ ((z 11))
  (let ((c (mapFirstCell z y x)))
   (if (eqv? cellAIR c)
    (begin (WinColumnSetColor 0 8)
           (WinColumnPuts "()   "))
    (begin (set! c (if (< CellMax c) ((entitiesGet c) 'glyph)
                                     (cellGlyph (cellRef c)))) ; Dump the glyph
           (WinColumnSetColor (glyph0bg c) (glyph0fg c))
           (WinColumnPutc (glyph0ch c))
           (WinColumnSetColor (glyph1bg c) (glyph1fg c))
           (WinColumnPutc (glyph1ch c))
           (WinColumnSetColor 0 7)
           (set! c (mapBaseCell z y x)) ; Display base cell's hex value.
           (if (and (<= 0 c) (< c CellMax))
             (begin
               (if (< c 256) (WinColumnPuts "0"))
               (if (< c 16) (WinColumnPuts "0"))
               (WinColumnPuts (number->string c 16)))
             (WinColumnPuts "   ") ))))
  (if (> z -6) (~ (- z 1))))) ; dumpColumnInfo


; Screen redraw and signal handler
(define handlerCount 0)
(define (handleTerminalResize)
  ; TODO Temporary assertion
  (if (!= handlerCount 0) (WinChatDisplay "\r\nWARNING: handleTerminalResize is not reentrant"))
  (set! handlerCount 1)
  (letrec ((newTermSize (terminal-size))
           (tw (car newTermSize))
           (th (cdr newTermSize)))
    (WinConsoleDisplay "\r\nSIGWINCH: newTermSize " newTermSize)
    ((Terminal 'ResetTerminal) newTermSize)
    ((WinChat 'resize)          (- th 1) tw)
    ((myViewport 'move)                0 (- tw (myViewport 'Wwidth) 2))
    ((WinColumn 'move)                 1 (- tw 2) )
    ((WinStatus 'move)  (myViewport 'Y0) (- tw 14))
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
;; Button_commands
;;
; Toggle the help window
(define (help)
 ((WinHelp 'toggle))
 ((WinHelpBorder 'toggle)))

(define deltaMoveTime (utime)) ; Double click 1/16 sec.

(define (winMapBigger) ; TODO move to Map class
 (if (< (myViewport 'Wheight) (Terminal 'Theight)) (begin
  ((Terminal 'lock))
  ((myViewport 'home))
  (if (< (utime) deltaMoveTime)
    ((myViewport 'moveresize) ; Full resize
       0 (- (Terminal 'Twidth) (myViewport 'Wwidth) 2)
             (min (/ (Terminal 'Twidth) 2) (- (Terminal 'Theight) 1))
       (* 2  (min (/ (Terminal 'Twidth) 2) (- (Terminal 'Theight) 1)))))
    ((myViewport 'moveresize) ; Resize by one
       0 (- (Terminal 'Twidth) (myViewport 'Wwidth) 2)
       (+ 1 (myViewport 'Wheight))
       (+ 2 (myViewport 'Wwidth)))
  ((myMap 'circularize))
  ((Terminal 'unlock))
  (mapViewportReset (avatar 'y) (avatar 'x))
  (set! deltaMoveTime (+ 125 (utime))))))

(define (winMapSmaller) ; TODO move to Map class
 (if (< 5 (myViewport 'Wheight)) (begin
  ((Terminal 'lock))
  ((myViewport 'home))
  (if (< (utime) deltaMoveTime)
    ((myViewport 'moveresize) ; Full resize
       0 (- (Terminal 'Twidth) (myViewport 'Wwidth) -10)
       (+ -5 (myViewport 'Wheight))
       (+ -10 (myViewport 'Wwidth)))
    ((myViewport 'moveresize) ; Resize by one
       0 (- (Terminal 'Twidth) (myViewport 'Wwidth) -2)
       (+ -1 (myViewport 'Wheight))
       (+ -2 (myViewport 'Wwidth))))
  ((myMap 'circularize))
  ((Terminal 'unlock))
  (mapViewportReset (avatar 'y) (avatar 'x))
  (set! deltaMoveTime (+ 125 (utime))))))

(define (winMapUp)   ((myViewport 'move) (+ -1 (myViewport 'Y0))      (myViewport 'X0)))
(define (winMapDown) ((myViewport 'move) (+  1 (myViewport 'Y0))      (myViewport 'X0)))
(define (winMapLeft) ((myViewport 'move)       (myViewport 'Y0) (+ -1 (myViewport 'X0))))
(define (winMapRight)((myViewport 'move)       (myViewport 'Y0) (+  1 (myViewport 'X0))))

(define (walk d) ((avatar 'walk) d))

; Notify IPC of my name and glyph change
(define (changeName str)
  (IpcWrite
   (list 'entity DNA str
            (Glyph (glyph0bg (avatar 'glyph))
                   (glyph0fg (avatar 'glyph))
                   (string-ref str 0)
                   (glyph1bg (avatar 'glyph))
                   (glyph1fg (avatar 'glyph))
                   (string-ref str (if (< 1 (string-length str)) 1 0)))))) ; Notify IPC of my name change

(define (rollcall)
 (IpcWrite ; Force all to evaluate the following
  `(if (!= DNA ,DNA) ; Skip if I sent this message
   (IpcWrite ; Force all (except me) to evaluate the following
    `(if (= DNA ,,DNA) ; If me, evaluate this expression from the other peer
     (voice 0 10
      (string ,(avatar 'name) " "
              ,(let ((t (- (time) ActivityTime)))
                (if (< t 60)   (string (number->string t) "s")
                (if (< t 3600) (string (number->string (/ t 60)) "m")
                (if (< t 86400)(string (number->string (/ t 3600)) "h")
                (string (number->string (/ t 86400)) "d"))))))))))))

(define (buttonSetCell cell)
 (if PortMapAgent
   ; Send to map agent. If map agent doesn't respond
   ; then ignore it just send to everyone.
   (or ((ipc 'private) PortMapAgent `(setCellAgent ,(avatar 'z) ,(avatar 'y) ,(avatar 'x) ,cell))
     (begin
       (set! PortMapAgent #f) ; No response so unset the port and recurse
       (buttonSetCell)))
   ; Send to everyone
   (IpcWrite `(mapSetCell ,(avatar 'z) ,(avatar 'y) ,(avatar 'x) ,cell))))

(define (mouseWalkActionHandler action wy wx)
 (if (eq? action 'mouse0)
 (letrec ((mapy (+ (myViewport 'mapY) wy))
          (mapx (+ (myViewport 'mapX) (/ wx 2))))
   ;(WinConsoleDisplay "\r\n" wx " " wy mapy " " mapx)
   (let ~ ((l (lineWalks (avatar 'y) (avatar 'x) mapy mapx)))
     (if (pair? l) (begin
       (walk (car l))
       (~ (cdr l))))))))


; Avatar color chooser.
(define LastColors (make-list 32 0))
(define CursorYX (cons 0 0))

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
    (avatarColorToggle)
 (if (pair? (memv c (list RETURN NEWLINE SPACE)))
   (mouseColorsActionHandler 'mouse0 (car CursorYX) (cdr CursorYX))
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
   (updatePaletteCursor y x)))))

(define (mouseColorsActionHandler action wy wx) ; Was (mouseHandler)
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
   ;(WinChatDisplay "\r\n" wy " " wx " New color=" clr)
   (let ((glyph (avatar 'glyph)))
     (IpcWrite (list 'entity DNA
                      (Glyph
                        (glyph0bg glyph) clr (glyph0ch glyph)
                        (glyph1bg glyph) clr (glyph1ch glyph))))))))

(define (avatarColorToggle)
  ((WinPalette 'toggle))
  (if (WinPalette 'ENABLED)
    (keyDispatcherRegister keyColorsAction)
    (keyDispatcherUnRegister keyColorsAction)))



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
  (WinChatDisplay "\r\nMap animation " VIEWPORTANIMATION)))
(setButton #\C '(avatarColorToggle))
(setButton #\W '(rollcall))
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
(setButton #\M '((myMap 'toggleWindow)))
(setButton #\t '(begin
  (WinInputPuts (string ">" (replTalk 'getBuffer)))
  (set! state 'talk)))
(setButton CHAR-CTRL-D '(ipc '(set! Debug (not Debug))))
(setButton CHAR-CTRL-E '(begin (set! EDIT (not EDIT)) (WinChatDisplay "\r\nEDIT " EDIT)))
(setButton CHAR-CTRL-L '(begin (mapViewportReset (avatar 'y) (avatar 'x))
                               ((WinChat 'repaint))))
(setButton CHAR-CTRL-M '(begin ((WinStatus 'toggle)) ((WinColumn 'toggle))))
(setButton CHAR-CTRL-Q '(set! state 'done))
(setButton #\d '(buttonSetCell (avatar 'cell)))
(setButton #\g
   '(let ((o (apply mapBaseCell ((avatar 'gps)))))
     (WinChatDisplay "\r\nGrabbed " o)
     (buttonSetCell cellAIR)
     (avatar `(set! cell ,o))))
(setButton #\? '(help))
(setButton #\< '(winMapSmaller))
(setButton #\> '(winMapBigger))
(setButton #\z '((myMap 'circularize)))
(setButton #\Z '((myMap 'circularize) #t))
(setButton #\Q '(set! state 'done))
(setButton #eof '(set! state 'done))
(setButton CHAR-CTRL-C '((WinConsole 'toggle)))
(setButton #\1 '(begin (set! state 'pacman) (pacman)))
(setButton #\2 '(thread (pong)))
(setButton #\3 '(thread (spawnKitty 1000)))
;(setButton #\1 '((WinChat 'resize) (WinChat 'Wheight) (WinChat 'Wwidth)))
;(setButton #\1 '((WinChat 'scrollUp)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typing_and_talking
;;
(define (say talkInput . level)
  (apply (avatar 'say) talkInput level))

(define (saySystem talkInput)
 (IpcWrite (list 'voice 0 10 talkInput)))

(define (sayHelloWorld)
 (saySystem (string (avatar 'name)
  (vector-random #(" emerges from the Interwebs" " turns on a VT100" " CONNECT 2400" " CONNECT 14400/ARQ/V34/LAPM/V42BIS"))
  ;" says " (vector-random #("*PUSH* *SQUIRT* *SPANK* *WAAAAAAAAA*" "*All Worldlians Want to Get Borned*" "*Happy Birthday*" "*I thought you were in Hong Kong*"))
 )))

(define (sayByeBye)
  (saySystem (string (avatar 'name) " exits")))

(define replTalk
 (let ((talkInput ""))
  (lambda (b)
   (if (eq? b 'getBuffer) talkInput ; Return input buffer contents.
   ; backspace
   (if (or (eq? b CHAR-CTRL-H)
           (eq? b CHAR-CTRL-_)
           (eq? b CHAR-CTRL-?))
       (begin
        (if (not (eq? "" talkInput))
         (begin ((WinInput 'backspace) #\ )
                (set! talkInput (substring talkInput 0 (- (string-length talkInput) 1)))))
        'talk)
   ; Send Chatter
   (if (or (eq? b RETURN)
           (eq? b NEWLINE))
     (begin
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
       ; Perform actions based on talk phrases.
       (tankTheOperator talkInput)
       (WinInputPuts "\r\n>")
       (set! talkInput "")
       'talk)
   ; Quit chat mode.
   (if (or (eq? b CHAR-ESC) ; Escape char
           (eq? b CHAR-CTRL-I)) ; Tab char
     (begin (WinInputPuts "\r\n")
            'cmd)
   (if (and (>= b #\ )(<= b #\~))
       (begin (WinInputPutc b)
              (set! talkInput (string talkInput b))
              'talk)
   'talk))))))))

(define (replCmd b)
 (define state 'cmd) ; state might be changed to 'done or 'talk.
 (let ((button (getButton b)))
   (if ShowButtons (WinChatDisplay "\r\n" b " " button))
   (if (pair? button) (eval button)
    (if (procedure? button) (button)
     (WinConsoleDisplay "\r\nButton " b " undefined " button))))
 state)

; The various states (talk cmd pacman done)
(define wrepl
 (let ((state 'cmd))
  (lambda ()
   (if (neq? state 'done) ; Exit if state is done.
     (let ((b (getKey)))
       (set! ActivityTime (time))
       (if (eq? state 'talk) (set! state (replTalk b))
         (if (eq? state 'cmd) (set! state (replCmd b))
           (if (eq? state 'pacman) (set! state (replPacman b))
             (begin (WinChatDisplay "\r\nUnknown REPL state " state)
                    (set! state 'cmd)))))
       (wrepl))))))

(define (avatarVoiceHandler dna level text)
  (if (= dna 0)
     (begin
       (WinChatDisplay "\r\n")
       (WinChatSetColor 0 9) (WinChatDisplay "W")
       (WinChatSetColor 0 11) (WinChatDisplay "O")
       (WinChatSetColor 0 10) (WinChatDisplay "R")
       (WinChatSetColor 0 12) (WinChatDisplay "L")
       (WinChatSetColor 0 13) (WinChatDisplay "D")
       (WinChatSetColor 0 8) (WinChatDisplay VOICEDELIMETER)
       (WinChatSetColor 0 7) (WinChatDisplay text))
     (letrec ((entity (entitiesGet dna))
              (glyph (entity 'glyph)))
       (WinChatSetColor (glyph0bg glyph) (glyph0fg glyph))
       (WinChatDisplay "\r\n" (if (null? entity) "???" (entity 'name)) VOICEDELIMETER)
       (WinChatSetColor (glyph1bg glyph) (glyph1fg glyph))
       (WinChatDisplay text)))
   (if (and (!= dna DNA) (eqv? text "unatco"))
       (say "no Savage")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prototypes_and_fun_things
;;

;; Walking kitty soldier
(define (spawnKitty . cycles)
 (set! cycles (if (null? cycles) 128 (car cycles))) ; Set max cycles
 (letrec ((kitty (Avatar #f "Kat" (avatar 'z) (avatar 'y) (avatar 'x)))
          (happyVector (vector 0 0 0 0 0 0 0 0))
          (dist 0))
 ; Tell everyone who this kitteh is.
 (IpcWrite `(entity ,(kitty 'dna) 0 "kitty" ',((kitty 'gps))))
 (let ~ ((tic 0)) ; Main loop
   ; Distance from parent avatar
   (set! dist (distance ((kitty 'gps)) ((avatar 'gps))))
   ; Neuron depletion.
   (if (= (modulo tic 10) 0) (vector-map! (lambda (x) (/ x 2)) happyVector))
   ; Walk kitty quasi-randomly.
   ((kitty 'look) ; was walkDir
       (letrec ((dir (kitty 'dir))
                (dir1 (modulo (+ dir (random 3) -1) 8))
                (dir2 (modulo (+ dir (random 3) -1) 8)))
            (if (> (vector-ref happyVector dir1)
                   (vector-ref happyVector dir2))
                dir1 dir2)))
   (apply (kitty 'setLoc) ((kitty 'gpsLook)))
   ; Update neuron vector
   (vector-set! happyVector (kitty 'dir)
       (+ (let ((kd (distance ((kitty 'gps))
                              ((avatar 'gps)))))
            (if (< kd dist) 1
            (if (= kd dist) -1 -2)))
          (vector-ref happyVector (kitty 'dir))))
   (IpcWrite `(move ,(kitty 'dna) ,@((kitty 'gps))))
   (sleep 200)
   (if (equal? ((kitty 'gps)) ((avatar 'gps))) ; If avatar and kitty meet...
     (begin
        (IpcWrite `(voice ,(kitty 'dna) 10 "Hiss!"))
        (set! tic (+ cycles 30))))
   ; Kill entity or loop again
   (if (> tic (+ cycles (random 30)))
       (IpcWrite `(die ,(kitty 'dna)))
       (~ (+ tic 1))))))

(define march (let ((walkForeverFlag #f)) (lambda ()
 (if walkForeverFlag
  (begin
   (set! walkForeverFlag #f)
   (WinChatSetColor 0 10)
   (tankTalk "\r\nThus ends the journey"))
  (begin
   (WinChatSetColor 0 10)
   (tankTalk "\r\nThe journey begins")
   (set! walkForeverFlag #t)
   (thread (let ~ ()
     (for-each
       (lambda (x) (or walkForeverFlag (unthread)) (walk x) (sleep 400))
       '(0 0 0 0 2 2 2 2 4 4 4 4 6 6 6 6))
     (sleep 500)
     (~))))))))


; Tank agent - The first interactive user agent.
(define (tankTalk str)
 (thread
  (sleep 700)
  (WinChatSetColor 0 15) (WinChatDisplay "\r\nTank ")
  (WinChatSetColor 0 7)  (WinChatDisplay str)))

(define tankHangupTime 0)
(define tankIsListening #f)

(define (tankStartListening)
  (set! tankHangupTime (+ 10 (time)))
  (tankTalk "Operator")
  (if (not tankIsListening)
    (thread (let ~ ()
      (set! tankIsListening #t)
      (sleep 12000)
      (if (< (time) tankHangupTime)
        (~)
        (begin
          (tankTalk "*CLICK*")
          (set! tankIsListening #f)))))))

(define (tankTheOperator talkInput)
 (if (string=? talkInput "tank")
   (tankStartListening)
   (let ((strLen (string-length talkInput)))
    (if (and (> strLen 11) (string=? "my name is " (substring talkInput 0 11)))
      (thread (changeName (substring talkInput 11 strLen))))))
 (if tankIsListening (begin
   (if (string=? "who" talkInput) (IpcWrite '(say "I'm here!")))
   (if (string=? "load the jump program" talkInput) (tankTalk "I can't find the disk")
   (if (string=? "march" talkInput) (thread (march))
   (if (string=? "edit" talkInput) (begin (set! EDIT (not EDIT)) (tankTalk "Edit mode " EDIT))
   (if (string=? "island" talkInput) (jump 1 4150 5602)
   (if (string=? "scrabble" talkInput) (jump 1 3338 3244)
   (if (string=? "britania" talkInput) (jump 1 3456 2751))))))))))

; Display the same string repeatedly with colors of increasing inensity.
(define (fancyDisplay c s)
 (for-each
   (lambda (c)
        (WinChatSetColor 0 c)
        (WinChatDisplay "\r" s)
        (sleep 1))
   (list 8 4 12 6 7 14 15 14 7 6 12 4 8 c))
 "")

(define (chooseCell)
 (define WinCells ((Terminal 'WindowNew) 5 20 2 36 #x07))
 (define (WinCellsDisplay . e) (for-each (lambda (x) (for-each (WinCells 'puts) (display->strings x))) e))
 (define WinCellsSetColor  (WinCells 'set-color))
 (define WinCellsPutc  (WinCells 'putc))
 (sleep 1000)
 (loop 100 (lambda (k)
   ((WinCells 'home))
   (loop 10 (lambda (i)
     (let ((c (cellGlyph (cellRef (+ i k)))))
       (WinCellsSetColor (glyph0bg c) (glyph0fg c)) (WinCellsPutc (glyph0ch c))
       (WinCellsSetColor (glyph1bg c) (glyph1fg c)) (WinCellsPutc (glyph1ch c))
       (WinCellsSetColor 0 15)                      (WinCellsPutc (if (= i 4) #\[ (if (= i 5) #\] #\ ))))))
   (WinCellsSetColor 0 15)
   (WinCellsDisplay "\r\n" (cellSymbol (cellRef k)))
   (sleep 500)))
 ((WinCells 'delete)))

; Pacman

(define pacmanOn #f)
(define desiredDir 'ghost)

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

; Given a direction, return a list of possible directions (same direction, left or right)
; after filtering out non-obstructed directions
(define (pacmanFilterDirections l)
  (filter-not
    (lambda (d)
      ((avatar 'look) d)
      (cellSolid? (cellRef (apply mapBaseCell ((avatar 'gpsLook))))))
    l))

; Generate list of possible directions for a ghost given a direction
(define (pacmanNewGhostDirections dir)
  (pacmanFilterDirections
    (list (modulo (- dir 2) 8) ; Start with three direction: left current right
                  dir
                  (modulo (+ dir 2) 8))))

; Generate list with desired and/or current pacman directions
(define (pacmanNewPacmanDirections dir)
  (pacmanFilterDirections (list desiredDir dir)))

; Pacman thread
(define pacman (let ((dir 0)) (lambda ()
  (if pacmanOn
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
  (set! pacmanOn (not pacmanOn)) ; Call again to disable
  (thread (let ~ () (if pacmanOn (begin
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

; Given a vector on the cartesian plane in quadrant 1 between slope
; 0 and 1, return list of Bresenham Y increments which walk the line
; along X.  y must be <= x.
(define (lineIncrements y x stepDir incDir)
  (letrec ((yy (+ y y))  (yy-xx (- yy (+ x x))))
    (let ~ ((i x)  (e (- yy x)))
      (if (= i 0) ()
      (if (< 0 e) (cons incDir  (~ (- i 1) (+ e yy-xx)))
                  (cons stepDir (~ (- i 1) (+ e yy))))))))

; \2|1/  Return list of avatar movements
;_3\|/0_ required to walk from one point
; 4/|\7  another.
; /5|6\
(define (lineWalks y0 x0 y1 x1)
 (letrec ((y (- y1 y0))
          (x (- x1 x0))
          (ay (abs y))
          (ax (abs x)))
  (if (< ay ax)
    (if (< 0 x) ; linear X axis movement
      (if (< 0 y)
        (lineIncrements ay ax 0 7)      ; 7
        (lineIncrements ay ax 0 1))     ; 0
      (if (< 0 y)
        (lineIncrements ay ax 4 5)      ; 4
        (lineIncrements ay ax 4 3)))    ; 3
    (if (< 0 y) ; linear Y axis movement
      (if (< 0 x)
        (lineIncrements ax ay 6 7)      ; 6
        (lineIncrements ax ay 6 5))     ; 5
      (if (< 0 x)
        (lineIncrements ax ay 2 1)      ; 1
        (lineIncrements ax ay 2 3)))))) ; 2


(rem define pong
 (let ((power #f)
       (ball (Avatar #f "()" 0 0 0))
       (oy 0) ; Origin of this map block
       (ox 0)
       (m 0)
       (n 0))
 (lambda ()
  (set! power (not power))
  (WinChatDisplay "\r\nPong " (if power "enabled.  Press 2 to disable." "disabled"))
  ((ball 'setLoc) (avatar 'z) (avatar 'y) (avatar 'x))
  (set! oy (* (/ (avatar 'y) MapBlockSize) MapBlockSize)) ; Origin of this map block
  (set! ox (* (/ (avatar 'x) MapBlockSize) MapBlockSize))
  (IpcWrite `(entity ,(ball 'dna) ,(ball 'port) ,(ball 'name) ',((ball 'gps))))
  (sleep 500)
  (let ~ ((wall 0)) (if power (begin
    (if (= wall 0) (begin (set! m (random MapBlockSize)) (set! n (- MapBlockSize 1)))
    (if (= wall 1) (begin (set! m 0)                     (set! n (random MapBlockSize)))
    (if (= wall 2) (begin (set! m (random MapBlockSize)) (set! n 0))
    (if (= wall 3) (begin (set! m (- MapBlockSize 1))    (set! n (random MapBlockSize)))))))
    (let ~ ((l (lineWalks (ball 'y) (ball 'x) (+ oy m) (+ ox n))))
      (if (pair? l) (begin
        ((ball 'look) (car l))
        ; Is there something there?
        (if (= cellAIR (apply mapFirstCell ((ball 'gpsLook))))
          (begin
            (apply moveEntity ball ((ball 'gpsLook)))
            (IpcWrite (list 'move (ball 'dna) (ball 'z) (ball 'y) (ball 'x)))
            (sleep 100)
            (if power (~ (cdr l))))
          (set! wall (+ 1 wall))))))
    (~ (modulo (+ wall 1) 4)))))))) ; pong

; Mess with creating new entities using new Avatar oject
(define (makeGhost)
  (let ((a (Avatar ipc "Ghost" (avatar 'z) (avatar 'y) (avatar 'x))))
    (entitiesAdd a)
    (thread
      (sleep 2000)
      (moveEntity a (avatar 'z) (avatar 'y) (+ 1 (avatar 'x))))
    a)) ; Move avatar to entrance of Lord British's castle near 108 86

; Load a map file and dump in the current map
;(define (p m) (mapUpdateColumns 3456 2752 32 (read (open-file m))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Genesis
;;
(load "ultima4.cells")
(load "scrabble.scm") ; TODO temporary

; Create ipc object.  Pass in a debug message output port (can be empty lambda)
(define ipc (Ipc WinConsoleDisplay))
(ipc '(set! Debug #f))
(define IpcWrite (ipc 'qwrite))

; Start keyboard and mouse reader agents
(thread (keyScannerAgentLoop))

; Initialize map object
(define myMap (Map 256 ipc))
(define myViewport (myMap 'myViewport)) ; Genesis moveEntity handleTerminalResize mouseWalkActionHandler winMapBigger/move

(define mapDelEntitySprite (myMap 'delEntitySprite)) ; entitiesSet die
(define mapMoveObject      (myMap 'moveObject)) ; moveEntity entity
(define mapFirstCell       (myMap 'fieldFirstCell)) ; dumpColumnInfo walk fall Pong
(define mapBaseCell        (myMap 'baseCell)) ; dumpColumnInfo walk grab pacman
(define mapViewportReset   (myMap 'viewportReset)) ; winMapResize ^L Genesis
(define mapViewportRender  (myMap 'viewportRender)) ; entity Die
(define mapCanvasReset     (myMap 'canvasReset)) ; walkDetails
(define mapCanvasHeight    (myMap 'canvasHeight)) ; die
(define mapCanvasRender    (myMap 'canvasRender)) ; entity die

; Register windows and action handlers for mouse events
(mouseDispatcherRegister myViewport mouseWalkActionHandler)
(mouseDispatcherRegister WinPalette mouseColorsActionHandler)

(or QUIETLOGIN (begin
 ; Welcome marquee
 (thread (welcome))
 ; Ask for name via text box
 (let ((name (boxInput "Enter your name")))
   (or (eq? name "") (set! NAME name)))))

; Avatar creation
; TODO does the private port make sense when there will be more than one entity/IPCreader?
(define avatar (Avatar ipc NAME 99 3456 2751))
((avatar 'IPCvoiceRegister) avatarVoiceHandler)

(entitiesAdd avatar) ; TODO entity store concept needs to be redone

(set! DNA (avatar 'dna)) ; Copy my avatar's DNA value to the global variable

(define (setSprite x)
 (IpcWrite (list 'entity DNA
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

(moveEntity avatar 1 3456 2751) ; Move avatar to entrance of Lord British's castle near 108 86
;(mapViewportReset    3455 2751) ; Once had to reset the viewport to initialize synchronize avatar location

; Display some initial information
(or QUIETLOGIN (begin
 (fancyDisplay 5 "Welcome to World")
 (WinChatSetColor 0 3)
 (WinChatDisplay "\r\nSee http://code.google.com/p/worldtm")
 (WinChatSetColor 0 9)
 (WinChatDisplay "\r\nHit ? to toggle the help window")
 (WinChatSetColor 0 10)
 (WinChatDisplay (string "\r\nYour name is " (avatar 'name)))))

; Redraw map resulting in animated cells.
(thread ((myViewport 'animationAgentLoop)))

(define (shutdown)
  (or QUIETLOGIN (sayByeBye))
  ((avatar 'die)) ; Announce through IPC that I'm going away
  (sleep 1000) ; wait for ipc to flush
  (displayl "\e[" (Terminal 'Theight) "H\r\n\e[0m\e[?25h\e[?1000l")
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
;(IpcWrite '(who))

(wrepl)
(shutdown)
