;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; World
;;   Windows
;;    Glyphs
;;    Cells
;;    Column
;;    Field
;;    Canvas
;;    Viewport
;;   Map_manipulation
;;   Entites_and_avatar
;;    Window_functions_and_initialization
;;   Button_commands
;;   Buttons
;;    Incomming_IPC_messages
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
(define WinColumn ((Terminal 'WindowNew) 3 (- (Terminal 'Twidth) 5) 18 5 #x5b))
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
(define (cellSolid? cell) (vector-ref cell 1))
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

; Is the cell index a visible cell?  Anything but an entity and air.
; Air is CellMax value.  Maybe it should be 0?
(define (cellVisible? c)
 (and (<= 0 c) (< c CellMax)))


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

; Fields are vector-vectors of columns.  Columns are quasi-compressed stacks
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
(define (field-ref-top z y x)
 (letrec ((column (fieldColumn y x))
          (top    (columnHeightTop column)) ;1st implicit top cell in column
          (bot    (+ (columnHeightBottom column) 1)));1st implicit bottom cell in col
 ; Adjust the z coor down to the first explicit cell in the column
 (let findNonAir~ ((z (if (<= top z) (- top 1) z)))
   (if (or (!= (columnRef column z) cellAIR)
           (<= z bot))
       z ; Return first in improper list of objects.
       (findNonAir~ (- z 1))))))

; Scan up map column starting at z for first visibile cell.  Return height.
(define (field-ceiling z y x)
 (letrec ((column (fieldColumn y x))
          (top    (field-ref-top 100 y x))) ; top most visible cell in column
 (let ~ ((i (+ z 1)))
   (if (containsVisibleCell? (columnRef column i)) i
   (if (<= top i) 100
   (~ (+ i 1)))))))

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
;; A canvas entry is a vector-vector consisting of a glyph and it's Z
;; coordinate (the top most visible cell relative to the user usually).
;;
(define CANVAS (make-vector-vector FieldSize FieldSize #f))

; Initialize the canvas which is a vector of pairs (cellGlyph . cellHeight)
(define (canvasReset top . defaultGlyph)
  (set! defaultGlyph (if (null? defaultGlyph) #f (car defaultGlyph)))
  (loop2 0 FieldSize 0 FieldSize (lambda (y x)
    ; Each canvas entry consists of a map cell and its height.
    (vector-vector-set! CANVAS y x
      (if defaultGlyph
        ; default pair
        (cons defaultGlyph 0)
        ; pair based on visible cell in field
        (letrec ((t (field-ref-top top y x))
                 (celli (field-ref t y x)))
          (cons (if (< CellMax celli)
                    (((entitiesGet celli) 'glyph))
                    (cellGlyph (cellRef celli)))
                t)))))))

(define (canvasGlyph y x)
  (car (vector-vector-ref CANVAS
         (modulo y FieldSize)
         (modulo x FieldSize))))

(define (canvasHeight y x)
  (cdr (vector-vector-ref CANVAS
         (modulo y FieldSize)
         (modulo x FieldSize))))

(define (canvasGlyphSet y x c)
  (set-car! (vector-vector-ref CANVAS
              (modulo y FieldSize)
              (modulo x FieldSize))
            c))

(define (canvasHeightSet y x h)
 (set-cdr! (vector-vector-ref CANVAS
             (modulo y FieldSize)
             (modulo x FieldSize))
           h))

(define (canvasRender top y x)
 (let ((z (field-ref-top top y x))) ; Get z of first cell starting at top
   (let ((celli (field-ref z y x))) ; Field might contain an entity's dna
     (canvasGlyphSet y x (if (< CellMax celli)
                             (((entitiesGet celli) 'glyph))
                             (cellGlyph (cellRef celli)))))
   (canvasHeightSet y x z)))

(define (canvasRenderEntity top y x)
 (let ((z (field-ref-top top y x))) ; Get z of first cell starting at top
   (let ((celli (field-ref z y x))) ; Field might contain an entity's dna
     (canvasGlyphSet y x (if (< CellMax celli)
                             (letrec ((ent (entitiesGet celli))
                                      (sprite (ent 'sprite))
                                      (ey (ent 'y))
                                      (ex (ent 'x)))
                               ((sprite 'glyphRef) (- y ey) (- x ex)))
                             (cellGlyph (cellRef celli)))))
   (canvasHeightSet y x z)))



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
  ; The glyph index is based on the time which cycles to the next one every second.
  (let ((t (time)) ; Glyph index based on time
        (l (- (vector-length glyph) 5))) ; Number of animation glyphs ignoring the first
    (if (> (modulo t l) 0) (set! glyph (vector-ref glyph (+ 5 (modulo t l)))))
    ((WinMap 'goto) y x)
    ; 1st char
    (WinMapSetColor (glyph0bg glyph) (glyph0fg glyph))
    (WinMapPutc (glyph0ch glyph))
    ; 2nd char
    (WinMapSetColor (glyph1bg glyph) (glyph1fg glyph))
    (WinMapPutc (glyph1ch glyph))))

; Time of last viewport refresh used by the refresh thread to
; skip if an avatar movement has caused one within a set time
(define VIEWPORTREDRAWTIME (time))

(define (viewportReset y x)
 (set! VIEWPORTREDRAWTIME (time))
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
 ((Terminal 'unlock))) ; This shouldn't be such an all encompasing lock.


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
 (let ((y (modulo (- gy PortY) FieldSize)) ; Normalize avatar position.
       (x (modulo (- gx PortX) FieldSize)))
  (and (< y PortH) (< x PortW) (begin
    ((Terminal 'lock)) ; This shouldn't be such an all encompasing lock.
    (viewportPlot (canvasGlyph gy gx) y (* x 2))
    ((Terminal 'unlock)))))) ; This shouldn't be such an all encompasing lock.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map_manipulation
;;
;; Calls to edit the World "map" which is made up of Glyphs, Cells,
;; Columns, a Field, a Canvas and a Viewport.

; Drop a cell on the map
(define (mapDropCell y x cell)
 (let ((z (+ 1 (field-ref-top 100 y x))))
  (field-set! z y x cell)
  (canvasRender 100 y x)
  (viewportRender y x)))

; Set map cell and force rendering of cell through entire pipeline.
(define (mapSetCell z y x cell)
  (field-set! z y x cell)
  (canvasRender 100 y x)
  (viewportRender y x))

; Delete a cell from the field.  Update canvas if needed.
(define (mapDelCell cell z y x)
  (field-delete! z y x cell)
  (if (>= z (canvasHeight y x)) (begin
    (canvasRender (avatar 'ceiling) y x)
    (viewportRender y x))))

; Add a cell to the field.  Update canvas if needed.
(define (mapAddCell cell z y x)
  (field-add! z y x cell)
  (if (>= z (canvasHeight y x)) (begin
    (canvasRender (avatar 'ceiling) y x)
    (viewportRender y x))))

(define (mapMoveCell cell zo yo xo z y x)
  ; Old location removal
  (mapDelCell cell zo yo xo)
  ; New location added
  (mapAddCell cell z y x))

(define (mapDelEntitySprite dna zo yo xo)
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
          (WinConsoleDisplay (list m n))
          ; First update field
          (field-delete! zo (+ m yo) (+ n xo) dna)
          ; Render old deleted location
          (if (>= zo (canvasHeight (+ m yo) (+ n xo)))
           (begin
             (canvasRenderEntity (avatar 'ceiling) (+ m yo) (+ n xo))
             (viewportRender (+ m yo) (+ n xo)))))))))) ; Don't render cell if viewport to be reset

(define (mapMoveEntitySprite dna zo yo xo z y x centerMap)
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
                    (field-delete! zo (+ m yo) (+ n xo) dna) ; Old
                    (field-add! z (+ m y) (+ n x) dna))))) ; New
    ; Second update canvas and viewport
    (each-for coordinates
      (lambda (c)
        (if c (let ((m (car c))
                    (n (cdr c)))
          ; Render old deleted location
          (if (>= zo (canvasHeight (+ m yo) (+ n xo))) (begin
            (canvasRenderEntity (avatar 'ceiling) (+ m yo) (+ n xo))
            (or centerMap (viewportRender (+ m yo) (+ n xo))))) ; Don't render cell if viewport to be reset
          ; Render new added location
          (if (>= z (canvasHeight (+ m y) (+ n x))) (begin
            (canvasRenderEntity (avatar 'ceiling) (+ m y) (+ n x))
            (or centerMap (viewportRender (+ m y) (+ n x)))))))))) ; Don't render cell if vewport to be reset
  (if centerMap (viewportReset y x))
  (if (WinColumn 'ENABLED) (dumpColumnInfo y x)))


; Given a cell index or entity DNA value, move it in the field, canvas and viewport.
(define MAPMOVECELLSEMAPHORE (open-semaphore 1))

(define (mapMoveObject cell zo yo xo z y x centerMap)
  (semaphore-down MAPMOVECELLSEMAPHORE)
  (if (< CellMax cell)
    (mapMoveEntitySprite cell zo yo xo z y x centerMap)
    (mapMoveCell         cell zo yo xo z y x))
  (semaphore-up MAPMOVECELLSEMAPHORE))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entites_and_avatar
;; Simple objects more dynamic than just cells.
;;

; Association list of entites to their DNA values.
(define EntityDB ())

(define (entitiesAdd entity)
 (set! EntityDB (cons (cons (entity 'dna) entity) EntityDB)))

; Lookup entity in database.
(define (entitiesGet dna)
  (let ((entity (assv dna EntityDB)))
    (if (null? entity) #f (cdr entity))))

(define (entitiesSet dna . args)
  (let ((entity (entitiesGet dna)))
    (if entity
      (each-for args ; Update only name and glyph if entity already exists
        (lambda (a)
          (if (integer? a)   ((entity 'setPort) a)
          (if (string? a)    ((entity 'setName) a)
          (if (procedure? a) (begin
                (mapDelEntitySprite dna (entity 'z) (entity 'y) (entity 'x)) ; Remove it first from the map
                ((entity 'setSprite) a))))))) ; TODO BF sprite update mechanism stinks
      (begin
        ; Create a new entity.  Massage the arguments (port name glyph (x y z))->(port name glyph x y z)
        (set! entity (apply Entity dna (let ~ ((args args))
                                         (if (pair? (car args)) (car args)
                                             (cons (car args) (~ (cdr args)))))))
        (entitiesAdd entity)))
    ; Return the new or modified entity
    entity))

; The user's avatar.  An extended entity object that includes positioning and directional observation vectors
(define (Avatar port name sprite z y x) ; Inherits Entity
 ((Entity (random) port name sprite  z y x)
  `(let () ; This let block is passed in as the inheriting child object
    (define (self msg) (eval msg))
    (define cell 19) ; TODO replace with a generalize item container
    (define dir 0)
    (define tz 0) ; Translation coordinates 
    (define ty 0)
    (define tx 0)
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
    self)))

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
                           (< (- (/ (WinMap 'Wheight) 2) 2)
                              (distance (list 0 y x)
                                        (list 0 (+ PortY (/ PortH 2)) (+ PortX (/ PortW 2))))))))))) 



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
  (let ((c (field-ref z y x)))
   (if (eqv? cellAIR c)
    (begin (WinColumnSetColor 0 8)
           (WinColumnPuts "()   "))
    (begin (set! c (if (< CellMax c) (((entitiesGet c) 'glyph))
                                     (cellGlyph (cellRef c)))) ; Dump the glyph
           (WinColumnSetColor (glyph0bg c) (glyph0fg c))
           (WinColumnPutc (glyph0ch c))
           (WinColumnSetColor (glyph1bg c) (glyph1fg c))
           (WinColumnPutc (glyph1ch c))
           (WinColumnSetColor 0 7)
           (set! c (field-base-ref z y x)) ; Display base cell's hex value.
           (if (and (<= 0 c) (< c CellMax))
             (begin
               (if (< c 256) (WinColumnPuts "0"))
               (if (< c 16) (WinColumnPuts "0"))
               (WinColumnPuts (number->string c 16)))
             (WinColumnPuts "   ") ))))
  (if (> z -6) (~ (- z 1)))))

; Continuously redraw the viewport for cell animation.  Should be called in its own thread.
(define (viewportAnimationLoop)
   (sleep 1000)
   (and VIEWPORTANIMATION
        (< 0 (- (time) VIEWPORTREDRAWTIME))
        (viewportReset PortCY PortCX))
   (viewportAnimationLoop))


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

(define (winMapUp)   ((WinMap 'move) (+ -1 (WinMap 'Y0))      (WinMap 'X0)))
(define (winMapDown) ((WinMap 'move) (+  1 (WinMap 'Y0))      (WinMap 'X0)))
(define (winMapLeft) ((WinMap 'move)       (WinMap 'Y0) (+ -1 (WinMap 'X0))))
(define (winMapRight)((WinMap 'move)       (WinMap 'Y0) (+  1 (WinMap 'X0))))

(define (walkDetails)
  ; Update avatar locally in the field/canvas/viewport and via IPC
  (apply moveEntity avatar ((avatar 'gpsLook)))
  (ipcWrite (list 'move DNA (avatar 'z) (avatar 'y) (avatar 'x)))
  (let ((baseSym (cellSymbol (cellRef (apply field-base-ref ((avatar 'gps)))))))
    (if (eq? baseSym 'sprite0) (setSprite 0)
    (if (eq? baseSym 'sprite1) (setSprite 1)
    (if (eq? baseSym 'sprite2) (setSprite 2)))))
  ; If ceiling changes, repaint canvas using new ceiling height
  (let ((oldCeiling (avatar 'ceiling)))
    ((avatar 'setCeiling) (- (apply field-ceiling ((avatar 'gps))) 1))
    (if (!= oldCeiling (avatar 'ceiling))
      (thread (canvasReset (avatar 'ceiling))))))

;(if (eq? 'help  (cellSymbol (field-ref (avatar 'z) (avatar 'y) (avatar 'x))))  (help))
;(if (eq? 'snake (cellSymbol (field-ref (avatar 'z) (avatar 'y) (avatar 'x)))) (thread (snake-random)))
;(if (eq? 'brit2 (cellSymbol (field-base-ref (avatar 'z) (avatar 'y) (avatar 'x)))) (thread (spawnKitty)))

(define walkSemaphore (open-semaphore 1))

(define (walk dir)
  (semaphore-down walkSemaphore)
    ; Consider cell I'm walking into.  If cell is entity push it.
    ; Otherwise move to facing cell or on top of obstructing cell.
    ((avatar 'look) dir)
    (let ((nextCell (apply field-ref ((avatar 'gpsLook)))))
      ; Case 1 push entity
      (if (and #f (< CellMax nextCell))
        (ipcWrite `(force ,@((avatar 'gpsLook)) ,dir 10))
      ; Case 2 walk normally
      (if (or #t (or EDIT (not (cellSolid? (cellRef nextCell))))) ; TODO this always true
        (walkDetails)
      ; Case 3 step up
      (begin
        ((avatar 'look) dir 1) ; Peek at the cell above the one in front of me
        (set! nextCell (apply field-base-ref ((avatar 'gpsLook))))
        (or (cellSolid? (cellRef nextCell))
          (walkDetails))))))
    ; Gravity
    (or EDIT (fall))
  (semaphore-up walkSemaphore))

; Fall down one cell if a non-entity and non-solid cell below me
(define (fall)
 ((avatar 'look) 8) ; Look down
 (let ((nextCell (apply field-ref ((avatar 'gpsLook)))))
   (if (= nextCell CellMax)
     (begin
       (walkDetails)))))

; Change avatar color.  Will just cycle through all 16 avatar colors.
(define (changeColor) ; TODO BF: fix this shite
 (let ((glyph (avatar 'glyph)))
   (ipcWrite (list 'entity DNA 
                    (glyphNew
                      (glyph0bg glyph)  (modulo (+ (glyph0fg glyph) 1) 16)  (glyph0ch glyph)
                      (glyph1bg glyph)  (modulo (+ (glyph1fg glyph) 1) 16)  (glyph1ch glyph))))))

; Notify IPC of my name and glyph change
(define (changeName str)
  (ipcWrite
   (list 'entity DNA str
            (glyphNew (glyph0bg (avatar 'glyph))
                      (glyph0fg (avatar 'glyph))
                      (string-ref str 0)
                      (glyph1bg (avatar 'glyph))
                      (glyph1fg (avatar 'glyph))
                      (string-ref str (if (< 1 (string-length str)) 1 0)))))) ; Notify IPC of my name change

(define (rollcall)
 (ipcWrite ; Force all to evaluate the following
  `(if (!= DNA ,DNA) ; Skip if I sent this message
   (ipcWrite ; Force all (except me) to evaluate the following
    `(if (= DNA ,,DNA) ; If me, evaluate this expression from the other peer
     (voice 0 10
      (string ,(avatar 'name) " "
              ,(let ((t (- (time) ActivityTime)))
                (if (< t 60)   (string (number->string t) "s")
                (if (< t 3600) (string (number->string (/ t 60)) "m")
                (if (< t 86400)(string (number->string (/ t 3600)) "h")
                (string (number->string (/ t 86400)) "d"))))))))))))

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
 ;((WinStatus 'toggle))

(define (buttonSetCell)
 (if PortMapAgent
   ; Send to map agent. If map agent doesn't respond
   ; then ignore it just send to everyone.
   (or ((ipc 'private) PortMapAgent `(setCellAgent ,(avatar 'z) ,(avatar 'y) ,(avatar 'x) ,(avatar 'cell)))
     (begin 
       (set! PortMapAgent #f)
       (buttonSetCell)))
   ; Send to everyone
   (ipcWrite `(mapSetCell ,(avatar 'z) ,(avatar 'y) ,(avatar 'x) ,(avatar 'cell)))))



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
(setButton #\C '(changeColor))
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
(setButton #\M '((WinMap 'toggle)))
(setButton #\t '(begin
  (WinInputPuts (string ">" (replTalk 'getBuffer)))
  (set! state 'talk)))
(setButton CHAR-CTRL-D '(ipc '(set! Debug (not Debug))))
(setButton CHAR-CTRL-E '(begin (set! EDIT (not EDIT)) (WinChatDisplay "\r\nEDIT " EDIT)))
(setButton CHAR-CTRL-L '(begin (viewportReset (avatar 'y) (avatar 'x)) ((WinChat 'repaint))))
(setButton CHAR-CTRL-M '(begin ((WinStatus 'toggle)) ((WinColumn 'toggle))))
(setButton CHAR-CTRL-Q '(set! state 'done))
(setButton #\d '(buttonSetCell))
(setButton #\D '(chooseCell))
(setButton #\g
   '(let ((o (apply field-base-ref ((avatar 'gps)))))
     (WinChatDisplay "\r\nGrabbed " o)
     (avatar `(set! cell ,o))))
(setButton #\? '(help))
(setButton #\< '(winMapSmaller))
(setButton #\> '(winMapBigger))
(setButton #\z '(circularize))
(setButton #\Z '(circularize #t))
(setButton #\Q '(set! state 'done))
(setButton #eof '(set! state 'done))
(setButton CHAR-CTRL-C '((WinConsole 'toggle)))
;(setButton CHAR-CTRL-K '(ipcWrite `(set! FIELD ,FIELD))) ; Send my plane out to IPC.
(setButton #\1 '(begin (set! state 'pacman) (pacman)))
(setButton #\2 '(thread (pong)))
(setButton #\3 '(thread (spawnKitty 1000)))
;(setButton #\1 '((WinChat 'resize) (WinChat 'Wheight) (WinChat 'Wwidth)))
;(setButton #\1 '((WinChat 'scrollUp)))
;(setButton CHAR-CTRL-_ '(walkDir 4)) ; Sent by backspace?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incomming_IPC_messages
;;
(define (who . dna) ; TODO handle explicit request from specified peer
  (ipcWrite
    `(entity ,DNA ,(avatar 'port) ,(avatar 'name) ,(((avatar 'sprite) 'serialize)) ',((avatar 'gps)))))

; Update an entity's location
(define (move dna z y x)
 (or (= dna DNA) ; Skip if this is me since map rendering is handled during movement handling
     (let ((entity (entitiesGet dna)))
       (if entity (moveEntity entity z y x)))))

; Update one or more of an entity's attribute: dna port name glyph z y x
(define (entity dna . args)
  (let ((entity (entitiesGet dna)))
    (if entity
      (begin ; Modify entity attributes
        (apply entitiesSet dna args)
        (mapMoveObject (entity 'dna) (entity 'z) (entity 'y) (entity 'x)
                                     (entity 'z) (entity 'y) (entity 'x) #f)
        (canvasRender (avatar 'ceiling) (entity 'y) (entity 'x))
        (viewportRender (entity 'y) (entity 'x)))
      (begin ; Create new entity with all args
        (set! entity (apply entitiesSet dna args))
        (mapMoveObject (entity 'dna) (entity 'z) (entity 'y) (entity 'x)
                                     (entity 'z) (entity 'y) (entity 'x) #f)))
    (if (= dna 17749) (set! PortMapAgent (entity 'port))))) ; The map agent's DNA number

(define (die dna)
 (let ((entity (entitiesGet dna))
       (thisIsMe (= dna DNA)))
  (if entity ; Ignore unknown entities
    (begin
      ; Remove from here
      (field-delete! (entity 'z) (entity 'y) (entity 'x) dna)
      (if (>= (entity 'z) (canvasHeight (entity 'y) (entity 'x))) (begin
        (canvasRender 100 (entity 'y) (entity 'x))
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
  (letrec ((entity (entitiesGet dna))
           (glyph ((entity 'glyph))))
    (WinChatSetColor (glyph0bg glyph) (glyph0fg glyph))
    (WinChatDisplay "\r\n" (if (null? entity) "???" (entity 'name)) VOICEDELIMETER)
    (WinChatSetColor (glyph1bg glyph) (glyph0fg glyph))
    (WinChatDisplay text)))
 (if (and (!= dna DNA) (eqv? text "unatco")) (say "no Savage")))

; The 2d vector of columns will most likely come from a map agent.
; The map block coordinate and map block size is also passed.
(define (mapUpdateColumns y x blockSize v)
  ;(WinChatDisplay "\r\nBlock " (list (/ y 32) (/ x 32)))
  (loop2 y (+ y blockSize) x (+ x blockSize)
    (lambda (y x)
      (vector-vector-set! FIELD (modulo y FieldSize) (modulo x FieldSize)
         (vector-vector-ref v (modulo y MapBlockSize) (modulo x MapBlockSize)))
      (canvasRender 100 y x))))



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
   (KeyAgent))) ; rinse and repeat

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
       (begin ; Not an arrow key sequence, so send all the character to the key queue
           (QueueAdd keyQueue CHAR-ESC) 
           (QueueAdd keyQueue #\[) 
           (QueueAdd keyQueue c) 
           (semaphore-up getKeySemaphore)
           (semaphore-up getKeySemaphore)
           (semaphore-up getKeySemaphore)))))))
     (begin ; Not a recognized escape sequence, so send escape and the character
       (QueueAdd keyQueue CHAR-ESC)
       (QueueAdd keyQueue c)
       (semaphore-up getKeySemaphore)
       (semaphore-up getKeySemaphore)))))

(define (getKey)
 (semaphore-down getKeySemaphore)
 (QueueGet keyQueue))


(define (say talkInput . level)
 (ipcWrite (list 'voice DNA (if (null? level) 10 (car level)) talkInput)))

(define (saySystem talkInput)
 (ipcWrite (list 'voice 0 10 talkInput)))

(define (sayHelloWorld)
 (saySystem (string
     (avatar 'name)
     " says "
     (vector-random #("*PUSH* *SQUIRT* *SPANK* *WAAAAAAAAA*" "*All Worldlians Want to Get Borned*" "*Happy Birthday*" "*I thought you were in Hong Kong*")))))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prototypes_and_fun_things
;;

;; Walking kitty soldier
(define (spawnKitty . cycles)
 (set! cycles (if (null? cycles) 128 (car cycles))) ; Set max cycles
 (letrec ((kitty (Avatar 0 "Kat" (glyphNew 0 7 #\K 0 15 #\a) (avatar 'z) (avatar 'y) (avatar 'x)))
          (happyVector (vector 0 0 0 0 0 0 0 0))
          (dist 0))
 ; Tell everyone who this kitteh is.
 (ipcWrite `(entity ,(kitty 'dna) 0 "kitty" ,(kitty 'glyph) ',((kitty 'gps))))
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
   (ipcWrite `(move ,(kitty 'dna) ,@((kitty 'gps))))
   (sleep 200)
   (if (equal? ((kitty 'gps)) ((avatar 'gps))) ; If avatar and kitty meet...
     (begin
        (ipcWrite `(voice ,(kitty 'dna) 10 "Hiss!"))
        (set! tic (+ cycles 30))))
   ; Kill entity or loop again
   (if (> tic (+ cycles (random 30)))
       (ipcWrite `(die ,(kitty 'dna)))
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
   (if (string=? "who" talkInput) (ipcWrite '(say "I'm here!")))
   (if (string=? "load the jump program" talkInput) (tankTalk "I can't find the disk")
   (if (string=? "march" talkInput) (thread (march))
   (if (string=? "edit" talkInput) (begin (set! EDIT (not EDIT)) (tankTalk "Edit mode " EDIT))))))))

; Display the same string repeatedly with colors of increasing inensity.
(define (fancyDisplay c s)
 (for-each
   (lambda (c)
        (WinChatSetColor 0 c)
        (WinChatDisplay "\r" s)
        (sleep 1))
   (list 8 4 12 6 7 14 15 14 7 6 12 4 8 c))
 "")

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
      (cellSolid? (cellRef (apply field-base-ref ((avatar 'gpsLook))))))
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

; Given a vector on the cartesian plane in quadrant one between slope
; 0 and 1, return list of Bresenham Y increments which walk the line
; along X.
; y <= x
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


(define pong
 (let ((power #f)
       (ball (Avatar 0 "ball" (Sprite 1 1 (vector (glyphNew 0 15 #\( 0  15 #\)))) 0 0 0))
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
  (ipcWrite `(entity ,(ball 'dna) ,(ball 'port) ,(ball 'name) ,(((ball 'sprite) 'serialize)) ',((ball 'gps))))
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
        (if (= cellAIR (apply field-ref ((ball 'gpsLook))))
          (begin
            (apply moveEntity ball ((ball 'gpsLook)))
            (ipcWrite (list 'move (ball 'dna) (ball 'z) (ball 'y) (ball 'x)))
            (sleep 100)
            (if power (~ (cdr l))))
          (set! wall (+ 1 wall))))))
    (~ (modulo (+ wall 1) 4))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Genesis
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
(canvasReset 100 glyphXX)

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
(define ipcReadQueue ((ipc 'newReader)))
(define ipcWrite (ipc 'qwrite))

; Create avatar object and add to entity list
(define avatar (Avatar
  (ipc 'PrivatePort)
  NAME
  (Sprite 1 1 (vector (glyphNew 0 15 #\: 0 15 #\))))
  99 3456 2751))

(define (setSprite x)
 (ipcWrite (list 'entity DNA 
  (if (= x 0)
    '(Sprite 1 1 #(#(0 15 #\S  0 15 #\h)))
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

(entitiesAdd avatar)
(set! DNA (avatar 'dna))
(moveEntity avatar 1 3456 2751) ; Move avatar to entrance of Lord British's castle near 108 86
(viewportReset       3456 2751) ; Must reset the viewport to initialize synchronize location with avatar

; Display some initial information
(or QUIETLOGIN (begin
 (fancyDisplay 5 "Welcome to World")
 (WinChatSetColor 0 3)
 (WinChatDisplay "\r\nSee http://code.google.com/p/worldtm")
 (WinChatSetColor 0 9)
 (WinChatDisplay "\r\nHit ? to toggle the help window")
 (WinChatSetColor 0 10)
 (WinChatDisplay (string "\r\nYour name is " (avatar 'name)))))

; Always read and evaluate everything from IPC.
(thread
 ; Set the thread's error handler to a continuation so any user or IPC scheme error is caught
 ; causing the thread to begin again from the let block.
 (let ((s (call/cc (lambda (c) (vector-set! ERRORS (tid) c) 'starting))))
    (or (eq? s 'starting) (WinChatDisplay "\r\nIPC-REPL-ERROR::" s)))
 (let ~ ()
  (let ((sexp (QueueGet ipcReadQueue)))
     (eval sexp)
     (~))))

; Redraw map resulting in animated cells.
(thread (viewportAnimationLoop))

((WinMap 'toggle))

(define (shutdown)
  (or QUIETLOGIN (sayByeBye))
  (ipcWrite `(die ,DNA)) ; Kill avatar's entity
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
(ipcWrite '(who))

(wrepl)
(shutdown)
