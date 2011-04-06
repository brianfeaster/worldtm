;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glyphs - Two multi-colored characters.
;;

; Need to accept any number of objects since rest of vector could be other proper-glyphs.
(define Glyph vector)
;(define (Glyph bg0 fg0 ch0 bg1 fg1 ch1) (vector bg0 fg0 ch0 bg1 fg1 ch1))

(define (glyph0bg cell) (vector-ref cell 0))
(define (glyph0fg cell) (vector-ref cell 1))
(define (glyph0ch cell) (vector-ref cell 2))
(define (glyph1bg cell) (vector-ref cell 3))
(define (glyph1fg cell) (vector-ref cell 4))
(define (glyph1ch cell) (vector-ref cell 5))

(define glyphUNKNOWN (Glyph 0 8 #\? 0 8 #\?))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A sprite object which is a static grid of glyphs and #f
;; TODO list of glyph arrays as animation frames
(define (Sprite height width glyphArray) ; height, width, vector of glyphs/#f
 (define (self msg) (eval msg))
 (define (serialize) (list 'Sprite height width glyphArray))
 (define coordinates ()) ; Eventually a list of non-false relative glyph coordinates
 (define glyphCount (vector-length glyphArray))

 (define (glyphRef y x)
   (vector-ref glyphArray (modulo (+ (* y width) x) glyphCount)))

 ; Initialize the list of relative glyph coordinates.  Constructed in reverse
 ; so reversed as the last step.
 (loop2 0 height 0 width (lambda (y x)
  (let ((g (glyphRef y x)))
    (if g (set! coordinates (cons (cons y x) coordinates))))))
 (set! coordinates (reverse coordinates))

 self)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An entity
;;
;; The entity's name defines the initial glyph, sprite and color.
;; A direction is 0-9 where 0=right, 2=up, ..., 7=down/right, 8=down and 9=up.
(define (Entity dna port name z y x)
 (define (self msg) (eval msg))
 (define (inherit args macro) (apply macro args))
 (define glyph (Glyph 0 15 (string-ref name 0)
                      0 15 (string-ref name (if (< 1 (string-length name)) 1 0))))
 (define sprite (Sprite 1 1 (vector glyph)))
 (define color 15)
 (define oz z) ; Old location after an entity moves/(z y x) is changed
 (define oy y)
 (define ox x)
 (define dirFace 0) ; Direction entity is facing
 (define ftz 0) ; Facing translation coordinates added to facing location
 (define fty 0)
 (define ftx 0)
 (define dirLook 0) ; Direction entity is looking
 (define ltz 0) ; Look translation coordinates added to look location
 (define lty 0)
 (define ltx 0)
 (define (setPort port0)     (set! port port0))
 (define (setName name0)     (set! name name0))
 (define (setGlyph glyph0)   (set! glyph glyph0))
 (define (setSprite sprite0) (set! sprite sprite0))
 (define (setColor color0)   (set! color color0))
 (define (setLoc z0 y0 x0)   (set! oz z) (set! oy y) (set! ox x)
                             (set! z z0) (set! y y0) (set! x x0))
 (define (face d . tloc)
   (set! dirFace d)
   (if (pair? tloc)
     (begin
       (set! ftz (car tloc))
       (set! tloc (cdr tloc)))
     (set! ftz 0))
   (if (pair? tloc)
     (begin
       (set! fty (car tloc))
       (set! tloc (cdr tloc)))
     (set! fty 0))
   (if (pair? tloc)
     (set! ftx (car tloc))
     (set! ftx 0)))
 (define (look d . tloc) ; Look in a direction plus a cartesian (z y x) translation
   (set! dirLook d)
   (if (pair? tloc)
     (begin
       (set! ltz (car tloc))
       (set! tloc (cdr tloc)))
     (set! ltz 0))
   (if (pair? tloc)
     (begin
       (set! lty (car tloc))
       (set! tloc (cdr tloc)))
     (set! lty 0))
   (if (pair? tloc)
     (set! ltx (car tloc))
     (set! ltx 0)))
 ; Return location entity is currently located
 (define (gps) (list z y x))
 ; Return location entity is immediatley facing
 (define (gpsFace . d)
  (let ((dir (if (null? d) dirFace (car d))))
   (if (= dir 0) (list (+ z    ftz) (+ y    ftx) (+ x  1 ftx))
   (if (= dir 1) (list (+ z    ftz) (+ y -1 ftx) (+ x  1 ftx))
   (if (= dir 2) (list (+ z    ftz) (+ y -1 ftx) (+ x    ftx))
   (if (= dir 3) (list (+ z    ftz) (+ y -1 ftx) (+ x -1 ftx))
   (if (= dir 4) (list (+ z    ftz) (+ y    ftx) (+ x -1 ftx))
   (if (= dir 5) (list (+ z    ftz) (+ y  1 ftx) (+ x -1 ftx))
   (if (= dir 6) (list (+ z    ftz) (+ y  1 ftx) (+ x    ftx))
   (if (= dir 7) (list (+ z    ftz) (+ y  1 ftx) (+ x  1 ftx))
   (if (= dir 8) (list (+ z -1 ftz) (+ y    ftx) (+ x    ftx))
   (if (= dir 9) (list (+ z  1 ftz) (+ y    ftx) (+ x    ftx))))))))))))))
 ; Return location entity is looking at.  Includes a direction and relative translation
 (define (gpsLook . d)
  (let ((dir (if (null? d) dirLook (car d))))
   (if (= dir 0) (list (+ z    ltz) (+ y    lty) (+ x  1 ltx))
   (if (= dir 1) (list (+ z    ltz) (+ y -1 lty) (+ x  1 ltx))
   (if (= dir 2) (list (+ z    ltz) (+ y -1 lty) (+ x    ltx))
   (if (= dir 3) (list (+ z    ltz) (+ y -1 lty) (+ x -1 ltx))
   (if (= dir 4) (list (+ z    ltz) (+ y    lty) (+ x -1 ltx))
   (if (= dir 5) (list (+ z    ltz) (+ y  1 lty) (+ x -1 ltx))
   (if (= dir 6) (list (+ z    ltz) (+ y  1 lty) (+ x    ltx))
   (if (= dir 7) (list (+ z    ltz) (+ y  1 lty) (+ x  1 ltx))
   (if (= dir 8) (list (+ z -1 ltz) (+ y    lty) (+ x    ltx))
   (if (= dir 9) (list (+ z  1 ltz) (+ y    lty) (+ x    ltx))))))))))))))
 self)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An entity database.  List of entity/avatar objects.
;; TODO use hashtable
;; TODO port to map.scm
;;
; Association list of entites to their DNA values.
(define (EntityDB)
 (define (self msg) (eval msg))
 (define lst (ListCreate))
 (define (add ent) (ListAdd lst ent))
 (define (del ent) (ListDel lst ent))
 (define (getAll) (ListGet lst)) ; Map.updateColumnsIPC
 ; Lookup entity in database by scaning list for entity with the specified dna
 ; Return entity object or #f
 ; TODO implement hash.
 (define (get dnaIpc)
   (let ~ ((lst (getAll))) ; (car lst) is an entity object
     (cond ((null? lst) #f)
           ((eqv? dnaIpc ((car lst) 'dna)) (car lst))
           (else (~ (cdr lst))))))
 (define (set dna . args)
   (let ((entity (get dna)))
     (if entity
       ; Update name, glyph, port and sprite if entity already exists
       (each-for args
         (lambda (a)
           (cond ((integer? a)
                   ((entity 'setPort) a))
                 ((string? a)
                   ((entity 'setName) a))
                 ((vector? a)
                   ((entity 'setGlyph) a)
                   ; If the sprite is also a single glyph, update it as well
                   (if (= 1 ((entity 'sprite) 'glyphCount))
                       ((entity 'setSprite) (Sprite 1 1 (vector a)))))
                 ((and (pair? a) (eq? (car a) 'Sprite))
                   ((entity 'setSprite) (eval a))))))
       ; Create a new entity.  Massage the arguments (port name glyph (x y z))->(port name glyph x y z)
       (begin
         (set! entity (apply Entity dna (let ~ ((args args))
                                          (if (pair? (car args))
                                              (car args)
                                              (cons (car args) (~ (cdr args)))))))
         (add entity)))
     ; Return the new or modified entity
     entity))
 ; MAIN
 self) ; EntityDB
