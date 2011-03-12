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
; A sprite object which is a static grid of glyphs and #f
; TODO list of glyph arrays as animation frames
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
; An entity
;
; The entity's name defines the initial glyph, sprite and color.
(define (Entity dna port name z y x)
 (define (self msg) (eval msg))
 (define (inherit args macro) (apply macro args))
 (define glyph (Glyph 0 15 (string-ref name 0)
                      0 15 (string-ref name (if (< 1 (string-length name)) 1 0))))
 (define sprite (Sprite 1 1 (vector glyph)))
 (define color 15)
 (define oz z)
 (define oy y)
 (define ox x)
 (define (setPort port0)     (set! port port0))
 (define (setName name0)     (set! name name0))
 (define (setGlyph glyph0)   (set! glyph glyph0))
 (define (setLoc z0 y0 x0)   (set! oz z) (set! oy y) (set! ox x)
                             (set! z z0) (set! y y0) (set! x x0))
 (define (setSprite sprite0) (set! sprite sprite0))
 (define (setColor color0)   (set! color color0))
 (define (gps) (list z y x))
 self)
