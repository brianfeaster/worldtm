;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Graphic Libraries
;;
;; ICO_files
;;

(define (Palette256Xterm->RGB i)
 (if (or (< i 0) (< 255 i))
     (begin (displayl "ERROR: (Palette256Xterm->RGB " i ") out of range")) 
     (vector-ref #(
; 16 basic  00-0f
#(#x00 #x00 #x00) #(#x80 #x00 #x00) #(#x00 #x80 #x00) #(#x80 #x80 #x00) #(#x00 #x00 #x80) #(#x80 #x00 #x80) #(#x00 #x80 #x80) #(#xc0 #xc0 #xc0)
#(#x80 #x80 #x80) #(#xff #x00 #x00) #(#x00 #xff #x00) #(#xff #xff #x00) #(#x00 #x00 #xff) #(#xff #x00 #xff) #(#x00 #xff #xff) #(#xff #xff #xff)
; 6x6x6 palette 10-e7
#(#x00 #x00 #x00) #(#x00 #x00 #x5f) #(#x00 #x00 #x87) #(#x00 #x00 #xaf) #(#x00 #x00 #xd7) #(#x00 #x00 #xff)
#(#x00 #x5f #x00) #(#x00 #x5f #x5f) #(#x00 #x5f #x87) #(#x00 #x5f #xaf) #(#x00 #x5f #xd7) #(#x00 #x5f #xff)
#(#x00 #x87 #x00) #(#x00 #x87 #x5f) #(#x00 #x87 #x87) #(#x00 #x87 #xaf) #(#x00 #x87 #xd7) #(#x00 #x87 #xff)
#(#x00 #xaf #x00) #(#x00 #xaf #x5f) #(#x00 #xaf #x87) #(#x00 #xaf #xaf) #(#x00 #xaf #xd7) #(#x00 #xaf #xff)
#(#x00 #xd7 #x00) #(#x00 #xd7 #x5f) #(#x00 #xd7 #x87) #(#x00 #xd7 #xaf) #(#x00 #xd7 #xd7) #(#x00 #xd7 #xff)
#(#x00 #xff #x00) #(#x00 #xff #x5f) #(#x00 #xff #x87) #(#x00 #xff #xaf) #(#x00 #xff #xd7) #(#x00 #xff #xff)

#(#x5f #x00 #x00) #(#x5f #x00 #x5f) #(#x5f #x00 #x87) #(#x5f #x00 #xaf) #(#x5f #x00 #xd7) #(#x5f #x00 #xff)
#(#x5f #x5f #x00) #(#x5f #x5f #x5f) #(#x5f #x5f #x87) #(#x5f #x5f #xaf) #(#x5f #x5f #xd7) #(#x5f #x5f #xff)
#(#x5f #x87 #x00) #(#x5f #x87 #x5f) #(#x5f #x87 #x87) #(#x5f #x87 #xaf) #(#x5f #x87 #xd7) #(#x5f #x87 #xff)
#(#x5f #xaf #x00) #(#x5f #xaf #x5f) #(#x5f #xaf #x87) #(#x5f #xaf #xaf) #(#x5f #xaf #xd7) #(#x5f #xaf #xff)
#(#x5f #xd7 #x00) #(#x5f #xd7 #x5f) #(#x5f #xd7 #x87) #(#x5f #xd7 #xaf) #(#x5f #xd7 #xd7) #(#x5f #xd7 #xff)
#(#x5f #xff #x00) #(#x5f #xff #x5f) #(#x5f #xff #x87) #(#x5f #xff #xaf) #(#x5f #xff #xd7) #(#x5f #xff #xff)

#(#x87 #x00 #x00) #(#x87 #x00 #x5f) #(#x87 #x00 #x87) #(#x87 #x00 #xaf) #(#x87 #x00 #xd7) #(#x87 #x00 #xff)
#(#x87 #x5f #x00) #(#x87 #x5f #x5f) #(#x87 #x5f #x87) #(#x87 #x5f #xaf) #(#x87 #x5f #xd7) #(#x87 #x5f #xff)
#(#x87 #x87 #x00) #(#x87 #x87 #x5f) #(#x87 #x87 #x87) #(#x87 #x87 #xaf) #(#x87 #x87 #xd7) #(#x87 #x87 #xff)
#(#x87 #xaf #x00) #(#x87 #xaf #x5f) #(#x87 #xaf #x87) #(#x87 #xaf #xaf) #(#x87 #xaf #xd7) #(#x87 #xaf #xff)
#(#x87 #xd7 #x00) #(#x87 #xd7 #x5f) #(#x87 #xd7 #x87) #(#x87 #xd7 #xaf) #(#x87 #xd7 #xd7) #(#x87 #xd7 #xff)
#(#x87 #xff #x00) #(#x87 #xff #x5f) #(#x87 #xff #x87) #(#x87 #xff #xaf) #(#x87 #xff #xd7) #(#x87 #xff #xff)

#(#xaf #x00 #x00) #(#xaf #x00 #x5f) #(#xaf #x00 #x87) #(#xaf #x00 #xaf) #(#xaf #x00 #xd7) #(#xaf #x00 #xff)
#(#xaf #x5f #x00) #(#xaf #x5f #x5f) #(#xaf #x5f #x87) #(#xaf #x5f #xaf) #(#xaf #x5f #xd7) #(#xaf #x5f #xff)
#(#xaf #x87 #x00) #(#xaf #x87 #x5f) #(#xaf #x87 #x87) #(#xaf #x87 #xaf) #(#xaf #x87 #xd7) #(#xaf #x87 #xff)
#(#xaf #xaf #x00) #(#xaf #xaf #x5f) #(#xaf #xaf #x87) #(#xaf #xaf #xaf) #(#xaf #xaf #xd7) #(#xaf #xaf #xff)
#(#xaf #xd7 #x00) #(#xaf #xd7 #x5f) #(#xaf #xd7 #x87) #(#xaf #xd7 #xaf) #(#xaf #xd7 #xd7) #(#xaf #xd7 #xff)
#(#xaf #xff #x00) #(#xaf #xff #x5f) #(#xaf #xff #x87) #(#xaf #xff #xaf) #(#xaf #xff #xd7) #(#xaf #xff #xff)

#(#xd7 #x00 #x00) #(#xd7 #x00 #x5f) #(#xd7 #x00 #x87) #(#xd7 #x00 #xaf) #(#xd7 #x00 #xd7) #(#xd7 #x00 #xff)
#(#xd7 #x5f #x00) #(#xd7 #x5f #x5f) #(#xd7 #x5f #x87) #(#xd7 #x5f #xaf) #(#xd7 #x5f #xd7) #(#xd7 #x5f #xff)
#(#xd7 #x87 #x00) #(#xd7 #x87 #x5f) #(#xd7 #x87 #x87) #(#xd7 #x87 #xaf) #(#xd7 #x87 #xd7) #(#xd7 #x87 #xff)
#(#xd7 #xaf #x00) #(#xd7 #xaf #x5f) #(#xd7 #xaf #x87) #(#xd7 #xaf #xaf) #(#xd7 #xaf #xd7) #(#xd7 #xaf #xff)
#(#xd7 #xd7 #x00) #(#xd7 #xd7 #x5f) #(#xd7 #xd7 #x87) #(#xd7 #xd7 #xaf) #(#xd7 #xd7 #xd7) #(#xd7 #xd7 #xff)
#(#xd7 #xff #x00) #(#xd7 #xff #x5f) #(#xd7 #xff #x87) #(#xd7 #xff #xaf) #(#xd7 #xff #xd7) #(#xd7 #xff #xff)

#(#xff #x00 #x00) #(#xff #x00 #x5f) #(#xff #x00 #x87) #(#xff #x00 #xaf) #(#xff #x00 #xd7) #(#xff #x00 #xff)
#(#xff #x5f #x00) #(#xff #x5f #x5f) #(#xff #x5f #x87) #(#xff #x5f #xaf) #(#xff #x5f #xd7) #(#xff #x5f #xff)
#(#xff #x87 #x00) #(#xff #x87 #x5f) #(#xff #x87 #x87) #(#xff #x87 #xaf) #(#xff #x87 #xd7) #(#xff #x87 #xff)
#(#xff #xaf #x00) #(#xff #xaf #x5f) #(#xff #xaf #x87) #(#xff #xaf #xaf) #(#xff #xaf #xd7) #(#xff #xaf #xff)
#(#xff #xd7 #x00) #(#xff #xd7 #x5f) #(#xff #xd7 #x87) #(#xff #xd7 #xaf) #(#xff #xd7 #xd7) #(#xff #xd7 #xff)
#(#xff #xff #x00) #(#xff #xff #x5f) #(#xff #xff #x87) #(#xff #xff #xaf) #(#xff #xff #xd7) #(#xff #xff #xff)
; 24 greyscale e8-ff
#(#x08 #x08 #x08) #(#x12 #x12 #x12) #(#x1c #x1c #x1c) #(#x26 #x26 #x26) #(#x30 #x30 #x30) #(#x3a #x3a #x3a)
#(#x44 #x44 #x44) #(#x4e #x4e #x4e) #(#x58 #x58 #x58) #(#x62 #x62 #x62) #(#x6c #x6c #x6c) #(#x76 #x76 #x76)
#(#x80 #x80 #x80) #(#x8a #x8a #x8a) #(#x94 #x94 #x94) #(#x9e #x9e #x9e) #(#xa8 #xa8 #xa8) #(#xb2 #xb2 #xb2)
#(#xbc #xbc #xbc) #(#xc6 #xc6 #xc6) #(#xd0 #xd0 #xd0) #(#xda #xda #xda) #(#xe4 #xe4 #xe4) #(#xee #xee #xee)) i)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ICO_files
;;
;; Read an ICO graphic image into an object
;;  (IcoRead filename) => Icon graphic image object
;;
;; Write an ICO graphic object
;;  (IcoWrite ICO filename)
;;
;; Set an ICO object's palette color's RGB value
;;   (IcoPaletteRGBSet ICO iconIndex paletteIndex rgb)
;;
;; Set an ICO object's pixel
;;   IcoIndexedPixelSet ICO iconIndex y x index)
;;
(define ICODEBUG displayl)
(define (DB . r) (ICODEBUG "\r\n")
                 (apply ICODEBUG r))
(define (DBI . r) (apply ICODEBUG r))


; Split a number into a list of numbers at bit intervals
(define (byteSplit8 b)
 (list
   (if (= 0 (logand b #b10000000)) 0 1)
   (if (= 0 (logand b #b01000000)) 0 1)
   (if (= 0 (logand b #b00100000)) 0 1)
   (if (= 0 (logand b #b00010000)) 0 1)
   (if (= 0 (logand b #b00001000)) 0 1)
   (if (= 0 (logand b #b00000100)) 0 1)
   (if (= 0 (logand b #b00000010)) 0 1)
   (if (= 0 (logand b #b00000001)) 0 1)))
(define (byteSplit2 b)
 (list (/ b 16) (logand b #b1111)))
(define (byteSplit1 b) (list b))
 

; Create a function which accepts numbers, shifts by bitcount
; and accumulates, adds the number and writes to fp each
; byte assembled.
(define (packAndSend bitCount fp)
 (define count 0)
 (define byte 0)
 (lambda (n)
   (set! byte (+ (<< byte bitCount) n))
   (set! count (+ count bitCount))
   (if (= 8 count) (begin (sendByte byte fp)
                          (set! count 0)
                          (set! byte 0)))))


; Read (6 bytes) and return an ICO descriptor  #('ICO reserved type iconCount #entries(...) #infos(...))
(define (HeaderRead fs)
  (define reserved (recvWord fs))
  (define type (recvWord fs))
  (define iconCount (recvWord fs))
  (define entries (make-vector iconCount) #f)
  (define infos (make-vector iconCount) #f)
  (vector 'ICO reserved type iconCount entries infos))
; Validate anh ICO descriptor
(define (HeaderValidate desc)
  (or (vector? desc) (error "Header not a vector"))
  (or (= 6 (vector-length desc)) (error "Header not correct size"))
  (or (eq? 'ICO (vector-ref desc 0)) (error "Header incorrectly tagged" desc)))
; Set an icon's entry or info descriptor
(define (HeaderEntrySet desc i entry) (HeaderValidate desc) (vector-set! (vector-ref desc 4) i entry))
(define (HeaderInfoSet desc i entry) (HeaderValidate desc) (vector-set! (vector-ref desc 5) i entry))
; Get get an ICO element
(define (HeaderReserved desc)  (HeaderValidate desc) (vector-ref desc 1))
(define (HeaderType desc)      (HeaderValidate desc) (vector-ref desc 2))
(define (HeaderIconCount desc) (HeaderValidate desc) (vector-ref desc 3))
(define (HeaderEntryRef desc i) (HeaderValidate desc) (vector-vector-ref desc 4 i))
(define (HeaderInfoRef desc i) (HeaderValidate desc) (vector-vector-ref desc 5 i))


; Read and return an ICO's entry descriptor  16 bytes
(define (EntryReadAll ico fs)
 (loop (HeaderIconCount ico) (lambda (i)
  (define width     (recvByte fs))
  (define height    (recvByte fs))
  (define colorCount(recvByte fs))
  (define reserved  (recvByte fs))
  (define planes    (recvWord fs))
  (define bits      (recvWord fs))
  (define size      (recvLong fs))
  (define offset    (recvLong fs))
  (define entry (vector 'ENTRY width height colorCount reserved planes bits size offset))
  (HeaderEntrySet ico i entry))))
; Valide a header descriptor
(define (EntryValidate desc)
  (or (vector? desc) (error "Entry not a vector"))
  (or (= 9 (vector-length desc)) (error "Entry not correct size"))
  (or (eq? 'ENTRY (vector-ref desc 0)) (error "Entry incorrectly tagged")))
; Get the header's entries
(define (EntryWidth desc)      (EntryValidate desc) (vector-ref desc 1))
(define (EntryHeight desc)     (EntryValidate desc) (vector-ref desc 2))
(define (EntryColorCount desc) (EntryValidate desc) (vector-ref desc 3))
(define (EntryReserved desc)   (EntryValidate desc) (vector-ref desc 4))
(define (EntryPlanes desc)     (EntryValidate desc) (vector-ref desc 5))
(define (EntryBits  desc)      (EntryValidate desc) (vector-ref desc 6))
(define (EntrySize desc)       (EntryValidate desc) (vector-ref desc 7))
(define (EntryOffset desc)     (EntryValidate desc) (vector-ref desc 8))


; Read and return an ICO's info descriptor  40 bytes
; Also calls the color palette reader       4 * ColorCount bytes
(define (InfoRead ico index fs)
  (define size           (recvLong fs))
  (define width          (recvLong fs))
  (define height         (recvLong fs))
  (define planes         (recvWord fs))
  (define bitCount       (recvWord fs))
  (define compression    (recvLong fs))
  (define imageSize      (recvLong fs))
  (define xPixelsMeter   (recvLong fs))
  (define yPixelsMeter   (recvLong fs))
  (define colorsUsed     (recvLong fs))
  (define colorsImportant(recvLong fs))
  (define palette #f)
  (define xorBitmap #f)
  (define andBitmap #f)
  (define info (vector 'INFO size width height planes bitCount compression imageSize xPixelsMeter
                                yPixelsMeter colorsUsed colorsImportant palette xorBitmap andBitmap))
  (HeaderInfoSet ico index info))
; Validate info descriptor
(define (InfoValidate desc)
  (or (vector? desc) (error "Info not a vector"))
  (or (= 15 (vector-length desc)) (error "Info not correct size"))
  (or (eq? 'INFO (vector-ref desc 0)) (error "Info incorrectly tagged")))
; Set info descriptor values
(define (InfoPaletteSet desc p)  (InfoValidate desc) (if (vector-ref desc 12) (error "Palette alrady set")) (vector-set! desc 12 p))
(define (InfoXorBitmapSet desc p)(InfoValidate desc) (if (vector-ref desc 13) (error "Xor bitmap alrady set")) (vector-set! desc 13 p))
(define (InfoAndBitmapSet desc p)(InfoValidate desc) (if (vector-ref desc 14) (error "And bitmap alrady set")) (vector-set! desc 14 p))
; Get the header's entries
(define (InfoSize         desc) (InfoValidate desc) (vector-ref desc 1))
(define (InfoWidth        desc) (InfoValidate desc) (vector-ref desc 2))
(define (InfoHeight       desc) (InfoValidate desc) (vector-ref desc 3))
(define (InfoPlanes       desc) (InfoValidate desc) (vector-ref desc 4))
(define (InfoBitCount     desc) (InfoValidate desc) (vector-ref desc 5))
(define (InfoCompression  desc) (InfoValidate desc) (vector-ref desc 6))
(define (InfoImageSize    desc) (InfoValidate desc) (vector-ref desc 7))
(define (InfoXPixelsMeter desc) (InfoValidate desc) (vector-ref desc 8))
(define (InfoYPixelsMeter desc) (InfoValidate desc) (vector-ref desc 9))
(define (InfoColorsUsed   desc) (InfoValidate desc) (vector-ref desc 10))
(define (InfoColorsImportant desc) (InfoValidate desc) (vector-ref desc 11))
(define (InfoPalette      desc) (InfoValidate desc) (vector-ref desc 12))
(define (InfoXorBitmap    desc) (InfoValidate desc) (vector-ref desc 13))
(define (InfoAndBitmap    desc) (InfoValidate desc) (vector-ref desc 14))


; Read an ICO color palette which is a sequence of 4 bytes rgba.
; If bitcount > 8 the palette will be set to #().
(define (PaletteRead ico index fs)
  (define entryColorCount (EntryColorCount (HeaderEntryRef ico index)))
  (define info (HeaderInfoRef ico index))
  (define colorCount (if (< 8 (InfoBitCount info)) 0 (if (= 0 entryColorCount) 256 entryColorCount)))
  (define palette (make-vector colorCount)) ; colorCount could be 0 for non-palletized bitmaps
  (loop colorCount (lambda (c)
    (define r (recvByte fs))
    (define g (recvByte fs))
    (define b (recvByte fs))
    (define a (recvByte fs))
    (vector-set! palette c (vector r g b a))))
  (InfoPaletteSet info palette))


; Read XOR bitmaps
(define (XorBitmapRead ico index fp)
  (define info (HeaderInfoRef ico index)) ; Consider info descriptor
  (define width (InfoWidth info))
  (define height (/ (InfoHeight info) 2)) ; The height includes the AND bit map as well so divide it out
  (define bitCount (InfoBitCount info))
  (define bitMapSize (- (EntrySize (HeaderEntryRef ico index))
                        (InfoSize info)
                        (* 4 (vector-length (InfoPalette info)))))

  (DB "XOR and AND bitmap byte size:" bitMapSize NEWLINE)

  (if (< 8 bitCount)
    (begin ; Non-palletized bitmaps not supported.  Just read bytes into a list.
      (DB "skipping " bitCount "bit bitmaps for now" NEWLINE)
      (InfoXorBitmapSet info (let ~ ((i bitMapSize))
                                     (if (= i 0) () (cons (recvByte fp) (~ (- i 1)))))))
    ; Ranges and sizes need to be considered based on the pixel bitSize
    ; and 32bit padding.  A 2d pixel vector is sized to include the padded
    ; unused pixels.  As bytes are read fromthe file, the resulting bits that
    ; make up a pixel are stored as a number in the 2d vector.
    (letrec ((bitWidth (* width bitCount))
             (bitWidthPadded  (+ bitWidth (modulo bitWidth 32)))
             (pixelWidthPadded (/ bitWidthPadded bitCount))
             (byteWidthPadded (/ bitWidthPadded 8))
             (pixelPerByte (/ 8 bitCount))
             (bitmap (make-vector-vector height pixelWidthPadded #f))
             (byteSplitterFunc (if (= 1 bitCount) byteSplit8
                               (if (= 4 bitCount) byteSplit2
                               (if (= 8 bitCount) byteSplit1 (error "(XorBitmapRead) Unsupported bit count"))))))
      ; Set the vector repeatedly, bottom up, with a list of pixels generated one byte at a time
      (loop2 1 (+ 1 height)
             0 byteWidthPadded
             (lambda (y x)
               (vector-set-list! (vector-ref bitmap (- height y))
                                 (* x pixelPerByte)
                                 (byteSplitterFunc (recvByte fp)))))
      (InfoXorBitmapSet info bitmap))))


; Read AND bitmap.  Skipped if the icon doesn't have one (greater than 8 bit)
(define (AndBitmapRead ico index fp)
  (define info (HeaderInfoRef ico index)) ; Consider info descriptor
  (define width (InfoWidth info))
  (define height (/ (InfoHeight info) 2)) ; The height includes the XOR bit map as well so divide it out
  (define bitCount (InfoBitCount info))
  (define bitMapSize (- (EntrySize (HeaderEntryRef ico index))
                        (InfoSize info)
                        (* 4 (vector-length (InfoPalette info)))))
  (if (<= bitCount 8)
    ; AND bitmap
    (letrec ((bitWidthPadded  (+ width (modulo width 32)))
             (byteWidthPadded (/ bitWidthPadded 8))
             (bitmap (make-vector-vector height bitWidthPadded #f)))
      ; Set the vector repeatedly, bottom up, with a list of pixels generated one byte at a time
      (loop2 1 (+ 1 height)
             0 byteWidthPadded
             (lambda (y x)
                (vector-set-list! (vector-ref bitmap (- height y))
                                  (* x 8)
                                  (byteSplit8 (recvByte fp)))))
      (InfoAndBitmapSet info bitmap))))



(define (IcoRead filename)
  ; Open file port object
  (define fp (open-file filename))

  ; Read header
  (define ICO (HeaderRead fp))
  (define IconCount (HeaderIconCount ICO)) ; Consider icon count

  ; Dump header info
  (DB filename "  Reserved:" (HeaderReserved ICO) "  Type:" (HeaderType ICO) "  Count:" IconCount NEWLINE)

  ; Read all entries
  (EntryReadAll ICO fp)

  ; Dump all entries info
  (loop IconCount (lambda (i)
    (define ed (HeaderEntryRef ICO i))
    (DB i" Width:"      (EntryWidth ed) "  Height:" (EntryHeight ed) "  ColorCount:" (EntryColorCount ed)
              "  Reserved:"   (EntryReserved ed) "  Planes:" (EntryPlanes ed) "  BitCount:" (EntryBits ed)
              "  Size/Bytes:" (EntrySize ed) "  FileOffset:" (EntryOffset ed) NEWLINE)))

  ; Read and dump all info blocks (info, palette, XOR bitmap, AND bitmap)
  (loop IconCount (lambda (i)
    ; Read info header
    (InfoRead ICO i fp)
    ; Dump info  header
    (let ((info (HeaderInfoRef ICO i)))
      (DBI i" Size:" (InfoSize info))
      (DBI "  Width:" (InfoWidth info))
      (DBI "  Height:" (InfoHeight info))
      (DBI "  Planes:"  (InfoPlanes info))
      (DBI "  BitCount:" (InfoBitCount info))
      (DBI "  Compression:" (InfoCompression info))
      (DBI "  ImageSize:" (InfoImageSize info))
      (DBI "  XPixels/M:" (InfoXPixelsMeter info))
      (DBI "  YPixels/M:" (InfoYPixelsMeter info))
      (DBI "  ColorsUsed:" (InfoColorsUsed info))
      (DBI "  ColorsImpor:" (InfoColorsImportant info))
      (DB newline))
    ; Read palette
    (PaletteRead ICO i fp)
    ; Dump palette
    (let ((info (HeaderInfoRef ICO i)))
       (DB "RGBR " (InfoPalette info))
       (DB newline))
    ; Read XOR bitmap
    (XorBitmapRead ICO i fp)
    ; Dump XOR bitmap or raw bytes
    (if (<= (InfoBitCount (HeaderInfoRef ICO i)) 8)
      ;(vector-for-each (lambda (v) (DB v NEWLINE)) (InfoXorBitmap (HeaderInfoRef ICO i)))
      (vector-for-each (lambda (row) (vector-for-each (lambda (b) (DBI #\ )
                                                                  (if (< b 10) (DBI #\ ))
                                                                  (DBI b))
                                                      row)
                                     (DB))
                       (InfoXorBitmap (HeaderInfoRef ICO i)))
      (DB (InfoXorBitmap (HeaderInfoRef ICO i))))
    (DB newline)
    ; Read AND bitmap
    (AndBitmapRead ICO i fp)
     ; Dump AND bitmap if one exists
    (if (InfoAndBitmap (HeaderInfoRef ICO i))
      (vector-for-each (lambda (v) (vector-for-each (lambda (b) (DBI (if (= b 0) #\. #\*))) v)
                                   (DB))
                       (InfoAndBitmap (HeaderInfoRef ICO i))))
    (DB newline)))
 
  ; Make sure no extra info before quiting
  (or (eof-object? (recv 1 #f fp)) (eror "unexpected extra byes"))
  ICO)


           

(define (IcoWriteInternal ICO filename)
  (define fp (open-new-file filename))
  ; Header
  (sendWord (HeaderReserved  ICO) fp) 
  (sendWord (HeaderType      ICO) fp) 
  (sendWord (HeaderIconCount ICO) fp)
  ; Entries
  (loop (HeaderIconCount ICO) (lambda (i)
    (define ENTRY (HeaderEntryRef ICO i))
    (sendByte (EntryWidth      ENTRY) fp) 
    (sendByte (EntryHeight     ENTRY) fp) 
    (sendByte (EntryColorCount ENTRY) fp) 
    (sendByte (EntryReserved   ENTRY) fp) 
    (sendWord (EntryPlanes     ENTRY) fp) 
    (sendWord (EntryBits       ENTRY) fp) 
    (sendLong (EntrySize       ENTRY) fp) 
    (sendLong (EntryOffset     ENTRY) fp)))
  (loop (HeaderIconCount ICO) (lambda (i)
    ; Info header
    (define ENTRYCOLORCOUNT (EntryColorCount (HeaderEntryRef ICO i)))
    (define INFO (HeaderInfoRef ICO i))
    (define COLORCOUNT (if (< 8 (InfoBitCount INFO)) 0 (if (= 0 ENTRYCOLORCOUNT) 256 ENTRYCOLORCOUNT)))
    (define PALETTE (InfoPalette INFO))
    (define BITS (InfoBitCount INFO))
    (define XORBITMAP (InfoXorBitmap INFO))
    (define ANDBITMAP (InfoAndBitmap INFO))
    (sendLong (InfoSize            INFO) fp)
    (sendLong (InfoWidth           INFO) fp)
    (sendLong (InfoHeight          INFO) fp)
    (sendWord (InfoPlanes          INFO) fp)
    (sendWord (InfoBitCount        INFO) fp)
    (sendLong (InfoCompression     INFO) fp)
    (sendLong (InfoImageSize       INFO) fp)
    (sendLong (InfoXPixelsMeter    INFO) fp)
    (sendLong (InfoYPixelsMeter    INFO) fp)
    (sendLong (InfoColorsUsed      INFO) fp)
    (sendLong (InfoColorsImportant INFO) fp)
    ; Info Palette
    (loop COLORCOUNT (lambda (i)
      (vector-for-each (lambda (b)  (sendByte b fp))
                       (vector-ref PALETTE i))))
    ; Info XOR Bitmap
    (let ((packer (packAndSend BITS fp))
          (HEIGHT (/ (InfoHeight INFO) 2)))
      (loop1 1 (+ HEIGHT 1) (lambda (y)
        (set! y (- HEIGHT y))
        (vector-for-each packer (vector-ref XORBITMAP y)))))
    ; Info AND Bitmap
    (let ((packer (packAndSend 1 fp))
          (HEIGHT (/ (InfoHeight INFO) 2)))
      (loop1 1 (+ HEIGHT 1) (lambda (y)
        (set! y (- HEIGHT y))
        (vector-for-each packer (vector-ref ANDBITMAP y)))))
  ))
  ; Done
  (close fp))

(define (IcoWrite ICO filename)
  ; Verify all icons are 8 bit or less.  Non palletized icons are not supported. 
  (if (let ~ ((i 0))
        (if (= i (HeaderIconCount ICO)) #t
        (if (< 8 (EntryBits (HeaderEntryRef ICO i))) (begin (DB "Icon " i " is > 8 bit which are not supported for writing.") #f)
        (~  (+ i 1)))))
      (IcoWriteInternal ICO filename)))

(define (IcoPaletteRGBSet ICO iconIndex paletteIndex rgb)
 (define INFO (HeaderInfoRef ICO iconIndex))
 (define PALETTE (InfoPalette INFO))
 (if (and (<= 0 paletteIndex)
          (< paletteIndex (vector-length PALETTE)))
   (begin (vector-set! (vector-ref PALETTE paletteIndex) 0 (vector-ref rgb 2))
          (vector-set! (vector-ref PALETTE paletteIndex) 1 (vector-ref rgb 1))
          (vector-set! (vector-ref PALETTE paletteIndex) 2 (vector-ref rgb 0)))
   (DB "ERROR: (IcoPaletteRGBSet paletteIndex " paletteIndex " out of range " (vector-length PALETTE))))

(define (IcoIndexedPixelSet ICO iconIndex y x index)
 (define INFO (HeaderInfoRef ICO iconIndex))
 (define XOR (InfoXorBitmap INFO))
   (vector-vector-set! XOR y x index))


; Set the DEBUG message function.  Set to (lambda x) to disable debugging
(define (IcoInitialize db)
 (set! ICODEBUG db))
