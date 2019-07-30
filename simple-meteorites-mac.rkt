#lang racket/gui

(require csv-reading)

;;; Filenames
(define map-filename "BigEarth.jpg")
(define meteorites-filename "meteorite-landings.csv")

;;; Minimum and maximum latitude and longitude values
(define-values (lat-min lat-max) (values -90.0 90.0))
(define-values (long-min long-max) (values -180.0 180.0))

;;; Some data counts
(define fell-n (make-parameter 0))
(define found-n (make-parameter 0))
(define other-n (make-parameter 0))
(define invalid-n (make-parameter 0))
(define nomatch-n (make-parameter 0))

;;; (lat-long->x-y bitmap lat long) -> (values real? real?)
;;;   bitmap : (is-a?/c bitmap%
;;;   lat : (real-in -90.0 90.0)
;;;   long : (real-in -180.0 1800.0)
;;; Returns the (x, y) coordinates corresponding to the given lat and long.
(define/contract (lat-long->x-y bitmap lat long)
  (-> (is-a?/c bitmap%) (real-in -90.0 90.0) real? ; (real-in -180.0 180.0)
      (values real? real?))
  (define width (send bitmap get-width))
  (define height (send bitmap get-height))
  (values (* width (/ (- long long-min) (- long-max long-min)))
          (- height (* height (/ (- lat lat-min) (- lat-max lat-min))) 1)))

;;; (fall->color fall) -> string?
;;;   fall : string?
;;; Returns the color used to render a specified fall value. Also increments
;;; the data count dynamic variables.
(define/contract (fall->color fall)
  (-> string? string?)
  (case fall
    (("Fell")
     (fell-n (+ (fell-n) 1)) ; Increment fell count
     "red")
    (("Found")
     (found-n (+ (found-n) 1)) ; Increment found count
     "green")
    (else
     (other-n (+ (other-n) 1)) ; Increment other count
     "white")))

;;; (main) -> any
(define (main)  ;;; Initialize data count dynamic variables.
  (parameterize ((fell-n 0)
                 (found-n 0)
                 (other-n 0)
                 (invalid-n 0)
                 (nomatch-n 0))
    ;; Load the world map bitmap
    (define map (make-object bitmap% 1024 512))
    (send map load-file map-filename)
    ;; And draw it on the bitmap
    (send bitmap-dc draw-bitmap map 0 0)
    (send canvas refresh-now)
    ;; Draw lat/long grid on map
    (send bitmap-dc set-pen "gray" 1 'solid)
    (for ((lat (in-range lat-min lat-max 10.0)))
      (if (zero? lat) (send bitmap-dc set-alpha 0.75) (send bitmap-dc set-alpha 0.35))
      (define-values (x y) (lat-long->x-y bitmap lat 0))
      (send bitmap-dc draw-line 0 y 1024 y))
    (for ((long (in-range long-min long-max 10.0)))
      (if (zero? long) (send bitmap-dc set-alpha 0.75) (send bitmap-dc set-alpha 0.35))
      (define-values (x y) (lat-long->x-y bitmap 0 long))
      (send bitmap-dc draw-line x 0 x 512))
      (send canvas refresh-now)
    ;; Parse the meteorite landings file and skip the first row
    (define parsed-meteorite-landings
      (csv->list (file->string meteorites-filename)))
    (define meteroite-landings (cdr parsed-meteorite-landings))
    (printf "There are ~s meteorite landings in file ~s.~n"
            (length meteroite-landings) meteorites-filename)
    ;; Iterate over all the meteorite landings and put them on the map
    (for ((landing (in-list meteroite-landings)))
      (match landing
        ((list name id nametype recclass mass fall year reclat reclong GeoLocation)
         (define lat (string->number reclat))
         (define long (string->number reclong))
         ; Set pen width based on log of mass (min pen width = 3)
         (define bitmass (string->number mass))
         (define width 3)
         (if (and bitmass (> bitmass 3)) (set! width (log bitmass)) (set! width 3))
         (cond ((and (and lat long) (false? (zero? lat)) (false? (zero? long)))
                (send bitmap-dc set-pen (fall->color fall) width 'solid)
                (send bitmap-dc set-alpha 0.5)
                (define-values (x y) (lat-long->x-y bitmap lat long))
                (send bitmap-dc draw-point x y))
               (else
                (invalid-n (+ (invalid-n) 1)))))
        (_
         (nomatch-n (+ (nomatch-n) 1))
         (void))))
    (send canvas refresh-now)
    ;; Print the data counts.
    (printf "Fell    = ~a~n" (fell-n))
    (printf "Found   = ~a~n" (found-n))
    (printf "Other   = ~a~n" (other-n))
    (printf "Invalid = ~a~n" (invalid-n))
    (printf "Nomatch = ~a~n" (nomatch-n))))

;;; Graphical Elements

(define frame
  (instantiate frame%
    ("Meteorites")))

(define menu-bar
  (instantiate menu-bar%
    (frame)))

(define file-menu
  (instantiate menu%
    ("&File" menu-bar)))

(define exit-menu-item
  (instantiate menu-item%
    ("E&xit" file-menu)
    (callback
     (lambda (menu-item event)
       (send frame show #f)))))

(define bitmap (make-object bitmap% 1024 512))
(define bitmap-dc (make-object bitmap-dc% bitmap))

(define canvas
  (instantiate canvas%
    (frame)
    (paint-callback (lambda (c dc)
                      (send dc draw-bitmap bitmap 0 0)))
    (style '(border))
    (min-width 1024)
    (min-height 512)))

(send frame show #t)

(main)
