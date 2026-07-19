#lang racket/base

;;; visualize.rkt -- renders a benchmark JSON run (as produced by report.rkt's
;;; `save-run!` / main.rkt's `--save`) as a single SVG chart.
;;;
;;;   racket benchmark/visualize.rkt --in results.json --out results.svg
;;;
;;; Design: one horizontal row per (group, name) operation, one dot per input
;;; size it was benchmarked at, every dot placed on one shared log-scale time
;;; axis, rows sorted fastest-to-slowest by geometric mean. The single shared
;;; scale makes both absolute operation speed (dot's absolute x position,
;;; readable off the axis) and relative speed (row's position relative to
;;; every other row) directly legible in the same image. A thin whisker per
;;; dot spans the operation's observed [min, max] sample range, and the
;;; fastest/slowest dot in each row is labeled with its exact duration in an
;;; adaptive unit (ns/us/ms/s) at 3 significant figures.

(require json
         racket/cmdline
         racket/date
         racket/format
         racket/list
         racket/math
         racket/string)

(define (clamp lo v hi) (max lo (min v hi)))
(define (log10 x) (/ (log x) (log 10)))

;; ---------------------------------------------------------------------------
;; Data model
;; ---------------------------------------------------------------------------

(struct point (size mean min max) #:transparent)
(struct series (group name points) #:transparent) ; points sorted by size

;; Groups the flat list of per-(group,name,size) JSON records into one
;; `series` per (group, name), each holding one `point` per size tested.
(define (records->series records)
  (define grouped
    (for/fold ([acc (hash)]) ([r (in-list records)])
      (define key (cons (hash-ref r 'group) (hash-ref r 'name)))
      (define pt (point (hash-ref r 'size) (hash-ref r 'mean) (hash-ref r 'min) (hash-ref r 'max)))
      (hash-update acc key (λ (pts) (cons pt pts)) '())))
  (for/list ([key (in-hash-keys grouped)])
    (series (car key) (cdr key) (sort (hash-ref grouped key) < #:key point-size))))

;; Geometric mean of a series' per-size means -- appropriate for ranking
;; log-scale data by overall speed, since it weighs each order of magnitude
;; equally rather than letting the largest size dominate the way an arithmetic
;; mean over highly skewed timings would.
(define (series-geomean s)
  (define floored (map (λ (p) (max (point-mean p) 1e-9)) (series-points s)))
  (exp (/ (apply + (map log floored)) (length floored))))

;; ---------------------------------------------------------------------------
;; Formatting
;; ---------------------------------------------------------------------------

;; Renders a positive real with exactly 3 significant figures.
(define (sig3 v)
  (if (<= v 0)
      "0"
      (let* ([expo (inexact->exact (floor (log10 v)))]
             [decimals (max 0 (- 2 expo))])
        (~r v #:precision (list '= decimals)))))

;; Adaptive-unit rendering of a millisecond duration: 1.07e-5 -> "10.7ns",
;; 0.067 -> "67.0us", 506.3 -> "506ms", 2531.0 -> "2.53s".
(define (format-ms t)
  (define t* (max t 0.0))
  (cond
    [(< t* 1e-3)   (string-append (sig3 (* t* 1e6)) "ns")]
    [(< t* 1.0)    (string-append (sig3 (* t* 1e3)) "us")]
    [(< t* 1000.0) (string-append (sig3 t*) "ms")]
    [else          (string-append (sig3 (/ t* 1000.0)) "s")]))

;; ---------------------------------------------------------------------------
;; Color
;; ---------------------------------------------------------------------------

(define (byte->hex n) (~r n #:base 16 #:min-width 2 #:pad-string "0"))

;; h in [0,360), s and l in [0,1]. Standard HSL -> RGB conversion, done here
;; (rather than emitting `hsl(...)` directly) so the output is plain #rrggbb
;; and renders identically in every SVG consumer, not just browsers.
(define (hsl->hex h s l)
  (define c (* (- 1 (abs (- (* 2 l) 1))) s))
  (define hp (/ h 60.0))
  (define hp-mod2 (- hp (* 2 (floor (/ hp 2)))))
  (define x (* c (- 1 (abs (- hp-mod2 1)))))
  (define-values (r1 g1 b1)
    (cond [(< hp 1) (values c x 0)]
          [(< hp 2) (values x c 0)]
          [(< hp 3) (values 0 c x)]
          [(< hp 4) (values 0 x c)]
          [(< hp 5) (values x 0 c)]
          [else     (values c 0 x)]))
  (define m (- l (/ c 2)))
  (define (chan v) (byte->hex (inexact->exact (round (* 255 (+ v m))))))
  (string-append "#" (chan r1) (chan g1) (chan b1)))

(define (palette n)
  (for/list ([i (in-range n)])
    (hsl->hex (* i (/ 360.0 (max n 1))) 0.62 0.46)))

;; ---------------------------------------------------------------------------
;; SVG primitives
;; ---------------------------------------------------------------------------

(define (n2 v) (~r v #:precision 2))
(define (n0 v) (~r v #:precision 0))

(define (xml-escape s)
  (define s1 (string-replace s "&" "&amp;"))
  (define s2 (string-replace s1 "<" "&lt;"))
  (string-replace s2 ">" "&gt;"))

(define (svg-line x1 y1 x2 y2 #:stroke stroke #:width [width 1] #:opacity [opacity 1.0])
  (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" stroke=\"~a\" stroke-width=\"~a\" stroke-opacity=\"~a\" />\n"
          (n2 x1) (n2 y1) (n2 x2) (n2 y2) stroke (n2 width) (n2 opacity)))

(define (svg-circle cx cy r #:fill fill #:opacity [opacity 1.0])
  (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" fill=\"~a\" fill-opacity=\"~a\" />\n"
          (n2 cx) (n2 cy) (n2 r) fill (n2 opacity)))

(define (svg-rect x y w h #:fill fill #:opacity [opacity 1.0])
  (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" fill=\"~a\" fill-opacity=\"~a\" />\n"
          (n2 x) (n2 y) (n2 w) (n2 h) fill (n2 opacity)))

(define (svg-frame x y w h #:stroke stroke #:width [width 1])
  (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" fill=\"none\" stroke=\"~a\" stroke-width=\"~a\" />\n"
          (n2 x) (n2 y) (n2 w) (n2 h) stroke (n2 width)))

(define (svg-text x y str #:size [size 12] #:anchor [anchor "start"]
                  #:fill [fill "#1a1a1a"] #:weight [weight "normal"])
  (format "<text x=\"~a\" y=\"~a\" font-size=\"~a\" text-anchor=\"~a\" fill=\"~a\" font-weight=\"~a\" font-family=\"Helvetica, Arial, sans-serif\">~a</text>\n"
          (n2 x) (n2 y) size anchor fill weight (xml-escape str)))

;; ---------------------------------------------------------------------------
;; Layout & rendering
;; ---------------------------------------------------------------------------

(define (render-svg run-jsexpr #:canvas-width [canvas-width 1400] #:row-height [row-height 20])
  (define records (hash-ref run-jsexpr 'records))
  (when (null? records) (error 'visualize "no records in this run"))

  (define all-series (sort (records->series records) < #:key series-geomean))
  (define n-rows (length all-series))
  (define groups (remove-duplicates (map series-group all-series)))
  (define colors
    (for/hash ([g (in-list groups)] [c (in-list (palette (length groups)))]) (values g c)))

  ;; global log-scale time domain, padded a third of a decade on each side so
  ;; the extreme dots and their labels don't clip the plot edge
  (define all-times
    (apply append
           (for/list ([s (in-list all-series)])
             (apply append
                    (for/list ([p (in-list (series-points s))])
                      (list (max (point-mean p) 1e-9)
                            (max (point-min p) 1e-9)
                            (max (point-max p) 1e-9)))))))
  (define dom-lo (inexact->exact (floor (log10 (apply min all-times)))))
  (define dom-hi (inexact->exact (ceiling (log10 (apply max all-times)))))
  (define lo (- dom-lo 0.3))
  (define hi (+ dom-hi 0.3))

  ;; layout constants
  (define label-w
    (clamp 220 (+ 40 (* 6.4 (apply max (map (λ (s) (string-length (series-name s))) all-series)))) 560))
  (define label-x0 16)
  (define plot-x0 (+ label-x0 14 label-w))
  (define right-margin 90)
  (define plot-x1 (- canvas-width right-margin))
  (define plot-w (- plot-x1 plot-x0))
  (define items-per-legend-row (max 1 (quotient (exact-round plot-w) 160)))
  (define legend-rows (ceiling (/ (length groups) items-per-legend-row)))
  (define title-h 40)
  (define legend-h (+ (* legend-rows 18) 8))
  (define top-margin (+ title-h legend-h 14))
  (define bottom-margin 46)
  (define plot-h (* n-rows row-height))
  (define canvas-height (+ top-margin plot-h bottom-margin))

  (define (xpos t) (+ plot-x0 (* (/ (- (log10 (max t 1e-9)) lo) (- hi lo)) plot-w)))

  ;; -- rows: one per operation, one dot per size, whisker = observed [min,max]
  (define row-elems
    (for/list ([s (in-list all-series)] [i (in-range n-rows)])
      (define row-y (+ top-margin (* i row-height)))
      (define cy (+ row-y (/ row-height 2)))
      (define color (hash-ref colors (series-group s)))
      (define pts (series-points s))
      (define zebra
        (if (even? i) (svg-rect plot-x0 row-y plot-w row-height #:fill "#f4f5f7") ""))
      (define whiskers
        (apply string-append
               (for/list ([p (in-list pts)])
                 (svg-line (xpos (point-min p)) cy (xpos (point-max p)) cy
                           #:stroke color #:width 1 #:opacity 0.30))))
      (define connector
        (if (> (length pts) 1)
            (apply string-append
                   (for/list ([a (in-list pts)] [b (in-list (cdr pts))])
                     (svg-line (xpos (point-mean a)) cy (xpos (point-mean b)) cy
                               #:stroke color #:width 1.2 #:opacity 0.55)))
            ""))
      (define dots
        (apply string-append
               (for/list ([p (in-list pts)])
                 (svg-circle (xpos (point-mean p)) cy
                             (clamp 2.5 (+ 2.5 (* 1.3 (log10 (add1 (point-size p))))) 8)
                             #:fill color #:opacity 0.92))))
      (define swatch (svg-rect label-x0 (- cy 5) 10 10 #:fill color))
      (define label (svg-text (- plot-x0 12) (+ cy 4) (series-name s) #:size 11 #:anchor "end"))
      (define fastest (argmin point-mean pts))
      (define slowest (argmax point-mean pts))
      (define fastest-label
        (svg-text (- (xpos (point-mean fastest)) 6) (+ cy -6) (format-ms (point-mean fastest))
                  #:size 9 #:anchor "end" #:fill "#555555"))
      (define slowest-label
        (if (eq? fastest slowest)
            ""
            (svg-text (+ (xpos (point-mean slowest)) 6) (+ cy -6) (format-ms (point-mean slowest))
                      #:size 9 #:anchor "start" #:fill "#555555")))
      (apply string-append (list zebra whiskers connector dots swatch label fastest-label slowest-label))))

  ;; -- x-axis: one gridline + adaptive-unit label per decade
  (define axis-elems
    (append
     (for/list ([e (in-range dom-lo (add1 dom-hi))])
       (define t (expt 10.0 e))
       (define x (xpos t))
       (string-append
        (svg-line x top-margin x (+ top-margin plot-h) #:stroke "#d8dadd" #:width 1)
        (svg-text x (+ top-margin plot-h 18) (format-ms t) #:size 10 #:anchor "middle" #:fill "#555555")))
     (list (svg-line plot-x0 (+ top-margin plot-h) plot-x1 (+ top-margin plot-h) #:stroke "#9aa0a6" #:width 1.2)
           (svg-text (/ (+ plot-x0 plot-x1) 2) (+ top-margin plot-h 34) "mean time per call (log scale)"
                     #:size 10 #:anchor "middle" #:fill "#555555"))))

  ;; -- legend: one swatch + label per group, wrapped to fit the plot width
  (define legend-elems
    (for/list ([g (in-list groups)] [i (in-naturals)])
      (define col (remainder i items-per-legend-row))
      (define row (quotient i items-per-legend-row))
      (define lx (+ plot-x0 (* col 160)))
      (define ly (+ title-h 6 (* row 18)))
      (string-append (svg-rect lx ly 11 11 #:fill (hash-ref colors g))
                     (svg-text (+ lx 16) (+ ly 10) g #:size 10 #:anchor "start" #:fill "#333333"))))

  ;; -- title block
  (define run-label (hash-ref run-jsexpr 'label "run"))
  (define run-ts (hash-ref run-jsexpr 'timestamp #f))
  (define run-rv (hash-ref run-jsexpr 'racket-version "?"))
  (define ts-str (if run-ts (date->string (seconds->date run-ts) #t) "unknown time"))
  (define title-elems
    (list
     (svg-text 16 24 (format "Rope Benchmark Results - ~a" run-label) #:size 17 #:anchor "start" #:weight "bold")
     (svg-text 16 40 (format "~a records, ~a operations - Racket ~a - ~a"
                             (length records) n-rows run-rv ts-str)
               #:size 11 #:anchor "start" #:fill "#555555")))

  (define fmt
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~a\" height=\"~a\" viewBox=\"0 0 ~a ~a\">\n")
  (apply string-append
         (list (format fmt (n0 canvas-width) (n0 canvas-height) (n0 canvas-width) (n0 canvas-height))
               (svg-rect 0 0 canvas-width canvas-height #:fill "#ffffff")
               (apply string-append title-elems)
               (apply string-append legend-elems)
               (apply string-append axis-elems)
               (apply string-append row-elems)
               (svg-frame plot-x0 top-margin plot-w plot-h #:stroke "#c7cacf")
               "</svg>\n")))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define (default-out-path in-path-str)
  (if (string-suffix? in-path-str ".json")
      (string-append (substring in-path-str 0 (- (string-length in-path-str) 5)) ".svg")
      (string-append in-path-str ".svg")))

(module+ main
  (define in-path #f)
  (define out-path #f)
  (define canvas-width 1400)
  (define row-height 20)

  (command-line
   #:program "rope-bench-visualize"
   #:once-each
   [("--in") path "Input benchmark JSON file (from main.rkt --save)"
    (set! in-path path)]
   [("--out") path "Output SVG path (default: <in>.svg)"
    (set! out-path path)]
   [("--width") w "Canvas width in px (default: 1400)"
    (set! canvas-width (string->number w))]
   [("--row-height") h "Pixels per operation row (default: 20)"
    (set! row-height (string->number h))])

  (unless in-path (error 'visualize "missing required --in PATH"))
  (define run-jsexpr (call-with-input-file in-path read-json))
  (define svg (render-svg run-jsexpr #:canvas-width canvas-width #:row-height row-height))
  (define final-out (or out-path (default-out-path in-path)))
  (call-with-output-file final-out #:exists 'replace (λ (out) (display svg out)))
  (printf "Wrote ~a\n" final-out))
