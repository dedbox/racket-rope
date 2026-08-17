#lang racket

;; =============================================================================
;; Mock Implementations for Benchmarking (Assuming String Chunks)
;; =============================================================================

(define CHUNK-SIZE (* 1024 1024))

(define chunk-a (make-string CHUNK-SIZE #\A))
(define chunk-b (make-string CHUNK-SIZE #\A)) ; Identical to force worst-case full traversal

(define (bulk-slice-equal? ra rb pa pb k)
  (equal? (substring ra pa (+ pa k))
          (substring rb pb (+ pb k))))

(define (element-loop-equal? ra rb pa pb k)
  (let loop ([i 0])
    (or (= i k)
        (and (char=? (string-ref ra (+ pa i))
                     (string-ref rb (+ pb i)))
             (loop (add1 i))))))

;; =============================================================================
;; Benchmarking Harness
;; =============================================================================
;; Using a large number of iterations minimizes the impact of JIT warmup and 
;; provides a stable average of GC behavior over time.

(define (run-benchmark k iterations)
  (printf "Benchmarking (chunk size ~a) with chunk overlap k = ~a (~a iterations)\n"
          CHUNK-SIZE k iterations)
  (printf "--------------------------------------------------------\n")
  
  (printf "Bulk Slice (equal?):\n")
  (time
   (for ([_ (in-range iterations)])
     (bulk-slice-equal? chunk-a chunk-b 0 0 k)))
  
  (printf "\nElement Loop (chunk-ref):\n")
  (time
   (for ([_ (in-range iterations)])
     (element-loop-equal? chunk-a chunk-b 0 0 k)))
  (printf "========================================================\n\n"))

;; (module+ main
;;   (define NUM-ITERATIONS 1000000)

;;   (let loop ([k 16] [n NUM-ITERATIONS])
;;     (run-benchmark (min k CHUNK-SIZE) n)
;;     (when (< k CHUNK-SIZE) (loop (* k 4) (/ n 2))))

;;   ;; ;; Test with small overlaps (allocation penalty should be visible)
;;   ;; (run-benchmark 16 1000000)

;;   ;; ;; Test with moderate overlaps
;;   ;; (run-benchmark 64 1000000)

;;   ;; ;; Test with large overlaps (native equal? speed should dominate)
;;   ;; (run-benchmark 512 1000000)

;;   )
