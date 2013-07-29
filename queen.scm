(define (shl bits)
  (let* ((len (vector-length bits))
         (res (make-vector len)))
    (define (copy i)
      (if (= i (- len 1))
        #t
        (begin
          (vector-set! res i
                       (vector-ref bits (+ i 1)))
          (copy (+ i 1)))))
    (copy 0)
    (vector-set! res (- len 1) #f)
    res))

(define (shr bits)
  (let* ((len (vector-length bits))
         (res (make-vector len)))
    (define (copy i)
      (if (= i (- len 1))
        #t
        (begin
          (vector-set! res (+ i 1)
                       (vector-ref bits i))
          (copy (+ i 1)))))
    (copy 0)
    (vector-set! res 0 #f)
    res))

(define (empty-bits len) (make-vector len #f))

(define (queen n)
  (let ((vs vector-set!)
        (vr vector-ref)
        (res 0))

    (define (search l m r step)
      (define (col-iter c)
        (if (= c n)
          #f
          (begin
            (if (and (eq? (vr l c) #f)
                     (eq? (vr r c) #f)
                     (eq? (vr m c) #f))
              (begin
                (vs l c #t)
                (vs m c #t)
                (vs r c #t)
                (search l m r (+ step 1))
                (vs l c #f)
                (vs m c #f)
                (vs r c #f)))
            (col-iter (+ c 1)))))
      (set! l (shl l))
      (set! r (shr r))
      (if (= step n)
        (set! res (+ res 1))
        (col-iter 0)))

    (search (empty-bits n)
            (empty-bits n)
            (empty-bits n)
            0)
    res))

