;
;          wavelets.scm -- Lance R. Williams, Sept. 7, 2005.
;
;            Copyright 2004 University of New Mexico.
;                      All rights reserved.
;
;     Permission to copy and modify this software and its documen-
;     tation only for internal use in your organization is hereby
;     granted, provided that this notice is retained thereon and
;     on all copies.  UNM makes no representations as to the sui-
;     tability and operability of this software for any purpose.
;     It is provided "as is" without express or implied warranty.
;
;     UNM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
;     INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FIT-
;     NESS.  IN NO EVENT SHALL UNM BE LIABLE FOR ANY SPECIAL,
;     INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY OTHER DAMAGES WHAT-
;     SOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
;     IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
;     ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PER-
;     FORMANCE OF THIS SOFTWARE.
;
;     No other rights, including, for example, the right to redis-
;     tribute this software and its documentation or the right to
;     prepare derivative works, are granted unless specifically
;     provided in a separate license agreement.
;
;     Copyright 2005, University of New Mexico. All rights
;     reserved.

(define-macro name-parts-of
  (lambda (ls names . body)
    `(apply (lambda ,names ,@body) ,ls)))

(define (reduce-rows x)
  (downsample-rows
   (convolve-rows x #(0.05 0.25 0.4 0.25 0.05))))

(define (reduce-cols x)
  (downsample-cols
   (convolve-cols x #(0.05 0.25 0.4 0.25 0.05))))

(define (reduce x)
  (reduce-rows (reduce-cols x)))

(define (right-split x n)
  (if (= n 0)
      x
      (let ((y (reduce (right-split x (- n 1)))))
	(left-to-right (reduce-rows x)
		       (top-to-bottom y y)))))

(define (bottom-split x n)
  (if (= n 0)
      x
      (let ((y (reduce (bottom-split x (- n 1)))))
	(top-to-bottom (reduce-cols x)
		       (left-to-right y y)))))

(define (corner-split x n)
  (if (= n 0)
      x
      (let ((right (right-split x (- n 1)))
	    (bottom (bottom-split x (- n 1))))
	(let ((bottom-left (left-to-right bottom bottom))
	      (upper-right (top-to-bottom right right))
	      (corner (corner-split x (- n 1))))
	  (left-to-right
	   (top-to-bottom (reduce x)
			  (reduce-rows (reduce bottom-left)))
	   (top-to-bottom (reduce-cols (reduce upper-right))
			  (reduce corner)))))))

(define (gaussian-convolve x)
  (convolve-rows
   (convolve-cols x #(0.05 0.25 0.4 0.25 0.05))
   #(0.05 0.25 0.4 0.25 0.05)))

(define (project-cols x)
  (convolve-cols
   (upsample-cols x)
   #(0.1 0.5 0.8 0.5 0.1)))

(define (project-rows x)
  (convolve-rows
   (upsample-rows x)
   #(0.1 0.5 0.8 0.5 0.1)))

(define (project x)
  (project-rows (project-cols x)))

(define (dog x)
  (- x (project (reduce x))))

(define (laplacian-pyramid x)
  (define (loop lowpass acc)
    (if (or (odd? (image-rows lowpass))
	    (odd? (image-cols lowpass)))
	(cons lowpass acc)
	(let ((y (reduce lowpass)))
	  (loop y (cons (- lowpass (project y)) acc)))))
  (loop x ()))

(define (inverse-laplacian-pyramid ls)
  (name-parts-of ls (lowpass bandpass . rest)
    (if (null? rest)
        (+ (project lowpass) bandpass)
        (inverse-laplacian-pyramid
	 (cons (+ (project lowpass) bandpass) rest)))))

(define (display-laplacian-pyramid ls)
  (define (normalize-around-zero im)
    (* (/ im (+ (image-max (magnitude im)) 0.00001)) 0.5))
  (let* ((ls (reverse ls))
	 (rows (image-rows (car ls))))
    (define (loop ls)
      (let* ((nim (normalize-around-zero (car ls)))
	     (cols (image-cols nim)))
	(if (null? (cdr ls))
	    (image-pad nim rows cols)
	    (left-to-right
	     (image-pad nim rows cols)
	     (loop (cdr ls))))))
    (loop ls)))

(define (color-left-to-right . args)
  (apply rgb->color-image (apply map left-to-right (map color-image->rgb args))))

(define (color-top-to-bottom . args)
  (apply rgb->color-image (apply map top-to-bottom (map color-image->rgb args))))

(define (display-color-laplacian-pyramid ls)
  (define (normalize-around-zero im)
    (* (/ im (+ (image-max (magnitude im)) 0.00001)) 0.5))
  (define (color-normalize-around-zero cim)
    (apply rgb->color-image (map normalize-around-zero (color-image->rgb cim))))
  (let* ((ls (reverse ls))
	 (rows (image-rows (car ls))))
    (define (loop ls)
      (let* ((nim (color-normalize-around-zero (car ls)))
	     (cols (image-cols nim)))
	(if (null? (cdr ls))
	    (image-pad nim rows cols)
	    (color-left-to-right
	     (image-pad nim rows cols)
	     (loop (cdr ls))))))
    (loop ls)))

(define (color-laplacian-pyramid cim)
  (apply map rgb->color-image (map laplacian-pyramid (color-image->rgb cim))))

(define (rows-down filter)
  (lambda (image)
    (downsample-rows
     (convolve-rows image filter))))

(define (cols-down filter)
  (lambda (image)
    (downsample-cols
     (convolve-cols image filter))))

(define (rows-up filter)
  (lambda (image)
    (convolve-rows
     (upsample-rows image) filter)))

(define (cols-up filter)
  (lambda (image)
    (convolve-cols
     (upsample-cols image) filter)))
  
(define (fast-wavelet-transform h0 h1)
  (let ((len (min (vector-length h0) (vector-length h1))))
    (letrec
      ((loop
	(lambda (image)
	  (let ((r (image-rows image))
		(c (image-cols image)))
	    (if (or (odd? r) (odd? c) (< r len) (< c len))
		image
		(let ((l ((rows-down h0) image))
		      (h ((rows-down h1) image)))
		  (list (loop ((cols-down h0) l))
			((cols-down h1) l)
			((cols-down h0) h)
			((cols-down h1) h))))))))
      loop)))

(define (inverse-fast-wavelet-transform g0 g1)
  (letrec
    ((loop
      (lambda (ls)
	(name-parts-of ls (ll hl lh hh)
	  (+ ((rows-up g1)
	      (+ ((cols-up g1) hh)
		 ((cols-up g0) lh)))
	   ((rows-up g0)
	    (+ ((cols-up g1) hl)
	       ((cols-up g0)
		(if (or (image? ll) (complex-image? ll))
		    ll
		    (loop ll))))))))))
    loop))

(define (display-wavelet-transform ls)
  (define (normalize-around-zero im)
    (* (/ im (image-max (magnitude im))) 0.5))
  (name-parts-of ls (ll lh hl hh)
    (left-to-right
     (top-to-bottom
      (if (pair? ll)
	  (display-wavelet-transform ll)
	  (normalize-around-zero ll))
      (normalize-around-zero lh))
     (top-to-bottom
      (normalize-around-zero hl)
      (normalize-around-zero hh)))))

(define haar
  (fast-wavelet-transform
   #(0  0.707107 0.707107)
   #(0 -0.707107 0.707107)))

(define inverse-haar
  (inverse-fast-wavelet-transform
   #(0.707107  0.707107 0)   
   #(0.707107 -0.707107 0)))

(define cvdw6
  (fast-wavelet-transform
   #(0.0
    -0.06629126073624+0.08558164961018i
     0.11048543456040+0.08558164961018i
     0.66291260736239-0.17116329922036i
     0.66291260736239-0.17116329922036i
     0.11048543456040+0.08558164961018i
    -0.06629126073624+0.08558164961018i)
   #(0.0
    -0.06629126073624-0.08558164961018i
    -0.11048543456040+0.08558164961018i
     0.66291260736239+0.17116329922036i
    -0.66291260736239-0.17116329922036i
     0.11048543456040-0.08558164961018i
     0.06629126073624+0.08558164961018i)))

(define inverse-filter
  (lambda (x)
    (list->vector
     (reverse (map conjugate (vector->list x))))))

(define inverse-cvdw6
  (inverse-fast-wavelet-transform
   (inverse-filter
   #(0.0
    -0.06629126073624+0.08558164961018i
     0.11048543456040+0.08558164961018i
     0.66291260736239-0.17116329922036i
     0.66291260736239-0.17116329922036i
     0.11048543456040+0.08558164961018i
    -0.06629126073624+0.08558164961018i))
   (inverse-filter
   #(0.0
    -0.06629126073624-0.08558164961018i
    -0.11048543456040+0.08558164961018i
     0.66291260736239+0.17116329922036i
    -0.66291260736239-0.17116329922036i
     0.11048543456040-0.08558164961018i
     0.06629126073624+0.08558164961018i))))

(define cvdw10
  (fast-wavelet-transform
   #(0.0
     0.01049245951230-0.02059043708702i
    -0.01712890812780-0.00872852869034i
    -0.08063970414533+0.11794747353812i
     0.15137970843150+0.09422365674476i
     0.64300323451588-0.18285216450551i
     0.64300323451588-0.18285216450551i
     0.15137970843150+0.09422365674476i
    -0.08063970414533+0.11794747353812i
    -0.01712890812780-0.00872852869034i
     0.01049245951230-0.02059043708702i)
   #(0.0
     0.01049245951230+0.02059043708702i
     0.01712890812780-0.00872852869034i
    -0.08063970414533-0.11794747353812i
    -0.15137970843150+0.09422365674476i
     0.64300323451588+0.18285216450551i
    -0.64300323451588-0.18285216450551i
     0.15137970843150-0.09422365674476i
     0.08063970414533+0.11794747353812i
    -0.01712890812780+0.00872852869034i
    -0.01049245951230-0.02059043708702i)))

(define inverse-cvdw10
  (inverse-fast-wavelet-transform
   (inverse-filter
   #(0.0
     0.01049245951230-0.02059043708702i
    -0.01712890812780-0.00872852869034i
    -0.08063970414533+0.11794747353812i
     0.15137970843150+0.09422365674476i
     0.64300323451588-0.18285216450551i
     0.64300323451588-0.18285216450551i
     0.15137970843150+0.09422365674476i
    -0.08063970414533+0.11794747353812i
    -0.01712890812780-0.00872852869034i
     0.01049245951230-0.02059043708702i))
   (inverse-filter
   #(0.0
     0.01049245951230+0.02059043708702i
     0.01712890812780-0.00872852869034i
    -0.08063970414533-0.11794747353812i
    -0.15137970843150+0.09422365674476i
     0.64300323451588+0.18285216450551i
    -0.64300323451588-0.18285216450551i
     0.15137970843150-0.09422365674476i
     0.08063970414533+0.11794747353812i
    -0.01712890812780+0.00872852869034i
    -0.01049245951230-0.02059043708702i))))

(define daubechies4
  (let ((root2 (* (sqrt 2.0) 4.0))
	(root3 (sqrt 3.0)))
    (fast-wavelet-transform
     (vector 0.0
	     (/ (+ 1.0 root3) root2)
	     (/ (+ 3.0 root3) root2)
	     (/ (- 3.0 root3) root2)
	     (/ (- 1.0 root3) root2))
     (vector 0.0
	     (/ (- 1.0 root3) root2)
	     (/ (- root3 3.0) root2)
	     (/ (+ 3.0 root3) root2)
	     (- (/ (+ 1.0 root3) root2))))))

(define inverse-daubechies4
  (let ((root2 (* (sqrt 2.0) 4.0))
	(root3 (sqrt 3.0)))
    (inverse-fast-wavelet-transform
     (vector (/ (- 1.0 root3) root2)
	     (/ (- 3.0 root3) root2)
	     (/ (+ 3.0 root3) root2)
	     (/ (+ 1.0 root3) root2)
	     0.0)
     (vector (- (/ (+ 1.0 root3) root2))
	     (/ (+ 3.0 root3) root2)
	     (/ (- root3 3.0) root2)
	     (/ (- 1.0 root3) root2)
	     0.0))))

(define legall3/5
  (fast-wavelet-transform
   #(-0.125 0.25 0.75 0.25 -0.125)
   #(-0.5 1.0 -0.5 0 0)))

(define inverse-legall3/5
  (inverse-fast-wavelet-transform
   #(0.5 1.0 0.5)
   #(0 0 -0.125 -0.25 0.75 -0.25 -0.125)))

(define daubechies7/9
  (fast-wavelet-transform
   #(0.02674875741080976
    -0.01686411844287495
    -0.07822326652898785
     0.2668641184428723
     0.6029490182363579
     0.2668641184428723
    -0.07822326652898785
    -0.01686411844287495
     0.02674875741080976)
   #(0.09127176311424948
    -0.05754352622849957
    -0.5912717631142470
     1.115087052456994
    -0.5912717631142470
    -0.05754352622849957
     0.09127176311424948
     0 0)))

(define inverse-daubechies7/9
  (inverse-fast-wavelet-transform
   #(-0.09127176311424948
     -0.05754352622849957
      0.5912717631142470
      1.115087052456994
      0.5912717631142470
     -0.05754352622849957
     -0.09127176311424948)
   #(0
     0
     0.02674875741080976
     0.01686411844287495
    -0.07822326652898785
    -0.2668641184428723
     0.6029490182363579
    -0.2668641184428723
    -0.07822326652898785
     0.01686411844287495
     0.02674875741080976)))

(define (shrinkage-old transform inverse-transform)
  (define (loop s threshold)
    (if (pair? s)
	(cons (loop (car s) threshold)
	      (loop (cdr s) threshold))
	(if (null? s)
	    ()
	    (shrink s threshold))))
  (lambda (image threshold)
    (inverse-transform
     (loop (transform image) threshold))))

(define (shrinkage transform inverse-transform)
  (define (loop ls threshold)
    (if (pair? (car ls))
	`(,(loop (car ls) threshold)
	  ,@(map (lambda (x) (shrink x threshold)) (cdr ls)))
	`(,(car ls)
	  ,@(map (lambda (x) (shrink x threshold)) (cdr ls)))))
  (lambda (image threshold)
    (inverse-transform
     (loop (transform image) threshold))))

(define (steerable-shrinkage transform inverse-transform)
  (define (loop trim threshold)
    (let ((next (cadr trim)))
      (if (pair? next)
	  `(,(map (lambda (x) (shrink x threshold)) (car trim))
	    ,(loop next threshold))
	  `(,(map (lambda (x) (shrink x threshold)) (car trim))
	    ,next))))
  (lambda (image threshold)
    (inverse-transform
     (let ((trim (transform image)))
       `(,(shrink (car trim) threshold)
	 ,(loop (cadr trim) threshold))))))

(define-macro define-steerable-filter
  (lambda (name A) `(define ,name (array-map ,(lambda (x) (/ x 10000)) ,A))))

(define-steerable-filter h0 #(
#(    14    -8   -19    -2    19    17     8     9    12     9     8    17    19    -2   -19    -8    14)
#(    -8     1    13     7   -12   -22   -23   -20   -18   -20   -23   -22   -12     7    13     1    -8)
#(    -19    13    20    -2   -19   -15    -2     2     1     2    -2   -15   -19    -2    20    13   -19)
#(    -2     7    -2   -11    -9    25    61    56    45    56    61    25    -9   -11    -2     7    -2)
#(    19   -12   -19    -9    24    46   -12   -96  -128   -96   -12    46    24    -9   -19   -12    19)
#(    17   -22   -15    25    46   -78  -154   -27    61   -27  -154   -78    46    25   -15   -22    17)
#(     8   -23    -2    61   -12  -154   179   453   373   453   179  -154   -12    61    -2   -23     8)
#(     9   -20     2    56   -96   -27   453  -614 -1903  -614   453   -27   -96    56     2   -20     9)
#(    12   -18     1    45  -128    61   373 -1903  5896 -1903   373    61  -128    45     1   -18    12)
#(     9   -20     2    56   -96   -27   453  -614 -1903  -614   453   -27   -96    56     2   -20     9)
#(     8   -23    -2    61   -12  -154   179   453   373   453   179  -154   -12    61    -2   -23     8)
#(    17   -22   -15    25    46   -78  -154   -27    61   -27  -154   -78    46    25   -15   -22    17)
#(    19   -12   -19    -9    24    46   -12   -96  -128   -96   -12    46    24    -9   -19   -12    19)
#(    -2     7    -2   -11    -9    25    61    56    45    56    61    25    -9   -11    -2     7    -2)
#(   -19    13    20    -2   -19   -15    -2     2     1     2    -2   -15   -19    -2    20    13   -19)
#(    -8     1    13     7   -12   -22   -23   -20   -18   -20   -23   -22   -12     7    13     1    -8)
#(    14    -8   -19    -2    19    17     8     9    12     9     8    17    19    -2   -19    -8    14)))

(define-steerable-filter l0 #(
#(     6   -13     7     4     1    -4    -8     0    -4     0    -8    -4     1     4     7   -13     6)
#(   -13    22    -3   -14    -4    14    16     1     1     1    16    14    -4   -14    -3    22   -13)
#(     7    -3   -19    22     6   -26   -19    -5    13    -5   -19   -26     6    22   -19    -3     7)
#(     4   -14    22   -12   -20    37    47    14   -12    14    47    37   -20   -12    22   -14     4)
#(     1    -4     6   -20    45   -17  -114   -66   -44   -66  -114   -17    45   -20     6    -4     1)
#(    -4    14   -26    37   -17   -79   137   244   190   244   137   -79   -17    37   -26    14    -4)
#(    -8    16   -19    47  -114   137    78  -487  -617  -487    78   137  -114    47   -19    16    -8)
#(     0     1    -5    14   -66   244  -487   229  1893   229  -487   244   -66    14    -5     1     0)
#(    -4     1    13   -12   -44   190  -617  1893  5236  1893  -617   190   -44   -12    13     1    -4)
#(     0     1    -5    14   -66   244  -487   229  1893   229  -487   244   -66    14    -5     1     0)
#(    -8    16   -19    47  -114   137    78  -487  -617  -487    78   137  -114    47   -19    16    -8)
#(    -4    14   -26    37   -17   -79   137   244   190   244   137   -79   -17    37   -26    14    -4)
#(     1    -4     6   -20    45   -17  -114   -66   -44   -66  -114   -17    45   -20     6    -4     1)
#(     4   -14    22   -12   -20    37    47    14   -12    14    47    37   -20   -12    22   -14     4)
#(     7    -3   -19    22     6   -26   -19    -5    13    -5   -19   -26     6    22   -19    -3     7)
#(   -13    22    -3   -14    -4    14    16     1     1     1    16    14    -4   -14    -3    22   -13)
#(     6   -13     7     4     1    -4    -8     0    -4     0    -8    -4     1     4     7   -13     6)))

(define-steerable-filter l1 #(
#(     1     2     1    -2    -8   -11   -12   -10    -9   -10   -12   -11    -8    -2     1     2     1)
#(     2    -1    -3    -2     3     8    12    13    13    13    12     8     3    -2    -3    -1     2)
#(     1    -3    -5     1    11    19    19    14    10    14    19    19    11     1    -5    -3     1)
#(    -2    -2     1     5     7     1   -11   -25   -31   -25   -11     1     7     5     1    -2    -2)
#(    -8     3    11     7   -11   -30   -39   -40   -39   -40   -39   -30   -11     7    11     3    -8)
#(   -11     8    19     1   -30   -35     4    63    91    63     4   -35   -30     1    19     8   -11)
#(   -12    12    19   -11   -39     4   141   300   371   300   141     4   -39   -11    19    12   -12)
#(   -10    13    14   -25   -40    63   300   559   671   559   300    63   -40   -25    14    13   -10)
#(    -9    13    10   -31   -39    91   371   671   801   671   371    91   -39   -31    10    13    -9)
#(   -10    13    14   -25   -40    63   300   559   671   559   300    63   -40   -25    14    13   -10)
#(   -12    12    19   -11   -39     4   141   300   371   300   141     4   -39   -11    19    12   -12)
#(   -11     8    19     1   -30   -35     4    63    91    63     4   -35   -30     1    19     8   -11)
#(    -8     3    11     7   -11   -30   -39   -40   -39   -40   -39   -30   -11     7    11     3    -8)
#(    -2    -2     1     5     7     1   -11   -25   -31   -25   -11     1     7     5     1    -2    -2)
#(     1    -3    -5     1    11    19    19    14    10    14    19    19    11     1    -5    -3     1)
#(     2    -1    -3    -2     3     8    12    13    13    13    12     8     3    -2    -3    -1     2)
#(     1     2     1    -2    -8   -11   -12   -10    -9   -10   -12   -11    -8    -2     1     2     1)))

(define-steerable-filter b31 #(
#(     6   -11    -2    11     6   -16   -29   -26   -23   -26   -29   -16     6    11    -2   -11     6)
#(    -4    -1     6    -7    -7    11    25    31    30    31    25    11    -7    -7     6    -1    -4)
#(    -5     4    -7    -7     2   -10   -30   -29   -21   -29   -30   -10     2    -7    -7     4    -5)
#(     1    -6   -10     5   -14   -15    32    82   106    82    32   -15   -14     5   -10    -6     1)
#(    -5    -4     4   -18   -18    29    -6   -66   -71   -66    -6    29   -18   -18     4    -4    -5)
#(     0    -7    -9   -11    15   -50  -111    89   264    89  -111   -50    15   -11    -9    -7     0)
#(    -5    -1    -6    -3   -30   -70    37    54   -38    54    37   -70   -30    -3    -6    -1    -5)
#(    -1    -2    -6     0   -26   -12  -207 -1192 -1936 -1192  -207   -12   -26     0    -6    -2    -1)
 #(    0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0)
#(     1     2     6     0    26    12   207  1192  1936  1192   207    12    26     0     6     2     1)
#(     5     1     6     3    30    70   -37   -54    38   -54   -37    70    30     3     6     1     5)
#(     0     7     9    11   -15    50   111   -89  -264   -89   111    50   -15    11     9     7     0)
#(     5     4    -4    18    18   -29     6    66    71    66     6   -29    18    18    -4     4     5)
#(    -1     6    10    -5    14    15   -32   -82  -106   -82   -32    15    14    -5    10     6    -1)
#(     5    -4     7     7    -2    10    30    29    21    29    30    10    -2     7     7    -4     5)
#(     4     1    -6     7     7   -11   -25   -31   -30   -31   -25   -11     7     7    -6     1     4)
#(    -6    11     2   -11    -6    16    29    26    23    26    29    16    -6   -11     2    11    -6)))

(define-steerable-filter b32 #(
#(    11   -10    -4     7     2    -4   -14    -8    -8    -6    -7    -7     5     5     6    -4    -1)
#(    -9     0    15    -6    -8    -3     6     5     0     4     3    10     2     1     2     2    11)
#(     0    18    -7    -7    14    -6   -18   -23   -13    -9    -8    -2    -4     9     5     5     2)
#(    18     1    -3    22   -19   -19    13    16     7    -3     5     2     9     3     1    10    -1)
#(    15     1    26    -7    -3    63   -12   -69   -60   -34    -1    -6     6     3     9     6     1)
#(    -4    15    10     2    95   -31  -111   -11   -10    -7     9    25    10     7    13    -8    19)
#(   -21    25   -19    55    45   -59   297   -36  -395   -36    17    49    25    -8    25   -12    25)
#(   -19    23   -34    66   -67   128   283 -1316 -1105   514   214    19    76   -26    30   -15    22)
#(   -20    14   -30    52  -107    94  -275 -1757     0  1757   275   -94   107   -52    30   -14    20)
#(   -22    15   -30    26   -76   -19  -214  -514  1105  1316  -283  -128    67   -66    34   -23    19)
#(   -25    12   -25     8   -25   -49   -17    36   395    36  -297    59   -45   -55    19   -25    21)
#(   -19     8   -13    -7   -10   -25    -9     7    10    11   111    31   -95    -2   -10   -15     4)
#(    -1    -6    -9    -3    -6     6     1    34    60    69    12   -63     3     7   -26    -1   -15)
#(     1   -10    -1    -3    -9    -2    -5     3    -7   -16   -13    19    19   -22     3    -1   -18)
#(    -2    -5    -5    -9     4     2     8     9    13    23    18     6   -14     7     7   -18     0)
#(   -11    -2    -2    -1    -2   -10    -3    -4     0    -5    -6     3     8     6   -15     0     9)
#(     1     4    -6    -5    -5     7     7     6     8     8    14     4    -2    -7     4    10   -11)))

(define-steerable-filter b33 #(
#(     1     4    -6    -5    -5     7     7     6     8     8    14     4    -2    -7     4    10   -11)
#(   -11    -2    -2    -1    -2   -10    -3    -4     0    -5    -6     3     8     6   -15     0     9)
#(    -2    -5    -5    -9     4     2     8     9    13    23    18     6   -14     7     7   -18     0)
#(     1   -10    -1    -3    -9    -2    -5     3    -7   -16   -13    19    19   -22     3    -1   -18)
#(    -1    -6    -9    -3    -6     6     1    34    60    69    12   -63     3     7   -26    -1   -15)
#(   -19     8   -13    -7   -10   -25    -9     7    10    11   111    31   -95    -2   -10   -15     4)
#(   -25    12   -25     8   -25   -49   -17    36   395    36  -297    59   -45   -55    19   -25    21)
#(   -22    15   -30    26   -76   -19  -214  -514  1105  1316  -283  -128    67   -66    34   -23    19)
#(   -20    14   -30    52  -107    94  -275 -1757     0  1757   275   -94   107   -52    30   -14    20)
#(   -19    23   -34    66   -67   128   283 -1316 -1105   514   214    19    76   -26    30   -15    22)
#(   -21    25   -19    55    45   -59   297   -36  -395   -36    17    49    25    -8    25   -12    25)
#(    -4    15    10     2    95   -31  -111   -11   -10    -7     9    25    10     7    13    -8    19)
#(    15     1    26    -7    -3    63   -12   -69   -60   -34    -1    -6     6     3     9     6     1)
#(    18     1    -3    22   -19   -19    13    16     7    -3     5     2     9     3     1    10    -1)
#(     0    18    -7    -7    14    -6   -18   -23   -13    -9    -8    -2    -4     9     5     5     2)
#(    -9     0    15    -6    -8    -3     6     5     0     4     3    10     2     1     2     2    11)
#(    11   -10    -4     7     2    -4   -14    -8    -8    -6    -7    -7     5     5     6    -4    -1)))

(define (reflect kernel)
  (list->vector
   (reverse
    (vector->list
     (vector-map
      (lambda (x) (list->vector (reverse (vector->list x))))
      kernel)))))

(define l1*4 (array-map (lambda (x) (* x 4.0)) l1))
(define -b31 (reflect b31))
(define -b32 (reflect b32))
(define -b33 (reflect b33))

(define (make-steerable-pyramid-transform h0 l0 l1 . bs)
  (let ((len (array-rows l1))
	(-bs (map reflect bs)))
    (define (loop *l1)
      (let ((r (image-rows *l1))
	    (c (image-cols *l1)))
	(if (or (odd? r) (odd? c) (< r len) (< c len))
	    *l1
	    (list (map (lambda (-b) (convolve *l1 -b)) -bs)
		  (loop (downsample (convolve *l1 l1)))))))
    (lambda (image)
      (list (convolve image h0)
	    (loop (convolve image l0))))))

(define (make-inverse-steerable-pyramid-transform h0 l0 l1 . bs)
  (let ((l1*4 (array-map (lambda (x) (* x 4.0)) l1)))
    (define (loop bl)
      (if (or (image? bl) (complex-image? bl))
	  bl
	  (name-parts-of bl (*bs *l1)
	    (+ (apply + (map convolve *bs bs))
	       (convolve (upsample (loop *l1)) l1*4)))))
    (lambda (hl)
      (name-parts-of hl (*h0 bl)
        (+ (convolve *h0 h0)
	   (convolve (loop bl) l0))))))

(define steerable-pyramid3
  (make-steerable-pyramid-transform
   h0 l0 l1 b31 b32 b33))

(define inverse-steerable-pyramid3
  (make-inverse-steerable-pyramid-transform
   h0 l0 l1 b31 b32 b33))
	      
;; Bandpass filter, B1(x,y)

(define-steerable-filter b41 #(
#(     4    -8    -3     9     5   -14   -26   -25   -23   -25   -26   -14     5     9    -3    -8     4)
#(    -5    -1     3    -6    -5    11    26    30    28    30    26    11    -5    -6     3    -1    -5)
#(    -6     0    -7    -7     1    -7   -22   -26   -24   -26   -22    -7     1    -7    -7     0    -6)
#(    -3    -9   -11     0   -11    -9    36    80    96    80    36    -9   -11     0   -11    -9    -3)
#(    -8    -7    -4   -19   -18    19     6   -48   -68   -48     6    19   -18   -19    -4    -7    -8)
#(    -5   -10   -14   -15    -5   -51   -77    99   248    99   -77   -51    -5   -15   -14   -10    -5)
#(    -8    -5   -10   -16   -46   -76    -7    80    97    80    -7   -76   -46   -16   -10    -5    -8)
#(    -3    -5    -8   -11   -33   -58  -316 -1070 -1573 -1070  -316   -58   -33   -11    -8    -5    -3)
#(     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0)
#(     3     5     8    11    33    58   316  1070  1573  1070   316    58    33    11     8     5     3)
#(     8     5    10    16    46    76     7   -80   -97   -80     7    76    46    16    10     5     8)
#(     5    10    14    15     5    51    77   -99  -248   -99    77    51     5    15    14    10     5)
#(     8     7     4    19    18   -19    -6    48    68    48    -6   -19    18    19     4     7     8)
#(     3     9    11     0    11     9   -36   -80   -96   -80   -36     9    11     0    11     9     3)
#(     6     0     7     7    -1     7    22    26    24    26    22     7    -1     7     7     0     6)
#(     5     1    -3     6     5   -11   -26   -30   -28   -30   -26   -11     5     6    -3     1     5)
#(    -4     8     3    -9    -5    14    26    25    23    25    26    14    -5    -9     3     8    -4)))

;; Bandpass filter, B2(x,y)

(define-steerable-filter b42 #(
#(    11    -8     0    13    10     1   -10    -7   -10   -12   -16   -14    -2     1     2    -9     0)
#(    -8     1    20     1    -2     7    15    11     1     1    -1     3    -4    -6    -2     0     9)
#(     0    20    -3     0    24     6   -12   -22   -18   -18   -18   -13   -10     3     0     2    -2)
#(    13     1     0    30    -7    -7    34    34    11   -12    -9    -6     0     0    -3     6    -1)
#(    10    -2    24    -7     9    92    27   -57   -77   -64   -32   -17     0     0    10     4     2)
#(     1     7     6    -7    92    -6   -64    63   -10   -66   -30     0    17     6    13    -3    14)
#(   -10    15   -12    34    27   -64   355   214  -428  -266     0    30    32     9    18     1    16)
#(    -7    11   -22    34   -57    63   214  -998 -1292     0   266    66    64    12    18    -1    12)
#(   -10     1   -18    11   -77   -10  -428 -1292     0  1292   428    10    77   -11    18    -1    10)
#(   -12     1   -18   -12   -64   -66  -266     0  1292   998  -214   -63    57   -34    22   -11     7)
#(   -16    -1   -18    -9   -32   -30     0   266   428  -214  -355    64   -27   -34    12   -15    10)
#(   -14     3   -13    -6   -17     0    30    66    10   -63    64     6   -92     7    -6    -7    -1)
#(    -2    -4   -10     0     0    17    32    64    77    57   -27   -92    -9     7   -24     2   -10)
#(     1    -6     3     0     0     6     9    12   -11   -34   -34     7     7   -30     0    -1   -13)
#(     2    -2     0    -3    10    13    18    18    18    22    12    -6   -24     0     3   -20     0)
#(    -9     0     2     6     4    -3     1    -1    -1   -11   -15    -7     2    -1   -20    -1     8)
#(     0     9    -2    -1     2    14    16    12    10     7    10    -1   -10   -13     0     8   -11)))

;; Bandpass filter, B3(x,y)

(define-steerable-filter b43 #(
#(     4    -5    -6    -3    -8    -5    -8    -3     0     3     8     5     8     3     6     5    -4)
#(    -8    -1     0    -9    -7   -10    -5    -5     0     5     5    10     7     9     0     1     8)
#(    -3     3    -7   -11    -4   -14   -10    -8     0     8    10    14     4    11     7    -3     3)
#(     9    -6    -7     0   -19   -15   -16   -11     0    11    16    15    19     0     7     6    -9)
#(     5    -5     1   -11   -18    -5   -46   -33     0    33    46     5    18    11    -1     5    -5)
#(   -14    11    -7    -9    19   -51   -76   -58     0    58    76    51   -19     9     7   -11    14)
#(   -26    26   -22    36     6   -77    -7  -316     0   316     7    77    -6   -36    22   -26    26)
#(   -25    30   -26    80   -48    99    80 -1070     0  1070   -80   -99    48   -80    26   -30    25)
#(   -23    28   -24    96   -68   248    97 -1573     0  1573   -97  -248    68   -96    24   -28    23)
#(   -25    30   -26    80   -48    99    80 -1070     0  1070   -80   -99    48   -80    26   -30    25)
#(   -26    26   -22    36     6   -77    -7  -316     0   316     7    77    -6   -36    22   -26    26)
#(   -14    11    -7    -9    19   -51   -76   -58     0    58    76    51   -19     9     7   -11    14)
#(     5    -5     1   -11   -18    -5   -46   -33     0    33    46     5    18    11    -1     5    -5)
#(     9    -6    -7     0   -19   -15   -16   -11     0    11    16    15    19     0     7     6    -9)
#(    -3     3    -7   -11    -4   -14   -10    -8     0     8    10    14     4    11     7    -3     3)
#(    -8    -1     0    -9    -7   -10    -5    -5     0     5     5    10     7     9     0     1     8)
#(     4    -5    -6    -3    -8    -5    -8    -3     0     3     8     5     8     3     6     5    -4)))

;; Bandpass filter, B4(x,y)

(define-steerable-filter b44 #(
#(     0     9    -2    -1     2    14    16    12    10     7    10    -1   -10   -13     0     8   -11)
#(    -9     0     2     6     4    -3     1    -1    -1   -11   -15    -7     2    -1   -20    -1     8)
#(     2    -2     0    -3    10    13    18    18    18    22    12    -6   -24     0     3   -20     0)
#(     1    -6     3     0     0     6     9    12   -11   -34   -34     7     7   -30     0    -1   -13)
#(    -2    -4   -10     0     0    17    32    64    77    57   -27   -92    -9     7   -24     2   -10)
#(   -14     3   -13    -6   -17     0    30    66    10   -63    64     6   -92     7    -6    -7    -1)
#(   -16    -1   -18    -9   -32   -30     0   266   428  -214  -355    64   -27   -34    12   -15    10)
#(   -12     1   -18   -12   -64   -66  -266     0  1292   998  -214   -63    57   -34    22   -11     7)
#(   -10     1   -18    11   -77   -10  -428 -1292     0  1292   428    10    77   -11    18    -1    10)
#(    -7    11   -22    34   -57    63   214  -998 -1292     0   266    66    64    12    18    -1    12)
#(   -10    15   -12    34    27   -64   355   214  -428  -266     0    30    32     9    18     1    16)
#(     1     7     6    -7    92    -6   -64    63   -10   -66   -30     0    17     6    13    -3    14)
#(    10    -2    24    -7     9    92    27   -57   -77   -64   -32   -17     0     0    10     4     2)
#(    13     1     0    30    -7    -7    34    34    11   -12    -9    -6     0     0    -3     6    -1)
#(     0    20    -3     0    24     6   -12   -22   -18   -18   -18   -13   -10     3     0     2    -2)
#(    -8     1    20     1    -2     7    15    11     1     1    -1     3    -4    -6    -2     0     9)
#(    11    -8     0    13    10     1   -10    -7   -10   -12   -16   -14    -2     1     2    -9     0)))

(define steerable-pyramid4
  (make-steerable-pyramid-transform h0 l0 l1 b41 b42 b43 b44))

(define inverse-steerable-pyramid4
  (make-inverse-steerable-pyramid-transform h0 l0 l1 b41 b42 b43 b44))

(define (denoise image threshold)
  (define (wrapper image threshold)
    (let ((r (image-rows image))
	  (c (image-cols image)))
      (image-crop
       ((steerable-shrinkage steerable-pyramid4 inverse-steerable-pyramid4)
	(image-pad image (nlpot r) (nlpot c))
	threshold)
       0 0 r c)))
  (if (color-image? image)
      (apply rgb->color-image
	     (map (lambda (x) (wrapper x threshold))
		  (color-image->rgb image)))
      (wrapper image threshold)))

(define (denoise-complex image threshold)
  (define (wrapper image threshold)
    (let ((r (image-rows image))
	  (c (image-cols image)))
      (image-crop
       (real-part
	((shrinkage cvdw10 inverse-cvdw10)
	 (image-pad image (nlpot r) (nlpot c))
	 threshold))
       0 0 r c)))
  (if (color-image? image)
      (apply rgb->color-image
	     (map (lambda (x) (wrapper x threshold))
		  (color-image->rgb image)))
      (wrapper image threshold)))

(define (log2 x)
  (/ (log x) (log 2)))

(define (nlpot x)
  (let* ((y (log2 x)) (z (floor y)))
    (if (= y z)
	x
	(expt 2 (+ z 1)))))

(define (left-to-right-color-image x y)
  (apply rgb->color-image (map left-to-right  (color-image->rgb x) (color-image->rgb y))))
