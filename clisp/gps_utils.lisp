;;; Coordinate transformations functions

;;; Some constants...everyone needs those...EVERYONE
(defvar *rad2deg* (/ 180.0 pi))
(defvar *deg2rad* (/ pi 180.0))
(defvar *fourthpi* (/ pi 4.0))

;;; ellipsoid class, constructor and print methods (for repl support)...

(defclass ellipsoid()
  ((name :accessor name :initarg :name :initform nil)
   (radius :accessor radius :initarg :radius :initform nil)
   (ecc :accessor ecc :initarg :ecc :initform nil)))

(defun make-ellipsoid (name radius ecc)
  (make-instance 'ellipsoid :name name :radius radius :ecc ecc))

(defmacro make-ellipsoids (&rest ellipsoids)
  `(loop for el in ',ellipsoids
	collecting (apply #'make-ellipsoid el)))

(defmethod print-object ((ellipsoid ellipsoid) stream)
  (prin1 (list (class-name (class-of ellipsoid))
	       :name (name ellipsoid)
	       :radius (radius ellipsoid)
	       :eccentricity-squared (ecc ellipsoid))
	 stream))

;;; collection of ellipsoids for utm-to-ll conversion. Realistically, we'll
;;; only ever be using wgs84 but who knows...
(defvar *ellipsoids*
  (make-ellipsoids ("Airy" 6377563 0.00667054)
		   ("Australian National" 6378160 0.006694542)
		   ("Bessel 1841" 6377397 0.006674372)
		   ("Bessel 1841 (Nambia)" 6377484 0.006674372)
		   ("Clarke 1866" 6378206 0.006768658)
		   ("Clarke 1880" 6378249 0.006803511)
		   ("Everest" 6377276 0.006637847)
		   ("Fischer 1960 (Mercury)" 6378166 0.006693422)
		   ("Fischer 1968" 6378150 0.006693422)
		   ("GRS 1967" 6378160 0.006694605)
		   ("GRS 1980" 6378137 0.00669438)
		   ("Helmert 1906" 6378200 0.006693422)
		   ("Hough" 6378270 0.00672267)
		   ("International" 6378388 0.00672267)
		   ("Krassovsky" 6378245 0.006693422)
		   ("Modified Airy" 6377340 0.00667054)
		   ("Modified Everest" 6377304 0.006637847)
		   ("Modified Fischer 1960" 6378155 0.006693422)
		   ("South American 1969" 6378160 0.006694542)
		   ("WGS 60" 6378165 0.006693422)
		   ("WGS 66" 6378145 0.006694542)
		   ("WGS-72" 6378135 0.006694318)
		   ("WGS-84" 6378137 0.00669438)))

;;; Macro to allow for a more convenient interval notation used in the calculations
;;; that will follow.
(defmacro in-interval (val low-brack low high high-brack)
  (let ((lt (gensym))
	(gt (gensym)))
    `(let ((,gt (if (equal ',low-brack ']) #'> #'>=))
	   (,lt (if (equal ',high-brack '[) #'< #'<=)))
       (when (and (funcall ,gt ,val ,low)
		  (funcall ,lt ,val ,high)) t))))

;;; utm zone number and zone designator functions. These are only needed for the
;;; ll-to-utm conversion, which I will implement once my brain has recovered.
;;; TODO: Implement ll-to-utm

;;; Get the utm zone number for the given latitude and longitude
(defun get-utm-zone-number (latitude longitude)
  (let* ((long-temp (- (+ longitude 180.0)
		       (* 360 (car (floor (/ (+ longitude 180.0) 360.0))))
		       180.0))
	 (zone-number (+ (car (floor (/ (+ 180.0 long-temp) 6.0))) 1.0))
	 (utm-designator (get-utm-designator latitude)))
    (cond 
      ((and (in-interval latitude [ 56.0 64.0 [)
	       (in-interval long-temp [ 3.0 12.0 [)) (list 32 utm-designator))
      ((in-interval latitude [ 72.0 84.0 [)
       (cond 
	 ((in-interval long-temp [ 0.0   9.0 [) (list 31 utm-designator))
	 ((in-interval long-temp [ 9.0  21.0 [) (list 33 utm-designator))
	 ((in-interval long-temp [ 21.0 33.0 [) (list 35 utm-designator))
	 ((in-interval long-temp [ 33.0 42.0 [) (list 37 utm-designator))
	 (t (list zone-number utm-designator)))
       (t (list zone-number utm-designator))))))

;;; Get the utm letter designator for a given latitude (in degrees). 
;;; I've ommited a,b and y,z which which cover the far sout and far north
;;; arctic reagions. Should anyone ever want to simulate a dragline in those
;;; regions, knock yourself out and add the bands :)
(defun get-utm-zone-designator (latitude)
  (cond 
    ((in-interval latitude [ 72   84 [) (char-int #\x))
    ((in-interval latitude [ 64   72 [) (char-int #\w))
    ((in-interval latitude [ 56   64 [) (char-int #\v))
    ((in-interval latitude [ 48   56 [) (char-int #\u))
    ((in-interval latitude [ 40   48 [) (char-int #\t))
    ((in-interval latitude [ 32   40 [) (char-int #\s))
    ((in-interval latitude [ 24   32 [) (char-int #\r))
    ((in-interval latitude [ 16   24 [) (char-int #\q))
    ((in-interval latitude [  8   16 [) (char-int #\p))
    ((in-interval latitude [  0    8 [) (char-int #\n))
    ((in-interval latitude [ -8    0 [) (char-int #\m))
    ((in-interval latitude [ -16  -8 [) (char-int #\l))
    ((in-interval latitude [ -24 -16 [) (char-int #\k))
    ((in-interval latitude [ -32 -24 [) (char-int #\j))
    ((in-interval latitude [ -40 -32 [) (char-int #\h))
    ((in-interval latitude [ -48 -40 [) (char-int #\g))
    ((in-interval latitude [ -56 -48 [) (char-int #\f))
    ((in-interval latitude [ -64 -56 [) (char-int #\e))
    ((in-interval latitude [ -72 -64 [) (char-int #\d))
    ((in-interval latitude [ -80 -72 [) (char-int #\c))
    (t (char-int #\z))))

;;; Are we on the northern hemisphere?
(defun northern-hemisphere-p (zone-designator)
  (when (>= (- zone-designator (char-int #\m)) ) t))

;;; Beware, ugliness lies within...
(defun utm-to-lat-long (ellipsoid northing easting utm-zone)
  (let* ((k0 0.9996)
	 (a (radius ellipsoid))
	 (ecc-squared (ecc ellipsoid))
	 (sqrt-ecc-squared-less-1 (sqrt (- 1 ecc-squared)))
	 (e1 (/ (- 1 sqrt-ecc-squared-less-1)
		(+ 1 sqrt-ecc-squared-less-1)))
	 (zone-number (nth 0 utm-zone))
	 (zone-designator (nth 1 utm-zone))
         (x (- easting 500000))
	 (y (if (northern-hemisphere-p zone-designator) northing (- northing 10000000.0)))
	 (long-origin (+ (- (* (- zone-number 1) 6) 180) 3))
	 (long-origin-rad (* *deg2rad* long-origin))
	 (ecc-prime-squared (/ ecc-squared (- 1 ecc-squared)))
	 (m (/ y k0))
	 (mu (/ m (* a (- 1.0 (/ ecc-squared 4.0) (* 3.0 ecc-squared ecc-squared (/ 1.0 64.0)) (* 5 ecc-squared ecc-squared ecc-squared (/ 1.0 256.0))))))
	 (phi1-rad (+ mu 
		      (* (- (* 3.0 (/ e1 2.0))
			    (* 27 e1 e1 e1 (/ 1.0 32.0)))
			 (sin (* 2.0 mu)))
		      (* (- (* 21 e1 e1 (/ 1.0 16.0))
			    (* 55 e1 e1 e1 e1 (/ 1.0 32.0)))
			 (sin (* 4.0 mu)))
		      (* 151.0 e1 e1 e1 (/ 1.0 96.0)) (sin (* 6.0 mu))))
	 (phi1 (* phi1-rad *rad2deg*))
	 (n1 (/ a (sqrt (- 1.0 (* ecc-squared (sin phi1-rad) (sin phi1-rad))))))
	 (t1 (* (tan phi1-rad) (tan phi1-rad)))
	 (c1 (* ecc-prime-squared (cos phi1-rad) (cos phi1-rad)))
	 (r1 (/ (* a (- 1 ecc-squared))
		(pow (- 1 (* ecc-squared (sin phi1-rad) (sin phi1-rad))) 1.5)))
	 (d (/ x (* n1 k0)))
	 (lat-rad (- phi1-rad
		     (* n1 
			(/ (tan phi1-rad) r1)
			(+ (* d d 0.5)
			   (* -1.0
			      (+ 5.0
				 (* 3.0 t1)
				 (* 10.0 c1)
				 (* -4.0 c1 c1)
				 (* -9 ecc-prime-squared))
			      d d d d (/ 1.0 24.0))
			   (* (+ 61
				 (* 90 t1)
				 (* 298 c1)
				 (* 45.0 t1 t1)
				 (* -252.0 ecc-prime-squared)
				 (* -3.0 c1 c1))
			      d d d d d d (/ 1.0 720.0))))))
	 (latitude (* lat-rad *rad2deg*))
	 (long-rad (/ (+ 
		       (- d 
			  (* (+ 1 (* 2.0 t1) c1)
			     d d d (/ 1.0 6.0)))
		       (* (- 5 (* 2.0 c1) 
			     (* -28.0 t1)
			     (* 3.0 c1 c1)
			     (* -8.0 ecc-prime-squared)
			     (* -24.0 t1 t1))
			  d d d d d (/ 1.0 120.0)))
		      (cos phi1-rad)))
	 (longitude (* long-rad *rad2deg*)))
       (list latitude longitude)))


