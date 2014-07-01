(defun northern-hemisphere-p (zone-designator)
  (when (>= (- zone-designator (char-int #\m)) ) true))
