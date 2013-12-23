Regex patterns are supported with a special reader macro.
	
(= __ (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

