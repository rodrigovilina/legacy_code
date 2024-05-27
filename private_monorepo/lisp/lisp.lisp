(defconstant +my-multiline-string+ "199
200
208
210
200
207
240
269
260
263")

(defun split-multiline-string (s)
  (with-input-from-string (stream s)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun strings-to-integers (strings)
  (mapcar #'parse-integer strings))

(defun create-pairs (numbers)
  (loop for i from 0 to (- (length numbers) 2)
        collect (subseq numbers i (+ i 2))))

(defun compare-pairs (pairs)
  (mapcar (lambda (pair) (< (first pair) (second pair))) pairs))

(defun count-trues (list)
  (count t list))

(defun main ()
  (let* ((lines (split-multiline-string +my-multiline-string+))
         (numbers (strings-to-integers lines))
         (pairs (create-pairs numbers))
         (comparisons (compare-pairs pairs))
         (count (count-trues comparisons)))
    (print count)))

(main)
