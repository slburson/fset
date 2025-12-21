(in-package :FSet/Jzon/Test)

(define-tuple-key +json-color+ :type (or null string))
(define-tuple-key +json-sizes+ :default (seq) :type seq)
(define-tuple-key +json-sizes-list+ :default '() :type list)
(define-tuple-key +json-visible?+ :default nil :type boolean)

(defmethod coerce-tuple-key (key (pkg (eql :fset/jzon/test)))
  (let ((str (symbol-name (tuple-key-name key))))
    ;; Strip leading "+JSON-" and trailing "+", and downcase
    (string-downcase (subseq str 6 (1- (length str))))))

(defun test-fset/jzon ()
  (macrolet ((test (form)
               `(unless ,form
                  (error "Test failed: ~S" ',form))))
    (test (equal? (parse "2") 2))
    (test (equal? (parse "[2]") (seq 2)))
    (test (equal? (parse "[2, [7, 19, [42]]]") (seq 2 (seq 7 19 (seq 42)))))
    (test (equal? (parse "[2, [7, 19, [42],]]" :allow-trailing-comma t) (seq 2 (seq 7 19 (seq 42)))))
    (test (handler-case (progn (parse "[2, [7, 19, [42]]]" :max-depth 2)
			       nil)
	    (json-parse-limit-error () t)))
    (test (equal? (parse "{\"foo\": 42}") (replay-map ("foo" 42))))
    (test (equal? (parse "{\"foo\": /* a comment */ 42}" :allow-comments t) (replay-map ("foo" 42))))
    (test (equal? (parse "{\"foo\": 42, \"bar\": 27}") (replay-map ("foo" 42) ("bar" 27))))
    (test (equal? (with-parser (p "{\"foo\": 42, \"bar\": 27}[3, 17]" :allow-multiple-content t)
		    (list (parse-top-level p) (parse-top-level p)))
		  (list (replay-map ("foo" 42) ("bar" 27)) (seq 3 17))))
    (test (equal? (parse "[\"gubbish\", {\"color\": \"blue\", \"sizes\": [4, 7, 11]}]")
		  (seq "gubbish" (replay-map ("color" "blue") ("sizes" (seq 4 7 11))))))
    (test (equal? (parse "[\"gubbish\", {\"color\": \"blue\", \"sizes\": [4, 7, 11]}]" :object-type 'map)
		  (seq "gubbish" (map ("color" "blue") ("sizes" (seq 4 7 11))))))
    (test (equal? (parse "[\"gubbish\", {\"color\": \"blue\", \"sizes\": [4, 7, 11]}]"
			 :object-type 'tuple :key-package :fset/jzon/test
			 :key-prefix '#:+json- :key-suffix #\+)
		  (seq "gubbish" (tuple (+json-color+ "blue") (+json-sizes+ (seq 4 7 11))))))

    (test (equal? (stringify 2) "2"))
    (test (equal? (stringify (seq 2)) "[2]"))
    (test (equal? (stringify (seq 2 (seq 7 19 (seq 42)))) "[2,[7,19,[42]]]"))
    (test (equal? (stringify (replay-map ("foo" 42) ("bar" 27))) "{\"foo\":42,\"bar\":27}"))
    ;; We don't test on plain `map' because the ordering may be implementation-dependent,
    ;; and it goes through the same code path as `replay-map' anyway.
    (test (equal? (stringify (seq "gubbish" (replay-map ("color" "blue") ("sizes" (seq 4 7 11)))))
		  "[\"gubbish\",{\"color\":\"blue\",\"sizes\":[4,7,11]}]"))
    (test (equal? (stringify (seq "gubbish" (tuple (+json-color+ nil) (+json-sizes-list+ nil) (+json-visible?+ nil))))
		  "[\"gubbish\",{\"color\":null,\"sizes-list\":[],\"visible?\":false}]"))
    "Passed."))
