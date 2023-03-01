;; stringp  (stringp object) ==  (typep object 'string')

;; transform a string in a char array
(defun jsonparse (JSONString)
  (if (not (stringp JSONString)) (error "Not a string!")
    (json-parse-charlist (clean-charlist
			  (string-to-list JSONString)))))

(defun string-to-list (JSONString)
  (if (= (length JSONString) 0) nil
    (cons (char JSONString 0)
	  (string-to-list (subseq JSONString 1)))))


;; clean-charlist handles whitespaces
(defun clean-charlist (charlist)
  (remove #\Return
	  (remove #\Tab 
		  (remove #\Newline 
			  (remove #\Space charlist)))))

;;  parse the created charlist
(defun json-parse-charlist (charlist)
  (cond ((and (eq (car charlist) #\{) (eq (car (last charlist)) #\}))
         (cons 'JSONOBJ (nth-value 0 (parse-members (remove-braces charlist)))))
        ((and (eq (car charlist) #\[) (eq (car (last charlist)) #\])) 
         (cons 'JSONARRAY (nth-value 0 (parse-array (remove-braces charlist)))))
        (T (error "Syntax error! Braces aren't balanced."))))
 
;; remove braces from charlist
(defun remove-braces (charlist)
  (cond ((eq (car (last charlist)) #\})
         (cdr (reverse (cdr (reverse charlist)))))
        ((eq (car (last charlist)) #\])
         (cdr (reverse (cdr (reverse charlist)))))))

(defun parse-members (charlist)
  (values 
   (if (null charlist) nil
     (cons (nth-value 0 (parse-pair charlist)) 
           (parse-members (cdr (nth-value 1 (parse-pair charlist))))))
   charlist))

(defun parse-array (charlist)
  (values
   (if (null charlist) nil
     (cons (nth-value 0 (parse-element charlist))
           (parse-array (cdr (nth-value 1 (parse-element charlist))))))
   charlist))

(defun parse-pair (charlist)
  (values 
   (list (nth-value 0 (parse-attribute  charlist)) 
         (nth-value 0 (parse-value (nth-value 1 (parse-attribute charlist)))))
   (nth-value 1 (parse-value (nth-value 1 (parse-attribute charlist))))))

(defun parse-element (charlist)
  (cond  ((and (>= (- (char-int (car charlist)) 48) 0) 
              (< (- (char-int (car charlist)) 48) 10)) 
         (values (parse-number (nth-value 0 (create-value-list charlist)))
                 (remove-parsed-chars (nth-value 0 (create-value-list charlist)) 
                                      (nth-value 1 (create-value-list charlist)))))
        ((eq (car charlist) #\")
         (values (convert-to-string (nth-value 0 (create-value-list charlist)))
                 (remove-parsed-chars (nth-value 0 (create-value-list charlist)) 
                                      (nth-value 1 (create-value-list charlist)))))
	((or (eq (car charlist) #\t) 
             (eq (car charlist) #\f)
             (eq (car charlist) #\n))
         (values (parse-atom (nth-value 0 (create-value-list charlist)))
                 (remove-parsed-chars (nth-value 0 (create-value-list charlist)) 
                                      (nth-value 1 (create-value-list charlist)))))
        ((eq (car charlist) #\[) 
         (values (json-parse-charlist (nth-value 0 (create-array-list charlist)))
                 (remove-parsed-chars (nth-value 0 (create-array-list charlist)) 
                                      (nth-value 1 (create-array-list charlist)))))
        ((eq (car charlist) #\{) 
         (values (json-parse-charlist (nth-value 0 (create-obj-list charlist)))
                 (remove-parsed-chars (nth-value 0 (create-obj-list charlist)) 
                                      (nth-value 1 (create-obj-list charlist)))))))

(defun parse-attribute (charlist)
  (values 
   (convert-to-string (nth-value 0 (parse-string charlist)))
   (remove-parsed-chars (nth-value 0 (parse-string charlist))
                        (nth-value 1 (parse-string charlist)))))

(defun parse-value (charlist)
  (cond ((eq (car charlist) #\:)
          (parse-value (cdr charlist)))
        ((and (>= (- (char-int (car charlist)) 48) 0) 
              (< (- (char-int (car charlist)) 48) 10)) 
         (values (parse-number (nth-value 0 (create-value-list charlist)))
                 (remove-parsed-chars (nth-value 0 (create-value-list charlist)) 
                                      (nth-value 1 (create-value-list charlist)))))
        ((eq (car charlist) #\")
         (values (convert-to-string (nth-value 0 (create-value-list charlist)))
                 (remove-parsed-chars (nth-value 0 (create-value-list charlist)) 
                                      (nth-value 1 (create-value-list charlist)))))
	((or (eq (car charlist) #\t) 
             (eq (car charlist) #\f)
             (eq (car charlist) #\n))
         (values (parse-atom (nth-value 0 (create-value-list charlist)))
                 (remove-parsed-chars (nth-value 0 (create-value-list charlist)) 
                                      (nth-value 1 (create-value-list charlist)))))
        ((eq (car charlist) #\[) 
         (values (json-parse-charlist (nth-value 0 (create-array-list charlist)))
                 (remove-parsed-chars (nth-value 0 (create-array-list charlist)) 
                                      (nth-value 1 (create-array-list charlist)))))
        ((eq (car charlist) #\{) 
         (values (json-parse-charlist (nth-value 0 (create-obj-list charlist)))
                 (remove-parsed-chars (nth-value 0 (create-obj-list charlist)) 
                                      (nth-value 1 (create-obj-list charlist)))))))

(defun parse-string (charlist)
  (values 
   (if (eq (car charlist) #\:) nil
     (cons (car charlist) (parse-string (cdr charlist))))
   charlist))

(defun parse-number (numberlist)
  (nth-value 0 (parse-integer (convert-to-string numberlist))))

(defun parse-atom (charlist)
  (cond ((equal charlist (list #\t #\r #\u #\e))
         'true)
        ((equal charlist (list #\f #\a #\l #\s #\e))
         'false)
        ((equal charlist (list #\n #\u #\l #\l))
         'null)))

(defun convert-to-string (charlist)
  (if(null charlist)""
    (if (eq (car charlist) #\") (convert-to-string (remove (car charlist)
charlist))
(concatenate 'string (string (car charlist)) (convert-to-string (cdr
charlist))))))

;; used to create a json object parsed with the right double quotes
(defun convert-to-json (charlist)
  (concatenate 'string charlist))

;; remove already parsed characters in list from charlist  
(defun remove-parsed-chars (list1 charlist)
  (if (not (null charlist))
      (if (eq (car list1) (car charlist)) 
          (remove-parsed-chars (cdr list1) 
                               (cdr charlist))
        charlist)
    nil))

;; creates a list of characters that will be used to parse a value
(defun create-value-list (charlist)
  (values 
   (if (or (eq (car charlist) #\,) (null charlist)) nil
     (cons (car charlist) (create-value-list (cdr charlist))))
   charlist))
   
;; creates a list of characters that represents a json array
(defun create-array-list (charlist)
  (values 
   (if (eq (car charlist) #\]) (cons #\] nil)
     (cons (car charlist) (create-array-list (cdr charlist))))
   charlist))

;; creates a list of characters whitch rapresents a json obj
(defun create-obj-list (charlist)
  (values 
   (if (eq (car charlist) #\}) (cons #\} nil)
     (cons (car charlist) (create-obj-list (cdr charlist))))
   charlist))

(defun jsonaccess (json &rest fields)
  (json-access-result json fields))
      
(defun json-access-result (json fields)
  (if (not (null fields))
      (cond ((eq (car json) 'JSONARRAY)
             (json-access-result (cdr json) fields))
            ((eq (car json) 'JSONOBJ)
             (json-access-result (cdr json) fields))
            ((numberp (car fields))
             (json-access-result (get-index json (car fields)) (cdr fields)))
            ((stringp (car (car json)))
             (json-access-result (json-access-obj json (car fields)) (cdr fields)))
            (T (error "Syntax error!")))
     json))
             
(defun json-access-obj (obj field)
  (if (string-equal (car (car obj)) field)
      (car (cdr (car obj)))
    (json-access-obj (cdr obj) field)))

(defun get-index (array index)
  (if (< index 0) (error "Incorrect index!")
    (if (= index 0) (car array)
      (get-index (cdr array) (- index 1))))) 

;; jsonread input file
(defun jsonread (filename)
  (with-open-file (in filename
		      :if-does-not-exist :error
		      :direction :input)
  (jsonparse (load-char in))))

 
;; reads the file one character at a time
(defun load-char (inputstream)
  (let ((json (read-char inputstream nil 'eof)))
    (if (eq json 'eof) ""
      (string-append json (load-char inputstream)))))

; element becomes the head element of the list l
(defun insert-element (element l)
  (if (null l)
      (list element)
    (cons (first l) (insert-element element (rest l)))))

; adds #\: #\ #\Space, inside the object
; used to reverse in lisp syntax
(defun clean-obj (List FixedList)
  (cond ((null List) FixedList)
        ((null FixedList) (clean-obj (cdr List) (fix-pairs (car List))))
        (T (clean-obj (cdr List) (append FixedList (list #\,) 
          (list #\Space) (fix-pairs (car List)))))))

; adds #\: between field and attribute
(defun fix-pairs (List)
  (append (first List) 
          (list #\Space) 
          (list #\:) 
          (list #\Space) 
          (second List)))

; adds #\, #\Space between elements
(defun clean-array (List FixedList)
  (cond ((null List) FixedList)
        ((null FixedList) (clean-array (cdr List) (first List)))
        (T (clean-array (cdr List) (append FixedList (list #\,) 
                                                   (list #\Space) 
                                                   (first List))))))

;; json-write
(defun jsondump (jsonobj filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
		  (format out (createjsonobj jsonobj)) filename))

(defun createjsonobj (JSONparsed)
  (convert-to-json (jsonreverse JSONparsed)))

(defun jsonreverse (JSONparsed)
  (cond ((null JSONparsed) nil)
        ((stringp JSONparsed) (append (list #\") 
                                      (string-to-list JSONparsed) 
                                      (list #\")))
        ((equal JSONparsed 'null) (string-to-list "null"))
        ((equal JSONparsed 'true) (string-to-list "true"))
        ((equal JSONparsed 'false) (string-to-list "false"))
        ((numberp JSONparsed) (string-to-list (write-to-string JSONparsed)))
        ((equal (car JSONparsed) 'jsonarray) 
          (append (list #\[)
                  (clean-array (mapcar 'jsonreverse (cdr JSONparsed)) nil)
                  (list #\])))
        ((equal (car JSONparsed) 'jsonobj) 
          (append (list #\{)
                  (clean-obj (mapcar 'jsonreverse (cdr JSONparsed)) nil)
                  (list #\})))
        (T (mapcar 'jsonreverse JSONparsed))))


;;;; end of file -- json-parsing.l 
