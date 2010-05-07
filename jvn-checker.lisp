(require :cxml)
(require :cl-ppcre)

(defvar *jvn-interval* 20)
(defvar *cache-file* ".jvn.cache")
(defvar *jvn-feed* "http://jvn.jp/rss/jvn.rdf")

(defun get-xml (file)
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede)
      (sb-ext:run-program "/usr/bin/curl" (list *jvn-feed*) :output out :error nil)))

(defun extract-item-nodes (file)
  (let* ((dom-builder (cxml:parse-file file (cxml-dom:make-dom-builder)))
         (childlen (dom:child-nodes (dom:document-element dom-builder))))
    (remove-if-not #'(lambda (element)
                       (string= (dom:node-name element) "item")) childlen)))

(defun extract-element-from-item (item element-name)
  (let* ((item-children (dom:child-nodes item))
        (element (find element-name item-children :key #'dom:node-name :test #'equal)))
    (dom:node-value (dom:last-child element))))

(defun format-item (item)
  (let ((data 
         (map 'list
              #'(lambda (element) (extract-element-from-item item element))
              (list "title" "dc:relation" "dc:date" "dc:identifier"))))
    (format nil "~{~a~%~a~%~a~%~a~%~}" data)))

(defun jvn-parse-time (time)
  (cl-ppcre:register-groups-bind (year month date hour minute second)
      ("(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})\\+.*" time)
    (eval (cons 'encode-universal-time 
                (map 'list #'parse-integer (list second minute hour date month year ))))))

;;  if the item is old ,return nil
(defun jvn-new-infop (item)
  (let* ((date-str (extract-element-from-item item "dcterms:issued"))
         (date (jvn-parse-time date-str)))
    (>  date (- (get-universal-time) (* *jvn-interval* 24 60 60)))))

(defun main ()
  (get-xml *cache-file*)
  (let ((items (extract-item-nodes *cache-file*)))
    (setf items (remove-if-not #'jvn-new-infop items))
    (dolist (data (map 'list #'format-item items))
      (format t "~a~%" data))))

(sb-ext:save-lisp-and-die "jvn-bin" :executable t :toplevel 'main )