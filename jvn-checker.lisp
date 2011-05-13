(require :cxml)
(require :local-time)

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
  (let ((title (extract-element-from-item item "title"))
        (relation (extract-element-from-item item "dc:relation"))
        (date (extract-element-from-item item "dc:date"))
        (id (extract-element-from-item item "dc:identifier")))
    (format nil "~{~a~%~a~%~a~%~a~%~}" (list title relation (jvn-format-datetime date) id))))

(defun jvn-parse-datetime (time)
   (local-time:parse-timestring time))

(defun jvn-format-datetime (datetime)
  (local-time:format-timestring
   nil
   (jvn-parse-datetime datetime)
   :format '(:year "/" :month "/" :day " " :hour ":" (:min 2)":" (:sec 2))))

;;  if the item is old ,return nil
(defun jvn-new-infop (item)
  (let* ((date-str (extract-element-from-item item "dcterms:issued"))
         (date (jvn-parse-datetime date-str)))
    (>  (local-time:timestamp-to-universal date)
        (- (get-universal-time) (* *jvn-interval* 24 60 60)))))

(defun main ()
  (get-xml *cache-file*)
  (let ((items (extract-item-nodes *cache-file*)))
    (setf items (remove-if-not #'jvn-new-infop items))
    (dolist (data (map 'list #'format-item items))
      (format t "~a~%" data))))

(sb-ext:save-lisp-and-die "jvn-bin" :executable t :toplevel 'main )


