(require :cxml)
(require :local-time)
(require :trivial-shell)

(defvar *jvn-interval* 20)

(defun extract-item-nodes (xml)
  (let* ((dom-builder (cxml:parse xml (cxml-dom:make-dom-builder)))
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
    (format nil "~{~a~%~a~%~a~%~a~%~}" (list id title relation (jvn-format-datetime date)))))

(defun jvn-parse-datetime (time)
   (local-time:parse-timestring time))

(defun jvn-format-datetime (datetime)
  (local-time:format-timestring
   nil
   (jvn-parse-datetime datetime)
   :format '("[" :year "/" :month "/" :day " " :hour ":" (:min 2)":" (:sec 2) "]")))

;;  if the item is old ,return nil
(defun jvn-new-infop (item)
  (let* ((date-str (extract-element-from-item item "dcterms:issued"))
         (date (jvn-parse-datetime date-str)))
    (>  (local-time:timestamp-to-universal date)
        (- (get-universal-time) (* *jvn-interval* 24 60 60)))))

(defun main ()
  (get-xml *cache-file*)
  (let ((items (extract-item-nodes
                (trivial-shell:shell-command "/usr/bin/curl http://jvn.jp/rss/jvn.rdf"))))
    (setf items (remove-if-not #'jvn-new-infop items))
    (dolist (data (map 'list #'format-item items))
      (format t "~a~%" data))))

;; (sb-ext:save-lisp-and-die "jvn-bin" :executable t :toplevel 'main )


