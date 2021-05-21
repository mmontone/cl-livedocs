;; An info document generated dynamically from Common Lisp packages exported definitions.

;; TODO: parse docstrings and format Texinfo rich text. For example, transform symbols in upper case to links to the object, if present in the package.

(require :def-properties (asdf:system-relative-pathname :cl-livedocs "def-properties.lisp"))

(in-package #:cl-livedocs)

(defclass lisp-info-document (webinfo:info-document)
  ((nodes :initarg :nodes
          :accessor nodes
          :initform nil))
  (:documentation "An info document generated dynamically from Common Lisp packages exported definitions."))

(defmethod webinfo::find-node ((doc lisp-info-document) node-name)
  (labels ((childs (doc-or-node)
             (if (typep doc-or-node 'webinfo::info-document)
                 (nodes doc-or-node)
                 (webinfo::children doc-or-node)))
           (tree-find (node node-name)
             (or (find node-name (childs node) :key 'webinfo::node-name :test 'string=)
                 (loop for child in (childs node)
                       for found-node := (tree-find child node-name)
                       while (not found-node)
                       finally (return found-node)))))
    (tree-find doc node-name)))

(defun collect-package-info (&optional (package *package*))
  (let (infos)
    (do-external-symbols (symbol package)
      (push
       (def-properties:symbol-properties symbol t)
       infos))
    (apply #'append infos)))

(defmethod webinfo::all-nodes ((doc lisp-info-document))
  (labels ((descendants (node)
             (append (webinfo::children node)
                     (apply #'append (mapcar #'descendants (webinfo::children node))))))
    (loop for node in (nodes doc)
          appending (descendants node))))

;; Info nodes

(defun make-info-document-for-package (package)
  (let ((doc (make-instance 'lisp-info-document
                            :name (hunchentoot:url-encode (package-name package))
                            :title (package-name package)))
        (top-node (make-instance 'webinfo::sexp-info-node
                                 :name "Top"
                                 :title (package-name package)))
        (variables-node (make-instance 'webinfo::sexp-info-node
                                       :name "Variables"
                                       :title "Variables"
                                       :description (format nil "Dictionary of all external variables in ~a package" package)))
        (macros-node (make-instance 'webinfo::sexp-info-node
                                    :name "Macros"
                                    :title "Macros"
                                    :description (format nil "Dictionary of all external macros in ~a package" package)))
        (functions-node (make-instance 'webinfo::sexp-info-node
                                       :name "Functions"
                                       :title "Functions"
                                       :description (format nil "Dictionary of all external functions in ~a package" package)))
        (classes-node (make-instance 'webinfo::sexp-info-node
                                     :name "Classes"
                                     :title "Classes"
                                     :description (format nil "Dictionary of all external classes in ~a package" package)))

        (index-node (make-instance 'webinfo::sexp-info-node
                                   :name "Index"
                                   :title "Index"
                                   :description "Definitions index"))

        (package-info (collect-package-info package)))

    ;; Build top node
    (flet ((menu-entry (node)
             `(:|menuentry| ()
                (:|menunode| ()
                  ,(webinfo::node-name node))
                (:|menudescription| ()
                  (:|pre| ,(webinfo::description node))))))

      (setf (webinfo::contents top-node)
            `(:|chapter| ()
               (:|sectiontitle| ()
                 ,(format nil "Package reference: ~a" (package-name package)))
               ,@(awhen (documentation package t)
                   (webinfo::text->sexp it))
               (:|menu| ()
                 ,@(loop for node in (list variables-node
                                           functions-node
                                           classes-node
                                           index-node)
                         collect (menu-entry node))))))
    (push top-node (nodes doc))

    ;; Build definition nodes

    (setf (webinfo::node-up variables-node) "Top")
    (setf (webinfo::contents variables-node)
          `(:|chapter| ()
             (:|sectiontitle| ()
               "Variables")
             ,@(loop for info in (remove-if-not
                                  (lambda (info)
                                    (eql (aget info :type) :variable))
                                  package-info)
                     collect (lispinfo->sexp info))))

    (push variables-node (webinfo::children top-node))

    (setf (webinfo::node-up macros-node) "Top")
    (setf (webinfo::contents macros-node)
          `(:|chapter| ()
             (:|sectiontitle| ()
               "Macros")
             ,@(loop for info in (remove-if-not
                                  (lambda (info)
                                    (eql (aget info :type) :macro))
                                  package-info)
                     collect (lispinfo->sexp info))))

    (push macros-node (webinfo::children top-node))

    (setf (webinfo::node-up functions-node) "Top")
    (setf (webinfo::contents functions-node)
          `(:|chapter| ()
             (:|sectiontitle| ()
               "Functions")
             ,@(loop for info in (remove-if-not
                                  (lambda (info)
                                    (member (aget info :type) '(:function :generic-function)))
                                  package-info)
                     collect (lispinfo->sexp info))))

    (push functions-node (webinfo::children top-node))

    (setf (webinfo::node-up classes-node) "Top")
    (setf (webinfo::contents classes-node)
          `(:|chapter| ()
             (:|sectiontitle| ()
               "Classes")
             ,@(loop for info in (remove-if-not
                                  (lambda (info)
                                    (member (aget info :type) '(:class)))
                                  package-info)
                     collect (lispinfo->sexp info))))
    (push classes-node (webinfo::children top-node))

    ;; Build index node
    (setf (webinfo::node-up index-node) "Top")
    (setf (webinfo::contents index-node)
          `(:|chapter| ()
             (:|sectiontitle| () "Index")
             (:|printindex| (:|value| "vr"))
             (:|printindex| (:|value| "fn"))
             (:|printindex| (:|value| "tp"))
             (:|printindex| (:|value| "cp"))))

    (push index-node (webinfo::children top-node))

    (setf (webinfo::children top-node) (nreverse (webinfo::children top-node)))

    (setf (webinfo::node-next variables-node) "Macros")
    (setf (webinfo::node-next functions-node) "Functions")
    (setf (webinfo::node-next functions-node) "Classes")
    (setf (webinfo::node-next classes-node) "Index")

    (initialize-lisp-document-indexes doc package-info)

    doc))


(defun initialize-lisp-document-indexes (doc package-info)
  (setf (webinfo::indexes doc)
        (list (cons :vr (loop for info in package-info
                              when (member (aget info :type) '(:variable))
                                collect (cons (princ-to-string (aget info :name))
                                              (webinfo::find-node doc "Variables"))))
              (cons :fn
                    (append
                     (loop for info in package-info
                           when (member (aget info :type) '(:macro))
                             collect (cons (princ-to-string (aget info :name))
                                           (webinfo::find-node doc "Macros")))
                     (loop for info in package-info
                           when (member (aget info :type) '(:function :generic-function))
                             collect (cons (princ-to-string (aget info :name))
                                           (webinfo::find-node doc "Functions")))
                     ))
              (cons :tp (loop for info in package-info
                              when (member (aget info :type) '(:class))
                                collect (cons (princ-to-string (aget info :name))
                                              (webinfo::find-node doc "Classes")))))))

(defun lispinfo->sexp (info)
  (ecase (aget info :type)
    (:variable
     `(:|defvr| ()
        (:|definitionterm| ()
          (:|indexterm| (:index "vr")
            ,(aget info :name))
          (:|defcategory| () "Variable")
          (:|defvariable| () ,(aget info :name)))
        (:|definitionitem| ()
          ,@(or (aand (aget info :documentation)
                      (webinfo::text->sexp it))
                `((:|para| () ""))))
        (:|vindex| ()
          (:|indexterm| ()
            ,(princ-to-string (aget info :name))))))
    (:function
     `(:|deffn| ()
        (:|definitionterm| ()
          (:|indexterm| (:index "fn")
            ,(aget info :name))
          (:|defcategory| () "Function")
          (:|deffunction| () ,(aget info :name))
          (:|defparam| () ,(aget info :args)))
        (:|definitionitem| ()
          ,@(or (aand (aget info :documentation)
                      (webinfo::text->sexp it))
                `((:|para| () ""))))
        (:|findex| ()
          (:|indexterm| ()
            ,(princ-to-string (aget info :name))))))
    (:generic-function
     `(:|deffn| ()
        (:|definitionterm| ()
          (:|indexterm| (:index "fn")
            ,(aget info :name))
          (:|defcategory| () "Generic function")
          (:|deffunction| () ,(aget info :name))
          (:|defparam| () ,(aget info :args)))
        (:|definitionitem| ()
          ,@(or (aand (aget info :documentation)
                      (webinfo::text->sexp it))
                `((:|para| () ""))))
        (:|findex| ()
          (:|indexterm| ()
            ,(princ-to-string (aget info :name))))))
    (:class
     `(:|deftp| ()
        (:|definitionterm| ()
          (:|indexterm| (:index "tp")
            ,(aget info :name))
          (:|defcategory| () "Class")
          (:|defdatatype| () ,(aget info :name)))
        (:|definitionitem| ()
          ,@(or (aand (aget info :documentation)
                      (webinfo::text->sexp it))
                `((:|para| () ""))))
        (:|tindex| ()
          (:|indexterm| ()
            ,(princ-to-string (aget info :name))))))
    (:macro
     `(:|deffn| ()
        (:|definitionterm| ()
          (:|indexterm| (:index "fn")
            ,(aget info :name))
          (:|defcategory| () "Macro")
          (:|deffunction| () ,(aget info :name))
          (:|defparam| () ,(aget info :args)))
        (:|definitionitem| ()
          ,@(or (aand (aget info :documentation)
                      (webinfo::text->sexp it))
                `((:|para| () ""))))
        (:|findex| ()
          (:|indexterm| ()
            ,(princ-to-string (aget info :name))))))))

(defclass livedocs-info-repository (webinfo::dir-info-repository webinfo::indexable-info-repository)
  ())

(defmethod initialize-instance ((repo livedocs-info-repository) &rest initargs)
  (declare (ignore initargs))
  (setf (webinfo::dir repo)
        (mapcar 'make-info-document-for-package
                (remove "COMMON-LISP" (sort (list-all-packages) 'string< :key 'package-name)
                        :test 'equalp
                        :key 'package-name)))
  (call-next-method))

(defun start (&rest args &key fulltext-search &allow-other-keys)
  (let ((acceptor
          (apply #'webinfo:start-webinfo
                 :info-repository
                 (make-instance 'livedocs-info-repository
                                :search-index (when fulltext-search
                                                (webinfo::make-memory-search-index)))
                 :app-settings (list (cons :theme (make-instance 'webinfo::nav-theme)))
                 (let ((acceptor-args (copy-list args)))
                   (remf acceptor-args :fulltext-search)
                   acceptor-args))))
    (format t "------------------------------------------------------------------
Common Lisp live documentation is here: http://localhost:~a~%
------------------------------------------------------------------~%"
            (hunchentoot:acceptor-port acceptor))))
