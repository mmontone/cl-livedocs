;; An info document generated dynamically from Common Lisp packages exported definitions.

;; TODO: parse docstrings and format Texinfo rich text. For example, transform symbols in upper case to links to the object, if present in the package.


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
  (let (docs)
    (do-external-symbols (symbol package)
      (when (fboundp symbol)
        (push (load-function-info symbol) docs))
      (when (boundp symbol)
        (push (load-variable-info symbol) docs))
      (when (safe-class-for-symbol symbol)
        (push (load-class-info symbol) docs)))
    docs))

(defmethod webinfo::all-nodes ((doc lisp-info-document))
  (labels ((descendants (node)
             (append (webinfo::children node)
                     (apply #'append (mapcar #'descendants (webinfo::children node))))))
    (loop for node in (nodes doc)
          appending (descendants node))))

;; From docbrowser

(defun nice-princ-to-string (obj)
  (typecase obj
    (string obj)
    (keyword (prin1-to-string obj))
    (t (princ-to-string obj))))

#+sbcl(defmethod documentation ((slotd sb-pcl::condition-effective-slot-definition) (doc-type (eql 't)))
        "This method definition is missing in SBCL as of 1.0.55 at least. Adding it here
will make documentation for slots in conditions work properly."
        (slot-value slotd 'sb-pcl::%documentation))

(defun assoc-cdr (key data &key error-p)
  "Return (CDR (ASSOC KEY DATA)). If ERROR-P is non-NIL, signal an error if KEY is
not available is DATA."
  (let ((v (assoc key data)))
    (when (and error-p
               (not v))
      (error "~s not found in data" key))
    (cdr v)))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "Return non-NIL if SYMBOL is external in PACKAGE. SYMBOL may be either
a symbol, or a SETF form, in which case the check will be performed on
the CADR of the list."
  (eq (nth-value 1 (find-symbol (symbol-name (cond ((symbolp symbol)
                                                    symbol)
                                                   ((eq (car symbol) 'setf)
                                                    (cadr symbol))
                                                   (t
                                                    (error "Unknown symbol type: ~s" symbol))))
                                package))
      :external))

(defun prin1-to-string-with-package (obj package)
  (let ((*package* package))
    (prin1-to-string obj)))

(defun format-argument-to-string (arg)
  (etypecase arg
    (symbol (nice-princ-to-string arg))
    (list   (mapcar #'(lambda (entry conversion) (funcall conversion entry))
                    arg (list #'(lambda (v)
                                  (if (listp v)
                                      (nice-princ-to-string (car v))
                                      (nice-princ-to-string v)))
                              #'prin1-to-string
                              #'nice-princ-to-string)))))

(defun load-function-info (symbol)
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'function))
        (cons :args (let ((*print-case* :downcase)
                          (*package* (symbol-package symbol)))
                      #+nil(format nil "~{~a~^ ~}"
                                   (mapcar #'format-argument-to-string (swank-backend:arglist symbol))
                                   )
                      (princ-to-string (swank-backend:arglist symbol))))
        (cons :type (cond ((macro-function symbol) :macro)
                          ((typep (symbol-function symbol) 'generic-function) :generic-function)
                          (t :function)))))

(defun load-variable-info (symbol)
  (list (cons :name (string symbol))
        (cons :documentation (documentation symbol 'variable))
        (cons :boundp (boundp symbol))
        (cons :value (when (boundp symbol) (prin1-to-string (symbol-value symbol))))
        (cons :constant-p (constantp symbol))
        (cons :type :variable)))

(defun find-superclasses (class)
  (labels ((f (classes found)
             (if (and classes
                      (not (eq (car classes) (find-class 'standard-object)))
                      (not (member (car classes) found)))
                 (f (cdr classes)
                    (f (closer-mop:class-direct-superclasses (car classes))
                       (cons (car classes) found)))
                 found)))
    (f (list class) nil)))

(defun safe-class-for-symbol (symbol)
  (handler-case
      (find-class symbol)
    (error nil)))

(defun assoc-name (v)
  (assoc-cdr :name v :error-p t))

(defun specialise->symbol (spec)
  (case (caar spec)
    ((defmethod) (cadar spec))
    #+ccl((ccl::reader-method) (cadr (assoc :method (cdar spec))))
    (t nil)))

(defun load-specialisation-info (class-name)
  (let* ((ignored '(initialize-instance))
         (class (if (symbolp class-name) (find-class class-name) class-name))
         (spec (swank-backend:who-specializes class)))
    (unless (eq spec :not-implemented)
      (sort (loop
              for v in spec
              for symbol = (specialise->symbol v)
              when (and (not (member symbol ignored))
                        (symbol-external-p symbol (symbol-package (class-name class))))
                collect (list (cons :name symbol)))
            #'string< :key (alexandria:compose #'princ-to-string #'assoc-name)))))

(defun %ensure-external (symbol)
  (let ((name (cond ((symbolp symbol)
                     symbol)
                    ((and (listp symbol) (eq (car symbol) 'setf))
                     (cadr symbol))
                    (t
                     (warn "Unknown type: ~s. Expected symbol or SETF form." symbol)
                     nil))))
    (when (symbol-external-p name)
      symbol)))

(defun load-accessor-info (class slot)
  (flet ((getmethod (readerp method-list)
           (dolist (method method-list)
             (let ((name (closer-mop:generic-function-name (closer-mop:method-generic-function method))))
               (when (and (eq (type-of method) (if readerp
                                                   'closer-mop:standard-reader-method
                                                   'closer-mop:standard-writer-method))
                          (eq (closer-mop:slot-definition-name (closer-mop:accessor-method-slot-definition method))
                              (closer-mop:slot-definition-name slot)))
                 (return-from getmethod name))))))

    ;; There are several different situations we want to detect:
    ;;   1) Only a reader method: "reader FOO"
    ;;   2) Only a writer method: "writer FOO"
    ;;   3) Only a writer SETF method: "writer (SETF FOO)"
    ;;   4) A reader and a SETF method: "accessor FOO"
    ;;   5) A reader and non-SETF writer: "reader FOO, writer FOO"
    ;;
    ;; The return value from this function is an alist of the following form:
    ;;
    ;;  ((:READER . FOO-READER) (:WRITER . FOO-WRITER) (:ACCESSOR . FOO-ACCESSOR))
    ;;
    ;; Note that if :ACCESSOR is given, then it's guaranteed that neither
    ;; :READER nor :WRITER will be included.
    ;;
    ;; We start by assigning the reader and writer methods to variables
    (let* ((method-list (closer-mop:specializer-direct-methods class))
           (reader (%ensure-external (getmethod t method-list)))
           (writer (%ensure-external (getmethod nil method-list))))
      ;; Now, detect the 5 different cases, but we coalease case 2 and 3.
      (cond ((and reader (null writer))
             `((:reader . ,reader)))
            ((and (null reader) writer)
             `((:writer . ,writer)))
            ((and reader (listp writer) (eq (car writer) 'setf) (eq (cadr writer) reader))
             `((:accessor . ,reader)))
            ((and reader writer)
             `((:reader . ,reader) (:writer . ,writer)))))))

(defun load-slots (class)
  (closer-mop:ensure-finalized class)
  (flet ((load-slot (slot)
           (list (cons :name (string (closer-mop:slot-definition-name slot)))
                 (cons :documentation (swank-mop:slot-definition-documentation slot))
                 ;; The LIST call below is because the accessor lookup is wrapped
                 ;; in a FOR statement in the template.
                 (cons :accessors (let ((accessor-list (load-accessor-info class slot)))
                                    (when accessor-list
                                      (list accessor-list)))))))
    (mapcar #'load-slot (closer-mop:class-slots class))))

(defun load-class-info (class-name)
  (let ((cl (find-class class-name)))
    (list (cons :name          (class-name cl))
          (cons :documentation (documentation cl 'type))
          (cons :slots         (load-slots cl))
          ;; (cons :methods       (load-specialisation-info cl)) TODO: fix

          (cons :type :class))))

(defun %annotate-function-info (fn-info classes)
  "Append :ACCESSORP tag if the function is present as an accessor function."
  (loop
    with name = (cdr (assoc :name fn-info))
    for class-info in classes
    do (loop
         for slot-info in (cdr (assoc :slots class-info))
         do (loop
              for accessor in (cdr (assoc :accessors slot-info))
              for accessor-sym = (cdar accessor)
              when (or (and (symbolp accessor-sym) (eq accessor-sym name))
                       (and (listp accessor-sym) (eq (car accessor-sym) 'setf) (eq (cadr accessor-sym) name)))
                do (return-from %annotate-function-info (append fn-info '((:accessorp t))))))
    finally (return fn-info)))

;; Info nodes

(defun make-info-document-for-package (package)
  (let ((doc (make-instance 'lisp-info-document
                            :name (hunchentoot:url-encode (package-name package))
                            :title (package-name package)))
        (top-node (make-instance 'webinfo::sexp-info-node
                                 :name "Top"
                                 :title "Top"))
        (dictionary-node (make-instance 'webinfo::sexp-info-node
                                        :name "Dictionary"
                                        :title "Dictionary"
                                        :description (format nil "Dictionary of all external definitions in ~a package" package)))
        (variable-index-node (make-instance 'webinfo::sexp-info-node
                                            :name "Variable index"
                                            :title "Variable index"
                                            :description "Variables index"))
        (function-index-node (make-instance 'webinfo::sexp-info-node
                                            :name "Function index"
                                            :title "Function index"
                                            :description "Functions index"))
        (class-index-node (make-instance 'webinfo::sexp-info-node
                                         :name "Class index"
                                         :title "Class index"
                                         :description "Classes index"))
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
                 ,@(loop for node in (list dictionary-node
                                           variable-index-node
                                           function-index-node
                                           class-index-node)
                         collect (menu-entry node))))))
    (push top-node (nodes doc))

    ;; Build dictionary node
    (setf (webinfo::node-up dictionary-node) "Top")
    (setf (webinfo::contents dictionary-node)
          `(:|chapter| ()
             (:|sectiontitle| ()
               "Dictionary")
             ,@(loop for info in package-info
                     collect (lispinfo->sexp info))))


    (push dictionary-node (webinfo::children top-node))

    ;; Build index nodes
    (setf (webinfo::node-up variable-index-node) "Top")
    (setf (webinfo::contents variable-index-node)
          `(:|chapter| ()
             (:|sectiontitle| () "Variable index")
             (:|printindex| (:|value| "vr"))))
    (push variable-index-node (webinfo::children top-node))

    (setf (webinfo::node-up function-index-node) "Top")
    (setf (webinfo::contents function-index-node)
          `(:|chapter| ()
             (:|sectiontitle| () "Function index")
             (:|printindex| (:|value| "fn"))))

    (setf (webinfo::node-up function-index-node) "Top")
    (push function-index-node (webinfo::children top-node))

    (setf (webinfo::node-up class-index-node) "Top")
    (setf (webinfo::contents class-index-node)
          `(:|chapter| ()
             (:|sectiontitle| () "Class index")
             (:|printindex| (:|value| "tp"))))

    (setf (webinfo::node-up class-index-node) "Top")
    (push class-index-node (webinfo::children top-node))

    (setf (webinfo::node-next dictionary-node) "Variable index")

    (setf (webinfo::node-prev variable-index-node) "Dictionary")
    (setf (webinfo::node-next variable-index-node) "Function index")

    (setf (webinfo::node-prev function-index-node) "Variable index")
    (setf (webinfo::node-next function-index-node) "Class index")

    (setf (webinfo::node-prev class-index-node) "Function index")

    (initialize-lisp-document-indexes doc package-info)

    doc))


(defun initialize-lisp-document-indexes (doc package-info)
  (let ((dictionary-node (webinfo::find-node doc "Dictionary")))
    (setf (webinfo::indexes doc)
          (list (cons :fn (loop for info in package-info
                                when (member (aget info :type) '(:function))
                                  collect (cons (princ-to-string (aget info :name))
                                                dictionary-node)))
                (cons :vr (loop for info in package-info
                                when (member (aget info :type) '(:variable))
                                  collect (cons (princ-to-string (aget info :name))
                                                dictionary-node)))
                (cons :tp (loop for info in package-info
                                when (member (aget info :type) '(:class))
                                  collect (cons (princ-to-string (aget info :name))
                                                dictionary-node)))))))

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

(defun start (&rest args &key (fulltext-search t) &allow-other-keys)
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
