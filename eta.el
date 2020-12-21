;; ETUDE LANG
;;
;; This package defines typical code convensions that
;; will be used. This follows governing clojure principles
;; in terms of readability and code maintainance.
;;
(require 'eta)

;; Modern libraries
(require 's)     ;; string
(require 'dash)  ;; list
(require 'ht)    ;; maps

(setq eta/*lock* nil)
(setq eta/*commands*       (ht-create))
(setq eta/*mode-bindings*  (ht-create))
(setq eta/*mode-functions* (ht-create))
(setq eta/*mode-lookup*    (ht-create))

;; Macro Definitions
(defun eta/let:fn (bindings &rest body)
  (let ((bargs (seq-reverse (seq-partition bindings 2))))
    (seq-reduce (lambda (body barg)
                  (list '-let barg body))
                bargs
                (cons 'progn body))))

(defmacro eta/let (bindings &rest body)
  (declare (indent 1))
  (apply 'eta/let:fn bindings body))

(defun eta/put-command (key fn)
  (if (and eta/*lock*
           (ht-get eta/*commands* key))
      (error (s-concat "key " (symbol-name key) " already exists"))
    (ht-set eta/*commands* key fn)))

(defun eta/get-command (key)
  (gethash key eta/*commands*))

;;
;; eta/bind
;;

(defun eta/bind:fn (declaration &rest specs)
  (eta/let [bind-map (if (seq-empty-p declaration)
                      nil
                    (seq-elt declaration 0))
         body (seq-mapcat (lambda (spec)
                            (eta/let [(key bindings fn) spec]
                              (if fn
                                  (progn (eta/put-command key (cadr fn))
                                         (seq-map (lambda (binding)
                                                    `(progn ,(if bind-map
                                                                 `(bind-key ,binding ,fn ,bind-map)
                                                               `(bind-key* ,binding ,fn))
                                                            (vector ,key ,binding ,fn)))
                                                  bindings)))))
                          (seq-partition specs 3))]
    (cons 'list body)))

(defmacro eta/bind (declaration &rest specs)
  (declare (indent 1))
  (apply 'eta/bind:fn declaration specs))

;;
;; setup for eta/mode
;;
(defun eta/mode-key ()
  (ht-get eta/*mode-lookup* major-mode))


(defun eta/mode-dispatch (fn-key &rest args)
  (eta/let [mode-key (ht-get eta/*mode-lookup* major-mode)
         fn-table (if mode-key
                      (ht-get eta/*mode-functions* mode-key))
         fn       (if fn-table
                      (ht-get fn-table fn-key))]
    (if fn
        (apply 'funcall fn args)
      (error (s-concat "Function unavailable ("
                       (symbol-name mode-key)
                       " "
                       (symbol-name fn-key) ")")))))

;;
;; eta/mode-init
;;

(defun eta/mode-init:create-mode-fn (fn-key bindings params)
  (eta/let [fn-name (intern (s-concat "eta/mode-fn"
                                   (symbol-name fn-key)))
         args    (seq-map 'intern params)]
    (ht-set eta/*mode-bindings* fn-key bindings)
    `(progn (defun ,fn-name (,@args)
              (interactive ,@params)
              (eta/mode-dispatch ,fn-key ,@args))
            (eta/bind nil ,fn-key ,bindings (quote ,fn-name)))))

(defun eta/mode-init:form (declaration &rest specs)
  (eta/let [body (seq-map (lambda (args)
                         (apply 'eta/mode-init:create-mode-fn args))
                       (seq-partition specs 3))]
    (cons 'progn body)))

(defmacro eta/mode-init (declaration &rest specs)
  (declare (indent 1))
  (apply 'eta/mode-init:form declaration specs))

;;
;; eta/mode
;;

(defun eta/mode:create-config-fn (mode-key mode-name mode-config)
  (eta/let [fn-name   (intern (s-concat "eta/mode-config" (symbol-name mode-key)))
         mode-map  (intern (s-concat (symbol-name mode-name) "-map"))]
    `(progn
       (defun ,fn-name ()
         (interactive)
         (eta/jump-to-config ,mode-config))
       (bind-key eta/*meta-config* (quote ,fn-name) ,mode-map))))

(defun eta/mode:form (declaration &rest specs)
  (eta/let [[mode-key mode-name &rest more] declaration
         mode-file-name (if (not (seq-empty-p more)) (seq-elt more 0))
         mode-table (ht-create)
         _    (ht-set eta/*mode-functions* mode-key mode-table)
         _    (ht-set eta/*mode-lookup* mode-name mode-key)
         conf-body (eta/mode:create-config-fn mode-key mode-name (or mode-file-name load-file-name))
         body      (seq-map (lambda (spec)
                              (eta/let [(fn-key fn) spec
                                     mode-fn-key (intern (s-concat (symbol-name fn-key) (symbol-name mode-key)))]
                                (ht-set mode-table fn-key (cadr fn))))
                            (seq-partition specs 2))]
    conf-body))

(defmacro eta/mode (declaration config &rest specs)
  (declare (indent 1))
  (apply 'eta/mode:form declaration config specs))

(provide 'eta)
