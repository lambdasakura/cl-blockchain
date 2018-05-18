(in-package :cl-user)
(defpackage block-chain-server
  (:use
   :jonathan
   :clack
   :ironclad
   :uuid
   :cl)
  (:export
   :resolve-conflict
   :get-longest-chain
   :init-block-chain
   :new-block
   :new-transaction
   :register-node
   :mine
   :*app*
   :*block-chain*
   :*nodes*
   :valid-chain
   :server-stop
   :server-start))
(in-package :block-chain-server)

(defun sha256-digest (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence (ironclad:make-digest :sha256)
                             (ironclad:ascii-string-to-byte-array string))))

(defmethod make-hash (block-elm)
  (sha256-digest (format nil "~A" (jonathan:to-json block-elm))))

(defun init-block-chain ()
  (let ((block-chain (make-hash-table :test 'equalp)))
    (setf (gethash "chain" block-chain) '())
    (setf (gethash "current-transaction" block-chain) '())
    (new-block block-chain :proof 500 :previous-hash 1)
    block-chain))

(defun last-block (block-chain)
   (first (last (gethash "chain" block-chain))))

(defun new-block (block-chain &key proof previous-hash)
  ;; Add new block to Block Chain
  (let ((next-block (alexandria:plist-hash-table
                     (list "index" (+ 1 (length (gethash "chain" block-chain)))
                           "transactions" (copy-list (gethash "current-transaction" block-chain))
                           "previous-hash" (or previous-hash (make-hash (last-block block-chain)))
                           "timestamp" (local-time:format-timestring nil (local-time:now))
                           "proof" proof)
                     :test 'equalp
                     )))
    (setf (gethash "current-transaction" block-chain) '())
    (setf (gethash "chain" block-chain) (append (gethash "chain" block-chain) (list next-block)))
    next-block
    ))

(defun new-transaction (block-chain &key sender recipient amount)
  ;; Add new transaction to Block Chain
  (let ((next-transaction (alexandria:plist-hash-table
                           (list "sender" sender
                                 "recipient" recipient
                                 "amount" amount)
                           :test 'equalp
                           )))
    (setf (gethash "current-transaction" block-chain)
          (append (gethash "current-transaction" block-chain) (list next-transaction)))
    (+ 1 (gethash "index" (last-block block-chain)))))

(defun valid-chain (block-chain)
  (let ((chain (gethash "chain" block-chain)))
    (loop 
      for i from 1 to (- (length chain) 1)
      for last-block = (elt chain 0) then (elt chain (- i 1))
      for current-block = (elt chain 1) then (elt chain i)
    always (valid-proof (gethash "proof" last-block) (gethash "proof" current-block))
      always (equal (gethash "previous-hash" current-block) (make-hash last-block)))))

(defun get-neighbour-chain (node current-length)
  (multiple-value-bind (body status rest)
      (dex:get (format nil "http://~A/chain" node))
    (declare (ignore status rest))
    (let ((node-block-chain (jonathan:parse body :as :hash-table)))
      (if (and (> (gethash "length" node-block-chain) current-length)
               (valid-chain (gethash "chain" node-block-chain)))
          node-block-chain
          nil))))

(defun get-longest-chain (chains)
  (gethash "chain" (first (sort chains #'> :key (lambda (x) (gethash "length" x))))))

(defun resolve-conflict (block-chain nodes)
  (let* ((chains (loop for node in nodes
                       collect (get-neighbour-chain node (length (gethash "chain" block-chain))))))
    (if chains (get-longest-chain chains))))

(defun valid-proof (last_proof proof)
  (let ((digest (sha256-digest (format nil "~A~A" last_proof proof))))
    (string= "0000" (subseq digest 0 4))))

(defun proof-of-work (last_proof)
  (loop for proof by 1
        if (equal t (valid-proof last_proof proof)) return proof))

(defparameter *nodes* nil)
(defun register-node (address)
  (setf *nodes* (adjoin address *nodes* :test #'string= )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REST API Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *block-chain* (init-block-chain))
(defparameter *node-identifier*  (princ-to-string (uuid:make-v4-uuid)))

;; サーバ用の変数
(defparameter *app* (make-instance 'ningle:<app>))
(defparameter *handler* nil)

;; サーバの起動
(defun server-start (&key (port 5000))
  (setf *handler*
        (clack:clackup *app*
                       :server :woo
                       :use-default-middlewares nil
                       :port port)))

;; サーバの停止
(defun server-stop () (clack:stop *handler*))

;; *app*のルーティングテーブルに関数を登録するマクロ
(defmacro defroute (name (params &rest route-args) &body body)
  `(setf (ningle:route *app* ,name ,@route-args)
         (lambda (,params)
           (declare (ignorable ,params))
           ,@body)))

(defun asc (key alist)
  (cdr (assoc key alist :test #'string=)))

(defmacro with-protect-to-json (&body body)
  `(handler-case
       `(200 (:content-type "application/json")
             (,(jonathan:to-json (progn ,@body))))
     (error (e)
       `(500 (:content-type "application/json")
             (,(jonathan:to-json (list :|error| (format nil "~A" e))))))))

(defroute "/transactions/new" (params :method :post)
  (with-protect-to-json
    (let ((sender (asc "sender" params))
          (recipient (asc "recipient" params))
          (amount (asc "amount" params)))
      (new-transaction *block-chain* :sender sender :recipient recipient :amount amount)
      (list :|message| (format nil "transaction add block(index: ~A)"
                               (gethash "index" (last-block *block-chain*)))))))

(defun mine (block-chain node-identifier)
  (let* ((last-block (last-block block-chain))
         (last-proof (gethash "proof" last-block))
         (proof (proof-of-work last-proof)))
  (new-transaction block-chain :amount 1 :recipient node-identifier :sender "0")
  (let ((new-block (new-block *block-chain* :proof proof)))
    (list :|message| (format nil "mine new block" )
          :|index| (gethash "index" new-block)
          :|transactions| (gethash "transactions" new-block)
          :|proof| (gethash "proof" new-block)
          :|Previous-hash| (gethash "previous-hash" new-block)))))

(defroute "/mine" (params)
  (with-protect-to-json
    (mine *block-chain* *node-identifier*)))

(defroute "/chain" (params)
  (with-protect-to-json
    (list :|chain| *block-chain*
          :|length| (length (gethash "chain" *block-chain*)))))

(defroute "/nodes/register" (params :method :post)
  (with-protect-to-json
    (let ((nodes (asc "nodes" params)))
      (loop for node in nodes
            do (register-node node))
      (list :|message| (format nil "new node was joined.")
            :|total_nodes| *nodes*))))

(defroute "/nodes/resolve" (params)
  (with-protect-to-json
    (let ((result (resolve-conflict *block-chain* *nodes*)))
      (cond (result
             (setf *block-chain* result)
             (list :|message| "chain updated."
                   :|chain| (gethash "chain" *block-chain*)))
            (t
             (list :|message| "chain confirmed."
                   :|chain| (gethash "chain" *block-chain*)))))))
