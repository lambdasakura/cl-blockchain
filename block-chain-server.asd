(in-package :cl-user)
(defpackage block-chain-server-asd
  (:use :cl :asdf))
(in-package :block-chain-server-asd)

(defsystem block-chain-server
  :version "0.1"
  :author "lambda_sakura"
  :license "MIT"
  :depends-on (:ironclad
               :local-time
               :ningle
               :clack
               :uuid
               :dexador
               :alexandria
               :jonathan)
  :components ((:module "src"
                :serial t
                :components
                ((:file "block-chain"))))

  :description "Block chain example system."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op block-chain-server-test))))
