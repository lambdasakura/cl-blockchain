(in-package :cl-user)
(defpackage block-chain-server-test-asd
  (:use :cl :asdf))
(in-package :block-chain-server-test-asd)

(defsystem block-chain-server-test
  :author "lambda_sakura"
  :license "MIT"
  :depends-on (:block-chain-server
               :prove)
  :components ((:module "t"
                :components
                ((:file "block-chain-server-test"))
                :serial t
                ))
  :perform (load-op :after (op c) (asdf:clear-system c)))
