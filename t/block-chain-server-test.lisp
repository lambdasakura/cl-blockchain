(in-package :cl-user)
(defpackage block-chain-server-test
  (:use :cl
        :block-chain-server
        :prove))
(in-package :block-chain-server-test)

(defparameter long-chain-json "{\"chain\":{\"chain\":[{\"index\":1,\"transactions\":[],\"previous-hash\":1,\"timestamp\":\"2018-05-18T13:35:15.877091+09:00\",\"proof\":500},{\"index\":2,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"DC6E6FAD-F064-442D-A2D0-CCB2450AE33F\",\"amount\":1}],\"previous-hash\":\"b190e02fdce4a7194b1794c52d054fc002c21e7986f81f53c62d1283488b87d6\",\"timestamp\":\"2018-05-18T13:35:22.972903+09:00\",\"proof\":38836},{\"index\":3,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"DC6E6FAD-F064-442D-A2D0-CCB2450AE33F\",\"amount\":1}],\"previous-hash\":\"f13c61b74a545beee1a06cf0634fe2927e57212eb1be474857a2f04fff366068\",\"timestamp\":\"2018-05-18T13:35:25.405216+09:00\",\"proof\":128289},{\"index\":4,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"DC6E6FAD-F064-442D-A2D0-CCB2450AE33F\",\"amount\":1}],\"previous-hash\":\"494663e382bf7b2809e660c4ced5f53489a75869864f4ba411ca3ed9c94eb050\",\"timestamp\":\"2018-05-18T13:35:26.532470+09:00\",\"proof\":202706},{\"index\":5,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"DC6E6FAD-F064-442D-A2D0-CCB2450AE33F\",\"amount\":1}],\"previous-hash\":\"a75f298ee7f84974c90db2571f8658353ca6c3d29075656c0a4d6e4739de2780\",\"timestamp\":\"2018-05-18T13:35:27.404849+09:00\",\"proof\":112571},{\"index\":6,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"DC6E6FAD-F064-442D-A2D0-CCB2450AE33F\",\"amount\":1}],\"previous-hash\":\"4e5986b7b6b6c49c1218926c56ea2cf276d4d2abf5f060e60fb0065f0a79426c\",\"timestamp\":\"2018-05-18T13:35:28.101835+09:00\",\"proof\":52273},{\"index\":7,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"DC6E6FAD-F064-442D-A2D0-CCB2450AE33F\",\"amount\":1}],\"previous-hash\":\"a0e626f11d0dc7d3e7b295e4aeed47a3e51e2734b0707e12c3a336cef042ab7f\",\"timestamp\":\"2018-05-18T13:35:28.904925+09:00\",\"proof\":56503},{\"index\":8,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"DC6E6FAD-F064-442D-A2D0-CCB2450AE33F\",\"amount\":1}],\"previous-hash\":\"03720bc2a6fc18e4fb63d259d59bf8237c62eb7df41cf23cb9eb4df4abea39e5\",\"timestamp\":\"2018-05-18T13:35:29.524474+09:00\",\"proof\":12646},{\"index\":9,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"DC6E6FAD-F064-442D-A2D0-CCB2450AE33F\",\"amount\":1}],\"previous-hash\":\"10535bbe4611dabb3addda50d476b2293d98d6926c2e56647ce35abbaca36dba\",\"timestamp\":\"2018-05-18T13:35:30.150491+09:00\",\"proof\":10755},{\"index\":10,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"DC6E6FAD-F064-442D-A2D0-CCB2450AE33F\",\"amount\":1}],\"previous-hash\":\"311162430ebe6b1756128aa873231b85c0857c6dca862d0b97e1acde243c434f\",\"timestamp\":\"2018-05-18T13:35:31.009706+09:00\",\"proof\":88964}],\"current-transaction\":[]},\"length\":10}")

(defparameter small-chain-json "{\"chain\":{\"chain\":[{\"index\":1,\"transactions\":[],\"previous-hash\":1,\"timestamp\":\"2018-05-18T13:44:37.608725+09:00\",\"proof\":500},{\"index\":2,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"72933126-EBD0-49AB-A0F7-04CB7CE1C23F\",\"amount\":1}],\"previous-hash\":\"226c1785e210b1710654fcf01e56270b4dabe23be0da2f539b55b25a2ee2e342\",\"timestamp\":\"2018-05-18T13:44:43.109623+09:00\",\"proof\":38836},{\"index\":3,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"72933126-EBD0-49AB-A0F7-04CB7CE1C23F\",\"amount\":1}],\"previous-hash\":\"e7a9d31aa2c8a5e258e1f326577d4284c6d05dce17cf3c2df3914f344888579d\",\"timestamp\":\"2018-05-18T13:44:44.377801+09:00\",\"proof\":128289},{\"index\":4,\"transactions\":[{\"sender\":\"0\",\"recipient\":\"72933126-EBD0-49AB-A0F7-04CB7CE1C23F\",\"amount\":1}],\"previous-hash\":\"5bf3405d26a3f2012319c3f79f5afd9658f8943505648138b7fd520af79e42a7\",\"timestamp\":\"2018-05-18T13:44:45.523825+09:00\",\"proof\":202706}],\"current-transaction\":[]},\"length\":4}")

(plan nil)
(setf *block-chain* (block-chain-server:init-block-chain))
(isnt (block-chain-server:init-block-chain) nil)

(setf block-chain-server:*nodes* nil)
(print (resolve-conflict *block-chain* *nodes*))
(is (resolve-conflict *block-chain* *nodes*) nil)

(register-node "localhost:5000")
(register-node "localhost:5000")
(register-node "localhost:5001")
(register-node "localhost:5002")
(is (length *nodes*) 3)


(is (length (gethash "chain" *block-chain*)) 1)
(mine *block-chain* "nodeid")
(mine *block-chain* "nodeid")
(mine *block-chain* "nodeid")
(is (length (gethash "chain" *block-chain*)) 4)

(is (valid-chain *block-chain*) t)


(setf *block-chain* (gethash "chain" (jonathan:parse small-chain-json :as :hash-table)))
(is (length (gethash "chain" *block-chain*)) 4)
(mine *block-chain* "nodeid")
(mine *block-chain* "nodeid")
(mine *block-chain* "nodeid")
(is (valid-chain *block-chain*) t)
(is (length (gethash "chain" *block-chain*)) 7)

(setf small-block-chain (jonathan:parse small-chain-json :as :hash-table))
(setf long-block-chain (jonathan:parse long-chain-json :as :hash-table))

(is (length (gethash "chain" (get-longest-chain (list small-block-chain long-block-chain))))
    (length (gethash "chain" (gethash "chain" long-block-chain))))

(finalize)
