(setf sb-impl::*default-external-format* :utf-8)
(setf sb-alien::*default-c-string-external-format* :utf-8)
(ql:quickload :block-chain-server)
block-chain-server:*app*
