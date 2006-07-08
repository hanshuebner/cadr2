;;; DON (disk over network) UDP server and client test program
;;; (C) 2006 Frank Buss
;;; see COPYING for license
;;;
;;; start #'server-loop on the server side and
;;; #'test-client on the same computer for testing

(defpackage #:cadr2-server
  (:use #:cl #:cffi))

(in-package #:cadr2-server)


;; application specific constants

(defconstant +ping+ 1)
(defconstant +pong+ 2)
(defconstant +read-request+ 10)
(defconstant +read-response+ 11)
(defconstant +write-request+ 12)
(defconstant +write-response+ 13)
(defconstant +max-message-size+ 1029)
(defconstant +server-port+ 17220)
(defconstant +disk-block-size+ 1024)
(defconstant +disk-blocks+ 2048)
(defconstant +disk-file+ "/tmp/cadr2-disk.bin")


;; network layer constants

(defconstant WSADESCRIPTION_LEN 256)
(defconstant WSASYS_STATUS_LEN 128)
(defconstant AF_INET 2)
(defconstant SOCK_DGRAM 2)
(defconstant IPPROTO_UDP 17)


;; network structs

(defcstruct WSADDATA
  (wVersion :int)
  (wHighVersion :int)
  (szDescription :char :count #.(1+ WSADESCRIPTION_LEN))
  (szSystemStatus :char :count #.(1+ WSASYS_STATUS_LEN))
  (iMaxSockets :int)
  (iMaxUdpDg :int)
  (lpVendorInfo :pointer))

(defcstruct SOCKADDR_IN
  (sin_family :unsigned-short)
  (sin_port :unsigned-short)
  (sin_addr :unsigned-int)
  (sin_zero :char :count 8))


;; network native functions

#+:win32
(defcfun ("WSAStartup" %WSAStartup) :int
  (wVersionRequired :int)
  (lpWSAData :pointer))

(defcfun ("socket" %socket) :int
  (af :int)
  (type :int)
  (protocol :int))

#+:win32
(defcfun ("closesocket" %closesocket) :int
  (socket :int))

#-:win32
(defcfun ("close" %closesocket) :int
  (socket :int))

(defcfun ("bind" %bind) :int
  (socket :int)
  (addr :pointer)
  (namelen :int))

(defcfun ("htons" %htons) :unsigned-int
  (port :unsigned-int))

(defcfun ("ntohs" %ntohs) :unsigned-int
  (port :unsigned-int))

(defcfun ("inet_addr" %inet_addr) :int
  (host :string))

(defcfun ("inet_ntoa" %inet_ntoa) :pointer
  (sockaddr :int))

(defcfun ("recvfrom" %recvfrom) :int
  (socket :int)
  (buf :pointer)
  (len :int)
  (flags :int)
  (from :pointer)
  (fromlen :pointer))

(defcfun ("sendto" %sendto) :int
  (socket :int)
  (buf :pointer)
  (len :int)
  (flags :int)
  (to :pointer)
  (tolen :int))


;; helper functions

(defun write-dword (data offset dword)
  (setf (aref data offset) (ldb (byte 8 0) dword)
        (aref data (+ offset 1))(ldb (byte 8 8) dword)
        (aref data (+ offset 2))(ldb (byte 8 16) dword)
        (aref data (+ offset 3))(ldb (byte 8 24) dword)))

(defun read-dword (data offset)
  (let ((dword 0)) 
    (setf (ldb (byte 8 0) dword) (aref data offset)
          (ldb (byte 8 8) dword) (aref data (+ offset 1))
          (ldb (byte 8 16) dword) (aref data (+ offset 2))
          (ldb (byte 8 24) dword) (aref data (+ offset 3)))
    dword))

(defun zero-sockaddr (sockaddr)
  (dotimes (i (foreign-type-size 'SOCKADDR_IN))
    (setf (mem-aref sockaddr :char i) 0)))

#+:win32
(let ((winsock-started nil))
  (defun start-winsock ()
    (unless winsock-started
      (load-foreign-library "wsock32.dll")
      (with-foreign-object (wsa 'WSADDATA)
        (when (/= 0 (%WSAStartup #x0101 wsa))
          (error "error starting winsock")))
      (setf winsock-started t))))


;; socket class

(defclass socket ()
  ((socket)))

(defmethod initialize-instance :after ((instance socket) &key)
  #+:win32 (start-winsock)
  (let ((socket (%socket AF_INET SOCK_DGRAM IPPROTO_UDP)))
    (when (< socket 0)
      (error "error creating socket"))
    (setf (slot-value instance 'socket) socket)
    (finalize instance (lambda () (%closesocket socket)))))

(defmethod bind ((instance socket) port)
  (with-foreign-object (sockaddr 'SOCKADDR_IN)
    (zero-sockaddr sockaddr)
    (setf (foreign-slot-value sockaddr 'SOCKADDR_IN 'sin_family) AF_INET)
    (setf (foreign-slot-value sockaddr 'SOCKADDR_IN 'sin_port) (%htons port))
    (when (< (%bind (slot-value instance 'socket) sockaddr (foreign-type-size 'SOCKADDR_IN)) 0)
      (error "can't bind socket"))))

(defmethod receive-from ((instance socket))
  (with-foreign-object (buf :unsigned-char +max-message-size+)
    (with-foreign-object (from 'SOCKADDR_IN)
      (zero-sockaddr from)
      (with-foreign-object (fromlen :int)
        (setf (mem-ref fromlen :int) (foreign-type-size 'SOCKADDR_IN))
        (let ((result (%recvfrom (slot-value instance 'socket)
                                 buf
                                 +max-message-size+
                                 0
                                 from
                                 fromlen)))
          (when (> result 0)
            (let ((data (make-array result :element-type '(unsigned-byte 8)))
                  (host (%inet_ntoa
                         (foreign-slot-value from 'SOCKADDR_IN 'sin_addr))))
              (loop for i from 0 below result do
                    (setf (aref data i) (mem-aref buf :unsigned-char i)))
              (values data
                      (foreign-string-to-lisp host)
                      (%ntohs (foreign-slot-value from 'SOCKADDR_IN 'sin_port))))))))))

(defmethod send-to ((instance socket) host port data)
  (with-foreign-object (sockaddr 'SOCKADDR_IN)
    (with-foreign-object (buf :unsigned-char (length data))
      (zero-sockaddr sockaddr)
      (setf (foreign-slot-value sockaddr 'SOCKADDR_IN 'sin_family) AF_INET)
      (setf (foreign-slot-value sockaddr 'SOCKADDR_IN 'sin_port) (%htons port))
      (setf (foreign-slot-value sockaddr 'SOCKADDR_IN 'sin_addr) (%inet_addr host))
      (loop for i from 0 below (length data) do
            (setf (mem-aref buf :unsigned-char i) (aref data i)))
      (%sendto (slot-value instance 'socket)
               buf
               (length data)
               0
               sockaddr
               (foreign-type-size 'SOCKADDR_IN)))))

(defmethod socket-close ((instance socket))
  (%closesocket (slot-value instance 'socket)))

(defmacro with-socket (socket &body body)
  `(let ((,socket (make-instance 'socket)))
     (unwind-protect (progn ,@body)
       (socket-close ,socket))))


;; server implementation and test client

(defun on-read-request (socket data host port)
  (format t "read-request: sending data to ~a:~a~%" host port)
  (when (/= (length data) 5)
    (error "wrong data length"))
  (let ((block-number (read-dword data 1)))
    (when (>= block-number +disk-blocks+)
      (error "block number out of range"))
    (with-open-file (s +disk-file+
                       :direction :input
                       :element-type '(unsigned-byte 8))
      (file-position s (* +disk-block-size+ block-number))
      (let ((out (make-array (+ 5 +disk-block-size+))))
        (setf (aref out 0) +read-response+)
        (write-dword out 1 block-number)
        (loop for i from 0 below +disk-block-size+ do
              (setf (aref out (+ 5 i)) (read-byte s)))
        (send-to socket host port out)))))

(defun on-write-request (socket data host port)
  (format t "write-request: sending acknowledge to ~a:~a~%" host port)
  (when (/= (length data) (+ 5 +disk-block-size+))
    (error "wrong data length"))
  (let ((block-number (read-dword data 1)))
    (when (>= block-number +disk-blocks+)
      (error "block number out of range"))
    (with-open-file (s +disk-file+
                       :if-exists :overwrite
                       :direction :output
                       :element-type '(unsigned-byte 8))
      (file-position s (* +disk-block-size+ block-number))
      (loop for i from 0 below +disk-block-size+ do
            (write-byte (aref data (+ 5 i)) s))
      (let ((out (make-array 5)))
        (setf (aref out 0) +write-response+)
        (write-dword out 1 block-number)
        (send-to socket host port out)))))

(defun server-loop ()
  (unless (probe-file +disk-file+)
    (with-open-file (s +disk-file+
                       :direction :output
                       :element-type '(unsigned-byte 8))
      (loop for i from 0 below (* +disk-block-size+ +disk-blocks+) do
            (write-byte 0 s))))
  (with-socket socket
    (bind socket +server-port+)
    (loop do
          (multiple-value-bind (data host port) (receive-from socket)
            (when (> (length data) 0)
              (let ((command (aref data 0)))
                (cond ((= command +ping+)
                       (format t "ping: sending pong to ~a:~a~%" host port)
                       (send-to socket host port #(#.+pong+)))
                      ((= command +read-request+)
                       (on-read-request socket data host port))
                      ((= command +write-request+)
                       (on-write-request socket data host port)))))))))

(defun on-read-response (data)
  (format t "read-response for block ~a, first 10 bytes: ~a~%"
          (read-dword data 1)
          (subseq data 5 15)))

(defun on-write-response (data)
  (format t "write-response for block ~a~%"
          (read-dword data 1)))

(defun parse-result (data)
  (when (> (length data) 0)
    (let ((command (aref data 0)))
      (cond ((= command +pong+)
             (format t "pong~%"))
            ((= command +read-response+)
             (on-read-response data))
            ((= command +write-response+)
             (on-write-response data))))))

(defun send-ping ()
  (with-socket socket
    (send-to socket "127.0.0.1" +server-port+ #(#.+ping+))
    (parse-result (receive-from socket))))

(defun send-read-request (block)
  (let ((data (make-array 5)))
    (setf (aref data 0) +read-request+)
    (write-dword data 1 block)
    (with-socket socket
      (send-to socket "127.0.0.1" +server-port+ data)
      (parse-result (receive-from socket)))))

(defun send-write-request (block first-bytes)
  (let ((data (make-array (+ 5 +disk-block-size+) :initial-element 0)))
    (setf (aref data 0) +write-request+)
    (write-dword data 1 block)
    (loop for i from 0 below (min +disk-block-size+ (length first-bytes)) do
          (setf (aref data (+ 5 i)) (aref first-bytes i)))
    (with-socket socket
      (send-to socket "127.0.0.1" +server-port+ data)
      (parse-result (receive-from socket)))))

(defun test-client ()
  (send-ping)
  (send-write-request 0 #(1 2 3 4))
  (send-write-request 1 #(5 6 7 8))
  (send-read-request 0)
  (send-read-request 1))
