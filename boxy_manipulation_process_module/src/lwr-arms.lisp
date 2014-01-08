;;; Copyright (c) 2014, Georg Bartels <georg.bartels@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; * Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;; * Neither the name of the Institute for Artificial Intelligence/
;;; Universitaet Bremen nor the names of its contributors may be used to 
;;; endorse or promote products derived from this software without specific 
;;; prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :boxy-manipulation-process-module)

(defvar *left-arm* nil
  "Variable holding the interface to the Beasty controller of the left arm.")
(defvar *left-arm-config* nil
  "Variable holding the current configuration of the left arm.")

(defparameter *left-beasty-action-name* "/LEFT_BEASTY"
  "ROS name of the Beasty action server for the left arm.")
(defparameter *left-simulation-flag* t
  "Flag indicating whether the left LWR is a simulated arm.")
(defparameter *left-gravity-vector* #(0 0 1 0 0 0)
  "_NEGATIV_ 6D acceleration vector indicating in which direction gravity is acting on the
  left arm. NOTE: Is expressed w.r.t. to the base-frame of the left arm. First translational (x,y,z), then rotational (x,y,z) acceleration.")

(defun init-boxy-manipulation-process-module ()
  "Inits connection to hardware drivers used by the process module."
  (unless *left-arm* 
    (setf *left-arm* (make-beasty-interface *left-beasty-action-name*)))
  (unless *left-arm-config*
    (setf *left-arm-config* (init-beasty-robot *left-simulation-flag* 
                                               *left-gravity-vector*))))
(defun clean-up-boxy-manipulation-process-module ()
  (when *left-arm*
    ;; TODO(Georg): stop arm
    ;; TODO(Georg): logout with Beasty
    (setf *left-arm* nil))
  (when *left-arm-config*
    (setf *left-arm-config* nil)))

(roslisp-utilities:register-ros-init-function init-boxy-manipulation-process-module)
(roslisp-utilities:register-ros-cleanup-function clean-up-boxy-manipulation-process-module)
;; TODO(Georg): add clean-up-function with LOGOUT

(defun init-beasty-robot (simulated-p base-acceleration)
  "Creates, inits, and returns an instance of class `beasty-robot'. `simulated-p' signals
 whether this a simulated LWR, and the 6D-vector `base-acceleration' denotes the _negative_
 external acceleration acting on the robot."
  (declare (type boolean simulated-p)
           (type (vector number 6) base-acceleration))
  (let ((robot (make-instance 'beasty-robot)))
    (setf (simulation-flag robot) simulated-p)
    (setf (base-acceleration (base-configuration robot))  base-acceleration)
    robot))