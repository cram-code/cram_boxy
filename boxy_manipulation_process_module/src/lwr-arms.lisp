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

(defparameter *left-beasty-action-name* "/BEASTY"
  "ROS name of the Beasty action server for the left arm.")
(defparameter *left-simulation-flag* nil
  "Flag indicating whether the left LWR is a simulated arm.")
(defparameter *left-tool-weight* 0.47
  "Weight of the tool mounted to the left LWR in kg.")
(defparameter *left-tool-com* (cl-transforms:make-3d-vector -0.04 -0.04 0.0)
  "Center of mass of tool mounted on left arm.")
(defparameter *left-arm-tool*
  (make-instance 'beasty-tool :mass *left-tool-weight* :com *left-tool-com*)
  "Modelling of tool mounted on left LWR.")
(defparameter *left-gravity-vector* #(0 0 9.81 0 0 0)
  "_NEGATIV_ 6D acceleration vector indicating in which direction gravity is acting on the
  left arm. NOTE: Is expressed w.r.t. to the base-frame of the left arm. First translational (x,y,z), then rotational (x,y,z) acceleration.")  
(defparameter *left-arm-base-config*
  (make-instance 'beasty-base :base-acceleration *left-gravity-vector*)
  "Modelling of mounting of left LWR to the torso of the robot.")
(defparameter *left-arm-config*
  (make-instance 'beasty-robot 
                 :simulation-flag *left-simulation-flag* 
                 :tool-configuration *left-arm-tool* 
                 :base-configuration *left-arm-base-config*)
  "Modelling of entire initial configuration of left LWR.")

(defun init-boxy-manipulation-process-module ()
  "Inits connection to hardware drivers used by the process module."
  (unless *left-arm* 
    (setf *left-arm* (make-beasty-interface *left-beasty-action-name* *left-arm-config*))))

(defun clean-up-boxy-manipulation-process-module ()
  (when *left-arm*
    (cleanup-beasty-interface *left-arm*)
    (setf *left-arm* nil)))

(roslisp-utilities:register-ros-init-function init-boxy-manipulation-process-module)
(roslisp-utilities:register-ros-cleanup-function clean-up-boxy-manipulation-process-module)