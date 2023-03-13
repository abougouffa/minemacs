;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


;;;###autoload
(defun +systemd-running-p (service)
  "Check if the systemd SERVICE is running."
  (zerop (call-process "systemctl" nil nil nil "--user" "is-active" "--quiet" service ".service")))

;;;###autoload
(defun +systemd-command (service command &optional pre-fn post-fn)
  "Call systemd with COMMAND and SERVICE."
  (when pre-fn (funcall pre-fn))
  (let ((success (zerop (call-process "systemctl" nil nil nil "--user" command service ".service"))))
    (unless success
      (user-error "[systemd]: Failed on calling '%s' on service %s.service." command service))
    (when post-fn (funcall post-fn success))
    success))

;;;###autoload
(defun +systemd-start (service &optional pre-fn post-fn)
  "Start systemd SERVICE."
  (+systemd-command service "start" pre-fn post-fn))

;;;###autoload
(defun +systemd-stop (service &optional pre-fn post-fn)
  "Stops the systemd SERVICE."
  (+systemd-command service "stop" pre-fn post-fn))
