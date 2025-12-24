;;; me-ros.el --- Robot Operating System -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;; A package to ease the interaction ROS nodes and the development of ROS software
(use-package ros
  :vc (:url "https://github.com/mbeutelspacher/ros.el")
  :commands (ros-set-network-setting ros-set-workspace ros-go-to-package ros-clean-cache))


;; Show information about ROS bag files in Emacs
(use-package rosbag-info
  :vc (:url "https://github.com/abougouffa/rosbag-info"))


(provide 'obsolete/me-ros)

;;; me-ros.el ends here
