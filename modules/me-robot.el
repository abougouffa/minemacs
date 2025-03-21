;;; me-ros.el --- Robot Operating System -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; A package to ease the interaction ROS nodes and the development of ROS software
(use-package ros
  :straight (:host github :repo "mbeutelspacher/ros.el")
  :commands (ros-set-network-setting ros-set-workspace ros-go-to-package ros-clean-cache))


;; Show information about ROS bag files in Emacs
(use-package rosbag-info
  :straight (:host github :repo "abougouffa/rosbag-info"))


(provide 'me-robot)

;;; me-robot.el ends here
