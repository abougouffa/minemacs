;;; me-ros.el --- Robot Operating System -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Needed by `ros', but provided by `tramp'
(push 'docker-tramp straight-built-in-pseudo-packages)

;; ROS package
(use-package ros
  :straight (:host github :repo "DerBeutlin/ros.el")
  :commands (ros-set-network-setting ros-set-workspace ros-go-to-package ros-clean-cache))

(use-package rosbag-info
  :straight (:host github :repo "abougouffa/rosbag-info"))

(use-package robot-mode
  :straight t)


(provide 'me-robot)

;;; me-robot.el ends here
