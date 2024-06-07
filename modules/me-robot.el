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
  :commands +hydra-ros-main/body ros-set-network-setting ros-set-workspace ros-go-to-package ros-clean-cache
  :config
  (defhydra +hydra-ros-main (:color blue :hint nil :foreign-keys warn)
    "
[ROS]                                                  [_q_] quit
  ├──────────────────────────────────────────────────────────────────────╮
  │  [_c_] Compile    [_t_] Test       [_w_] Set workspace   [_p_] Packages      │
  │  [_m_] Messages   [_s_] Services   [_a_] Actions         [_x_] Clean cache   │
  ╰──────────────────────────────────────────────────────────────────────╯
"
    ("c" ros-colcon-build-transient)
    ("t" ros-colcon-test-transient)
    ("w" ros-set-workspace)
    ("p" hydra-ros-packages/body)
    ("m" hydra-ros-messages/body)
    ("s" hydra-ros-srvs/body)
    ("a" hydra-ros-actions/body)
    ("x" ros-cache-clean)
    ("q" nil :color blue)))

(use-package rosbag-info
  :straight (:host github :repo "abougouffa/rosbag-info"))

(use-package robot-mode
  :straight t)


(provide 'me-robot)

;;; me-robot.el ends here
