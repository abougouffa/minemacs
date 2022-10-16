;; -*- lexical-binding: t; -*-

(with-eval-after-load 'minemacs-loaded
  (add-to-list 'auto-mode-alist '("\\.rviz\\'"   . conf-unix-mode))
  (add-to-list 'auto-mode-alist '("\\.urdf\\'"   . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.xacro\\'"  . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

  ;; Use gdb-script-mode for msg and srv files
  (add-to-list 'auto-mode-alist '("\\.msg\\'"    . gdb-script-mode))
  (add-to-list 'auto-mode-alist '("\\.srv\\'"    . gdb-script-mode))
  (add-to-list 'auto-mode-alist '("\\.action\\'" . gdb-script-mode))

  ;; A mode to display infos for ROS bag files
  (when (executable-find "rosbag")
    (define-derived-mode rosbag-view-mode
      fundamental-mode "Rosbag view mode"
      "Major mode for viewing ROS bag files."
      (let ((f (buffer-file-name)))
        (let ((buffer-read-only nil))
          (erase-buffer)
          (message "Calling rosbag info")
          (call-process "rosbag" nil (current-buffer) nil
                        "info" f)
          (set-buffer-modified-p nil))
        (view-mode
         (set-visited-file-name nil t))))

    ;; rosbag view mode
    (add-to-list 'auto-mode-alist '("\\.bag$" . rosbag-view-mode))))


;; Needed by ros.el
(use-package kv
  :straight t
  :defer t)

(use-package string-inflection
  :straight t
  :defer t)

(use-package with-shell-interpreter
  :straight t
  :defer t)

(when (< emacs-major-version 29)
  (use-package docker-tramp
    :straight t
    :defer t))


;; ROS package
(use-package ros
  :straight (:host github :repo "abougouffa/ros.el" :branch "fix-deps")
  :general
  (+map
    "or" '(nil :which-key "ROS")
    "orr" '(hydra-ros-main/body :which-key "Hydra")
    "ors" '(ros-set-workspace :which-key "Set workspace")
    "orp" '(ros-go-to-package :which-key "Go to package")
    "orC" '(ros-cache-clean :which-key "Clean cache"))

  :config
  (defhydra hydra-ros-main (:color blue :hint nil :foreign-keys warn)
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


(provide 'me-ros)
