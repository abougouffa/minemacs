;; -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.rviz\\'"   . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'"   . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xacro\\'"  . xml-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;; Use gdb-script-mode for msg and srv files
(add-to-list 'auto-mode-alist '("\\.msg\\'"    . gdb-script-mode))
(add-to-list 'auto-mode-alist '("\\.srv\\'"    . gdb-script-mode))
(add-to-list 'auto-mode-alist '("\\.action\\'" . gdb-script-mode))

;;; A mode to display infos for ROS bag files
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
      (view-mode)
      (set-visited-file-name nil t)))

  ;; rosbag view mode
  (add-to-list 'auto-mode-alist '("\\.bag$" . rosbag-view-mode)))

;; ROS package
(use-package ros
  :init
  :general
  (me-local-def :leader
                :prefix ("l" . "custom")
                :desc "Hydra ROS" "r" #'hydra-ros-main/body)
  :commands (hydra-ros-main/body ros-set-workspace)
  :config
  (setq ros-workspaces
        (list (ros-dump-workspace
               :tramp-prefix (format "/docker:%s@%s:" "ros" "ros-machine")
               :workspace "~/ros_ws"
               :extends '("/opt/ros/noetic/"))
              (ros-dump-workspace
               :tramp-prefix (format "/ssh:%s@%s:" "swd_sk" "172.16.96.42")
               :workspace "~/ros_ws"
               :extends '("/opt/ros/noetic/"))
              (ros-dump-workspace
               :tramp-prefix (format "/ssh:%s@%s:" "swd_sk" "172.16.96.42")
               :workspace "~/ros2_ws"
               :extends '("/opt/ros/foxy/")))))
