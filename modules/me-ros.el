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
  (me-global-def
    "or" '(nil :which-key "ROS")
    "orr" '(hydra-ros-main/body :which-key "Hydra")
    "ors" '(hydra-ros-main/body :which-key "Set workspace")))


(provide 'me-ros)
