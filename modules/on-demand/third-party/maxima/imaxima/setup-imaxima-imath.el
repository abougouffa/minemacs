;; Copyright (C) 2007, 2008, 2017 Yasuaki Honda
;; Copyright (C) 2017 Roland Salz

;; Author: Yasuaki Honda (yasuaki.honda@gmail.com)
;; $Id: setup-imaxima-imath.el,v 1.6 2009-02-22 09:18:27 yasu-honda Exp $

;; Adapted by Roland Salz 2016-2017
;; to be used alternatively with builds from installer, repository-snapshot or tarball.
;; In .emacs 
;; 		- define and bind global variable *maxima-build-type* to "repo-tarball" or "installer" and
;; 		- define and bind global variable *maxima-build-dir* to the root directory of the build,
;;			as a string terminated by a slash.
;; If these two variables are not defined and bound in .emacs, 
;; 		as default the first installer found in "C:/" is used.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA


;;; Set up Emacs for Imaxima with MiKTeX, and Ghostscript


; Set up MikTeX:
; MikTeX has to be installed before.
; Packages breqn, l3kernel, mathtools, unicode-data (and mhequ?) have to be installed with the MikTeX package manager before.

(defvar *imaxima-miktex-bin-dir*
  (cond ((file-expand-wildcards "c:/Program Files*/MiKTeX*/miktex/bin/latex.exe")
	 (file-name-directory (car (file-expand-wildcards "c:/Program Files*/MiKTeX*/miktex/bin/latex.exe"))))
	((file-expand-wildcards "c:/Program Files*/MiKTeX*/miktex/bin/x64/latex.exe")
	 (file-name-directory (car (file-expand-wildcards "c:/Program Files*/MiKTeX*/miktex/bin/x64/latex.exe"))))
	(t (display-warning :error "setup-imaxima-imath.el: MikteX not found in C:/Programa Files/")))
  "MiKTeX bin directory, added to exec-path")

; latex.exe is specified for the latex program to be invoked.
(setq imaxima-tex-program "latex.exe")


; Set up Ghostscript:  
; Ghostscript has to be installed before.

(defvar *imaxima-gs-bin-dir*
  (if (file-expand-wildcards "c:/Program Files*/gs/gs*/bin/gswin*.exe")
      (file-name-directory (car (file-expand-wildcards "c:/Program Files*/gs/gs*/bin/gswin*.exe")))
    (display-warning :error "setup-imaxima-imath.el: Ghostscript not found in C:/Programa Files/"))
  "Ghostscript bin directory, added to exec-path")

; ghostscript executable is specified for the gs program to be invoked.
(setq imaxima-gs-program
      (file-name-nondirectory (car (file-expand-wildcards "c:/Program Files*/gs/gs*/bin/gswin??c.exe"))))


; Set up path to maxima.bat:

(defvar *imaxima-maxima-bin-dir*
  (if (and (boundp '*maxima-build-type*) (boundp '*maxima-build-dir*))
      (cond ((equal *maxima-build-type* "repo-tarball")
	     (concat *maxima-build-dir* "src/"))
	    ((equal *maxima-build-type* "installer")
	     (concat *maxima-build-dir* "bin/"))
	    (t (display-warning :error "setup-imaxima-imath.el: *maxima-build-type* invalid")))
    (if (file-expand-wildcards "c:/Maxima*/bin/maxima.bat")
	(file-name-directory (car (file-expand-wildcards "c:/Maxima*/bin/maxima.bat")))
      (display-warning :error "setup-imaxima-imath.el: no Maxima installer found in C:/")))
  "Maxima bin directory, added to exec-path")

(if (not (file-exists-p (concat *imaxima-maxima-bin-dir* "maxima.bat")))
    (display-warning :error "setup-imaxima-imath.el: maxima.bat not found"))

; set up maxima-command to maxima.bat, which resides in the above path.
(setq maxima-command "maxima.bat")

; maxima.bat is specified as the maxima program for imaxima.
(setq imaxima-maxima-program "maxima.bat")


; Set up .el, imaxima and info directories:

(defvar *imaxima-maxima-el-dir*
  (if (and (boundp '*maxima-build-type*) (boundp '*maxima-build-dir*))
      (if (equal *maxima-build-type* "repo-tarball") 
	  (concat *maxima-build-dir* "interfaces/emacs/emaxima/")
	(file-name-directory (car (file-expand-wildcards (concat *maxima-build-dir* "share/maxima/*/emacs/maxima.el")))))	
    (file-name-directory (car (file-expand-wildcards "c:/Maxima*/share/maxima/*/emacs/maxima.el"))))
  "Maxima emacs mode maxima.el directory, added to load-path")

  
(defvar *imaxima-imath-dir* 
  (if (and (boundp '*maxima-build-type*) (boundp '*maxima-build-dir*))
      (if (equal *maxima-build-type* "repo-tarball") 
	  (concat *maxima-build-dir* "interfaces/emacs/imaxima/")
	*imaxima-maxima-el-dir*)
    *imaxima-maxima-el-dir*)
  "Imaxima imath directory, containing .el, .lisp and .info files")

;;; The following definition eases the locating of imaxima.lisp.
(setq imaxima-lisp-file (concat *imaxima-imath-dir* "imaxima.lisp"))


(defvar *imaxima-maxima-info-dir*
  (if (and (boundp '*maxima-build-type*) (boundp '*maxima-build-dir*))
      (if (equal *maxima-build-type* "repo-tarball") 
	  (concat *maxima-build-dir* "doc/info/")
	(concat *maxima-build-dir* "share/info/"))	
    (file-name-directory (car (file-expand-wildcards "c:/Maxima*/share/info/maxima.info"))))
  "Maxima info directory, added to Info-additional-directory-list")


;;; set up exec-path
(setq exec-path (append (list *imaxima-gs-bin-dir* *imaxima-maxima-bin-dir* *imaxima-miktex-bin-dir*) exec-path))

;;; set up load-path
(setq load-path (append (list *imaxima-imath-dir* *imaxima-maxima-el-dir*) load-path))

;;; set up Info-additional-directory-list
(if (not (boundp 'Info-additional-directory-list))
    (setq Info-additional-directory-list nil))

(setq Info-additional-directory-list (append (list *imaxima-imath-dir* *imaxima-maxima-info-dir*) Info-additional-directory-list))

;;; imaxima-maxima-options should be "" instead of "(user::run)"
;;; assigned in the imaxima.el
(setq imaxima-maxima-options "")

(autoload 'maxima "maxima" "Maxima CAS mode" t nil)

(autoload 'imaxima "imaxima" "Graphical frontend for Maxima CAS" t nil)

(autoload 'imath-mode "imath" "Math text mode" t nil)
