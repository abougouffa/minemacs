;;; me-geojson.el --- Convert GeoJSON files to GPX -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-09-18
;; Last modified: 2026-09-25

;;; Commentary:

;; Converts a GeoJSON file into a GPX file.
;;
;; Geometry mapping:
;;
;;   Point, MultiPoint                  -> <wpt>
;;   LineString                         -> <trk> with one <trkseg>
;;   MultiLineString, Polygon           -> <trk> with one <trkseg> per line/ring
;;   MultiPolygon                       -> one <trk> per polygon
;;   GeometryCollection                 -> walked recursively
;;
;; Feature properties are searched for a name (see `+geojson-name-properties');
;; a third coordinate, if present, is emitted as <ele>.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'xml)

(defvar +geojson-name-properties '(name Name NAME title Title label description desc)
  "Feature properties consulted, in order, for the name of a track or waypoint.")

;;;; Reading

(defun +geojson--read (file)
  "Parse the GeoJSON FILE into alists and lists."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil :false-object nil)))

(defun +geojson--get (key object)
  "Return the value of KEY in the GeoJSON OBJECT."
  (and (consp object) (cdr (assq key object))))

(defun +geojson--name (properties)
  "Return a name for a feature with PROPERTIES, or nil."
  (cl-loop for key in +geojson-name-properties
           for val = (cdr (assq key properties))
           thereis (and (stringp val) (> (length val) 0) val)))

(defun +geojson--position-p (pos)
  "Return non-nil if POS is a GeoJSON position, i.e. (LON LAT [ELE])."
  (and (consp pos) (numberp (car pos)) (numberp (cadr pos))))

;;;; Writing

(defun +gpx--number (n)
  "Format the number N for GPX output, avoiding exponential notation."
  (format "%.10f" (float n)))

(defun +gpx--point (tag position name indent)
  "Return the GPX element TAG for POSITION, prefixed by INDENT.
NAME, if non-nil, is added as a <name> child."
  (let* ((lon (nth 0 position))
         (lat (nth 1 position))
         (ele (nth 2 position))
         (body (concat
                (and (numberp ele) (format "<ele>%s</ele>" (+gpx--number ele)))
                (and name (format "<name>%s</name>" (xml-escape-string name))))))
    (concat indent
            (format "<%s lat=\"%s\" lon=\"%s\"" tag (+gpx--number lat) (+gpx--number lon))
            (if (equal body "") "/>\n" (format ">%s</%s>\n" body tag)))))

(defun +gpx--track (name segments)
  "Return a GPX <trk> element called NAME containing SEGMENTS."
  (concat "  <trk>\n"
          (and name (format "    <name>%s</name>\n" (xml-escape-string name)))
          (mapconcat
           (lambda (segment)
             (concat "    <trkseg>\n"
                     (mapconcat (lambda (pos) (+gpx--point "trkpt" pos nil "      "))
                                (cl-remove-if-not #'+geojson--position-p segment)
                                "")
                     "    </trkseg>\n"))
           (cl-remove-if #'null segments)
           "")
          "  </trk>\n"))

;;;; Conversion

(defun +geojson-to-gpx-string (data &optional title)
  "Convert the parsed GeoJSON DATA into a GPX document string.
TITLE, if non-nil, is used as the name in the GPX metadata."
  (let ((waypoints nil) (tracks nil))
    (cl-labels
        ((geometry (geom label)
           (let ((type (+geojson--get 'type geom))
                 (coords (+geojson--get 'coordinates geom)))
             (pcase type
               ('nil nil)
               ("Point"
                (when (+geojson--position-p coords)
                  (push (cons label coords) waypoints)))
               ("MultiPoint"
                (dolist (pos coords)
                  (when (+geojson--position-p pos)
                    (push (cons label pos) waypoints))))
               ("LineString" (push (cons label (list coords)) tracks))
               ((or "MultiLineString" "Polygon") (push (cons label coords) tracks))
               ("MultiPolygon"
                (dolist (polygon coords)
                  (push (cons label polygon) tracks)))
               ("GeometryCollection"
                (dolist (geo (+geojson--get 'geometries geom))
                  (geometry geo label)))
               (_ (message "Ignoring unsupported GeoJSON geometry `%s'" type)))))
         (feature (obj)
           (pcase (+geojson--get 'type obj)
             ("FeatureCollection"
              (mapc #'feature (+geojson--get 'features obj)))
             ("Feature"
              (geometry (+geojson--get 'geometry obj)
                        (+geojson--name (+geojson--get 'properties obj))))
             (_ (geometry obj nil)))))
      (feature data))
    (setq waypoints (nreverse waypoints)
          tracks (nreverse tracks))
    (unless (or waypoints tracks)
      (error "No point or line geometry found in the GeoJSON data"))
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<gpx version=\"1.1\" creator=\"MinEmacs\" xmlns=\"http://www.topografix.com/GPX/1/1\">\n"
     (and title (format "  <metadata><name>%s</name></metadata>\n" (xml-escape-string title)))
     (mapconcat (lambda (wpt) (+gpx--point "wpt" (cdr wpt) (car wpt) "  ")) waypoints "")
     (mapconcat (lambda (trk) (+gpx--track (car trk) (cdr trk))) tracks "")
     "</gpx>\n")))

;;;###autoload
(defun +geojson-to-gpx (file &optional output)
  "Convert the GeoJSON FILE to GPX and return the name of the GPX file.
The result is written to OUTPUT, or to a temporary file if OUTPUT is nil.
Interactively, the default OUTPUT sits next to FILE."
  (interactive
   (let ((file (read-file-name "GeoJSON file: " nil nil t)))
     (list file (read-file-name "Write GPX to: " nil nil nil (concat (file-name-base file) ".gpx")))))
  (let ((gpx (+geojson-to-gpx-string (+geojson--read file) (file-name-nondirectory file)))
        (out (or output (make-temp-file (file-name-base file) nil ".gpx"))))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file out (insert gpx)))
    (when (called-interactively-p 'interactive)
      (message "Wrote %s" out))
    out))


(provide 'me-geojson)
;;; me-geojson.el ends here
