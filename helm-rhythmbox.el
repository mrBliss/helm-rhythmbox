;;; helm-rhythmbox.el --- control Rhythmbox's play queue via Helm

;; Copyright (C) 2015 by Thomas Winant

;; Author: Thomas Winant <dewinant@gmail.com>
;; URL: https://github.com/mrBliss/helm-rhyhmtbox
;; Version: 0.1
;; Package-Requires: ((helm "1.5.0") (cl-lib "0.5"))
;; Created: Mar 13 2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Control Rhythmbox's play queue via Helm.
;;
;; Start by calling `helm-rhythmbox' to browse the library retrieved
;; from a running Rhythmbox instance via D-Bus. The default action is
;; to play the selected song. There is also the option to enqueue the
;; selected song, this also works for a selection.
;;
;; The whole library is retrieved on first use and can be reloaded
;; with `helm-rhythmbox-reload-library'.

;;; Code:

(require 'dbus)
(require 'cl-lib)
(require 'cl-macs)
(require' helm)

;; A struct representing a song retrieved from rhythmbox
(cl-defstruct rhythmbox-song artist album title uri)

(defvar rhythmbox-library nil
  "Store the library.
A library consists of a list of `rhythmbox-song' structs representing the
Rhythmbox's current library.")

(defun rhythmbox-song-from-dbus-item (dbus-item)
  "Make a `rhythmbox-song' from DBUS-ITEM, a song retrieved via D-Bus."
  (make-rhythmbox-song :artist (cl-caadr  (assoc "Artist" dbus-item))
                       :album  (cl-caadr  (assoc "Album" dbus-item))
                       :title  (cl-caadr  (assoc "DisplayName" dbus-item))
                       :uri    (cl-caaadr (assoc "URLs" dbus-item))))

(defun rhythmbox-load-callback (dbus-items)
  "Callback for `rhythmbox-load-library'.
Will populate `rhythmbox-library' with DBUS-ITEMS using
`rhythmbox-song-from-dbus-item'."
  (setq rhythmbox-library
        (mapcar #'rhythmbox-song-from-dbus-item dbus-items)))

(defun rhythmbox-load-library ()
  "Load the Rhythmbox library via D-Bus."
  (let* ((service "org.gnome.Rhythmbox3")
         (path "/org/gnome/UPnP/MediaServer2/Library/all")
         (interface "org.gnome.UPnP.MediaContainer2")
         (nb-songs (dbus-get-property
                    :session service path interface "ChildCount")))
    (if (not nb-songs)
        (error "Couldn't connect to Rhythmbox")
      (dbus-call-method-asynchronously
       :session service path interface "ListChildren" #'rhythmbox-load-callback
       0 nb-songs '("*")))))

(defun helm-rhythmbox-reload-library ()
  "Reload the Rhythmbox library."
  (interactive)
  (rhythmbox-load-library))

(defun helm-rhythmbox-candidate-default-format (song)
  "Default candidate format function for `helm-rhythmbox'.
Formats the SONG as \"ARTIST - ALBUM - TITLE\"."
  (format "%s - %s - %s"
          (rhythmbox-song-artist song)
          (rhythmbox-song-album song)
          (rhythmbox-song-title song)))

(defvar helm-rhythmbox-candidate-format
  #'helm-rhythmbox-candidate-default-format
  "The function to format a candidate.
Will get a `rhythmbox-song' struct as input and must output a
string.  Defaults to `helm-rhythmbox-candidate-format'.")

(defun helm-rhythmbox-candidates ()
  "Make the list of candidates for `helm-rhythmbox'.
The list is composed of the entries in `rhythmbox-library'
formatted with `helm-rhythmbox-candidate-format'."
  (mapcar (lambda (song)
            (cons (funcall helm-rhythmbox-candidate-format song)
                  song))
          rhythmbox-library))

(defun helm-rhythmbox-play-song (song)
  "Let Rhythmbox play the given SONG."
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/mpris/MediaPlayer2")
        (interface "org.mpris.MediaPlayer2.Player"))
    (dbus-call-method :session service path interface
                      "OpenUri" (rhythmbox-song-uri song))))

(defun helm-rhythmbox-enqueue-song (_)
  "Let Rhythmbox enqueue the marked songs."
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/gnome/Rhythmbox3/PlayQueue")
        (interface "org.gnome.Rhythmbox3.PlayQueue"))
    (dolist (song (helm-marked-candidates))
      (dbus-call-method :session service path interface
                        "AddToQueue" (rhythmbox-song-uri song)))))

(defvar helm-source-rhythmbox-track-search
  '((name . "Rhythmbox")
    (candidates . helm-rhythmbox-candidates)
    (action . (("Play song" . helm-rhythmbox-play-song)
               ("Enqueue song" . helm-rhythmbox-enqueue-song)))
    (init . (lambda () (when (null rhythmbox-library)
                    (rhythmbox-load-library))))))

;;;###autoload
(defun helm-rhythmbox ()
  "Choose a song from the Rhythmbox library to play or enqueue."
  (interactive)
  (helm :sources '(helm-source-rhythmbox-track-search)
        :buffer "*helm-rhythmbox*"))


(provide 'helm-rhythmbox)
;;; helm-rhythmbox.el ends here
