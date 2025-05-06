;;; denote-dired-weblinks.el --- URL bookmark support for Denote files in Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Duncan Britt

;; Author: Duncan Britt
;; Contact: https://github.com/Duncan-Britt/denote-dired-weblinks/issues
;; URL: https://github.com/Duncan-Britt/denote-dired-weblinks
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4") (denote "3.1.0"))
;; Keywords: convenience, files, hypermedia

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; ┌─────────┐
;; │ Summary │
;; └─────────┘
;; This package extends Dired to support web bookmarks as specialized
;; Denote files.  When `denote-dired-weblinks-mode' is enabled,
;; Denote files with the "weblink" keyword are treated specially in
;; Dired: by default, clicking or pressing RET on these files opens
;; the URL contained within them in your browser, rather than opening
;; the file itself.  This behavior can be customized through
;; variables, and the package also provides
;; `denote-dired-weblinks-create-bookmark' to easily create bookmark
;; files from URLs in your clipboard or entered manually.

;; ┌────────────┐
;; │ Motivation │
;; └────────────┘
;; The venerable `denote' Emacs package provides an excellent way to
;; organize and navigate notes on your filesystem such that you can
;; find what you need when you need it.  But the challenge of
;; organizing information such that you may easily retrieve it later
;; applies to more than just your personal notes on your file system.
;;
;; The `denote' system of organizing information via metadata offers a
;; solution, and while it might seem cumbersome to create a file for
;; every web bookmark, some users may like having one workflow for
;; accessing information whether it is stored locally or remotely.
;; This package aims to make it convenient by treating denote files in
;; Dired with the user-defined "weblink" keyword as actionable urls,
;; relieving the need to manually visit the bookmark file, copy the
;; url, open a browser tab, and paste it in.  It also provides a
;; command for creating said bookmark files from a url in the
;; clipboard or entered in the minibuffer.

;; ┌───────────────────────┐
;; │ Example Installations │
;; └───────────────────────┘
;; To use the default settings and create a keybinding for creating
;; bookmarks.  By default, clicking on or pressing enter with the point
;; on a bookmark file in Dired will open the link.
;; (use-package denote-dired-weblinks
;;   :ensure t
;;   :hook (dired-mode . denote-dired-weblinks-mode)
;;   :bind (("s-b" . ddb-create-bookmark)))
;; To disable the shadowing of the open file behavior, set custom
;; variables `denote-dired-weblinks-open-link-on-dired-find-file'
;; and `denote-dired-weblinks-open-link-on-dired-mouse-find-file'.
;; The following example does this and creates an alternate
;; keybinding with which to open bookmark links in Dired.
;; (use-package denote-dired-weblinks
;;   :ensure t
;;   :hook (dired-mode . denote-dired-weblinks-mode)
;;   :custom
;;   (denote-dired-weblinks-open-link-on-dired-find-file nil)
;;   (denote-dired-weblinks-open-link-on-dired-mouse-find-file nil)
;;   :bind (("C-c C-o" . denote-dired-weblinks-open-link)
;;          ("s-b" . denote-dired-weblinks-create-bookmark)))

;;; Code:

(require 'denote)
(require 'dired)

(defvar denote-dired-weblinks-mode nil "To resolve a compilation warning regarding reference to free variable.")

(defgroup denote-dired-weblinks nil
  "Web bookmark support for Denote files in Dired."
  :group 'denote
  :prefix "denote-dired-weblinks-")

(defcustom denote-dired-weblinks-keyword "weblink"
  "Keyword used to identify Denote files containing a URL bookmark."
  :type 'string
  :group 'denote-dired-weblinks)

(defcustom denote-dired-weblinks-open-link-on-dired-find-file t
  "When non-nil, open bookmark links in browser when using `dired-find-file'."
  :type 'boolean
  :group 'denote-dired-weblinks)

(defcustom denote-dired-weblinks-open-link-on-dired-mouse-find-file t
  "When non-nil, open bookmark links in browser on click."
  :type 'boolean
  :group 'denote-dired-weblinks)

(defun denote-dired-weblinks-file-p (file)
  "Return non-nil if FILE is a Denote bookmark file."
  (and (denote-file-is-note-p file)
       (member denote-dired-weblinks-keyword
               (denote-extract-keywords-from-path file))))

(defun denote-dired-weblinks--forward-frontmatter (&optional file)
  "Move point forward past frontmatter in FILE or current buffer.
When called interactively or with FILE, insert file contents into
a temp buffer.  Return the point position after frontmatter."
  (interactive "fFile: ")
  (when (called-interactively-p 'any)
    (let ((content (with-temp-buffer
                     (when file (insert-file-contents file))
                     (buffer-string))))
      (erase-buffer)
      (insert content)))

  (goto-char (point-min))
  (let ((ext (if file
                (file-name-extension file)
              (file-name-extension (or buffer-file-name "")))))
    (cond
     ;; Org mode frontmatter (#+ syntax)
     ((string= ext "org")
      (while (and (not (eobp))
                  (looking-at "^#\\+"))
        (forward-line 1)))

     ;; Markdown with YAML frontmatter (--- delimiters)
     ((and (string= ext "md")
           (looking-at "^---$"))
      (forward-line 1)
      (re-search-forward "^---$" nil t)
      (forward-line 1))

     ;; Markdown with TOML frontmatter (+++ delimiters)
     ((and (string= ext "md")
           (looking-at "^\\+\\+\\+$"))
      (forward-line 1)
      (re-search-forward "^\\+\\+\\+$" nil t)
      (forward-line 1))

     ;; Plain text frontmatter (ends with dashed line)
     ((string= ext "txt")
      (while (and (not (eobp))
                  (not (looking-at "^-\\{10,\\}$")))
        (forward-line 1))
      (when (looking-at "^-\\{10,\\}$")
        (forward-line 1)))))
  (point))

(defun denote-dired-weblinks-extract-url (file)
  "Extract URL from FILE, a Denote bookmark file."
  (with-temp-buffer
    (insert-file-contents file)
    (denote-dired-weblinks--forward-frontmatter file)

    ;; Search for URL in the content
    (let ((limit (min (+ (point) 2000) (point-max))))
      (if (re-search-forward "\\(https?://[^\s\n]+\\)" limit t)
          (match-string 1)
        nil))))

(defun denote-dired-weblinks-open-link (file)
  "Open link after frontmatter in FILE."
  (interactive (list (dired-get-file-for-visit)))
  (if (denote-dired-weblinks-file-p file)
      (let ((url (denote-dired-weblinks-extract-url file)))
        (browse-url url))
    (if (called-interactively-p 'interactive)
        (user-error "Failed to open bookmark at point: No bookmark file at point")
      (message "Error: denote-dired-weblinks-open-link called with non-bookmark file"))
    nil))

(defun denote-dired-weblinks-find-file-advice (orig-fun &rest args)
  "Advice around `dired-find-file' to handle Denote bookmark files.
ORIG-FUN is the original function and ARGS are its arguments."
  (if-let* ((custom-open-links denote-dired-weblinks-open-link-on-dired-find-file)
            (file (dired-get-file-for-visit))
            (is-bookmark (denote-dired-weblinks-file-p file)))
      (denote-dired-weblinks-open-link file)
    (apply orig-fun args)))

(defun denote-dired-weblinks-mouse-find-file-advice (orig-fun &rest args)
  "Advice around `dired-mouse-find-file' to handle Denote bookmark files.
ORIG-FUN is the original function and ARGS are its arguments."
  (if-let* ((custom-open-links denote-dired-weblinks-open-link-on-dired-mouse-find-file)
            (file (dired-get-file-for-visit))
            (is-bookmark (denote-dired-weblinks-file-p file)))
      (denote-dired-weblinks-open-link file)
    (apply orig-fun args)))

(defun denote-dired-weblinks--creation-get-data-from-prompts ()
  "Get Denote data and URL to make a new note.

Similar to `denote--creation-get-note-data-from-prompts'
\(denote version 3.1.0)."
  (let (url title keywords file-type directory date template signature)
    (let ((default-url (current-kill 0)))
      (setq url (read-string (format-prompt "URL" default-url) nil nil default-url)))
    (dolist (prompt denote-prompts)
      (pcase prompt
        ('title (setq title (denote-title-prompt
                             (when (and (not denote-ignore-region-in-denote-command)
                                        (use-region-p))
                               (buffer-substring-no-properties
                                (region-beginning)
                                (region-end))))))
        ('keywords (setq keywords (cons denote-dired-weblinks-keyword (denote-keywords-prompt))))
        ('file-type (unless denote-use-file-type
                      (setq file-type (denote-file-type-prompt))))
        ('subdirectory (unless denote-use-directory
                         (setq directory (denote-subdirectory-prompt))))
        ('date (unless denote-use-date
                 (setq date (denote-date-prompt))))
        ('template (unless denote-use-template
                     (setq template (denote-template-prompt))))
        ('signature (unless denote-use-signature
                      (setq signature (denote-signature-prompt))))))
    (list url title keywords file-type directory date template signature)))

(defun denote-dired-weblinks--propertize-bookmarks ()
  "Add tooltip properties to Denote bookmark files in the current Dired buffer."
  (let ((inhibit-read-only t))  ;; Allow modification of read-only buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (dired-file-name-at-point)
          (let* ((file (dired-get-filename nil t))
                 (url nil))
            (when (and file (denote-dired-weblinks-file-p file))
              (setq url (denote-dired-weblinks-extract-url file))
              (when url
                (let ((beg (line-beginning-position))
                      (end (line-end-position)))
                  (add-text-properties beg end
                                       `(help-echo ,(format "URL: %s" url))))))))
        (forward-line 1)))))

(defun denote-dired-weblinks--after-readin ()
  "Update Dired buffer after readin."
  (when denote-dired-weblinks-mode
    (denote-dired-weblinks--propertize-bookmarks)))

;;;###autoload
(defun denote-dired-weblinks-create-bookmark (&optional url title keywords file-type directory date template signature)
  "Create a new Denote bookmark file for URL with TITLE.
Optional KEYWORDS are additional Denote keywords besides the
bookmark keyword.

See `denote' command documentation for information about
FILE-TYPE, DIRECTORY, DATE, TEMPLATE and SIGNATURE."
  (interactive (denote-dired-weblinks--creation-get-data-from-prompts))
  (let ((orig-buffer (current-buffer)))
    (denote title keywords file-type directory date template signature)
    (insert url)
    (save-buffer)
    (switch-to-buffer orig-buffer)))

;;;###autoload
(define-minor-mode denote-dired-weblinks-mode
  "Minor mode to open Denote bookmark files as URLs in Dired."
  :lighter " DntBkmk"
  :global nil
  (if denote-dired-weblinks-mode
      (progn
        (advice-add 'dired-find-file :around #'denote-dired-weblinks-find-file-advice)
        (advice-add 'dired-mouse-find-file-other-window :around #'denote-dired-weblinks-mouse-find-file-advice)
        (add-hook 'dired-after-readin-hook #'denote-dired-weblinks--after-readin nil t)
        (when (eq major-mode 'dired-mode)
          (denote-dired-weblinks--propertize-bookmarks)))
    (advice-remove 'dired-find-file #'denote-dired-weblinks-find-file-advice)))

(provide 'denote-dired-weblinks)
;;; denote-dired-weblinks.el ends here
