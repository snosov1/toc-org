;;; toc-org.el --- add table of contents to org-mode files (formerly, org-toc)

;; Copyright (C) 2014 Sergei Nosov

;; Author: Sergei Nosov <sergei.nosov [at] gmail.com>
;; Version: 1.1
;; Keywords: org-mode org-toc toc-org org toc table of contents
;; URL: https://github.com/snosov1/toc-org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; toc-org helps you to have an up-to-date table of contents in org or markdown
;; files without exporting (useful primarily for readme files on GitHub).

;; NOTE: Previous name of the package is org-toc. It was changed because of a
;; name conflict with one of the org contrib modules.

;; After installation put into your .emacs file something like

;; (if (require 'toc-org nil t)
;;     (progn
;;       (add-hook 'org-mode-hook 'toc-org-mode)
;;
;;       ;; enable in markdown, too
;;       (add-hook 'markdown-mode-hook 'toc-org-mode)
;;       (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point))
;;   (warn "toc-org not found"))

;; And every time you'll be saving an org file, the first headline with a :TOC:
;; tag will be updated with the current table of contents.

;; For details, see https://github.com/snosov1/toc-org

;;; Code:

(require 'org)
(require 'thingatpt)

(defgroup toc-org nil
  "toc-org is a utility to have an up-to-date table of contents
in the org files without exporting (useful primarily for readme
files on GitHub)"
  :group 'org)

;; just in case, simple regexp "^*.*:toc:\\($\\|[^ ]*:$\\)"
(defconst toc-org-toc-org-regexp ".*?\\(<--\s+\\)?:toc\\([@_][0-9]\\|\\([@_][0-9][@_][a-zA-Z]+\\)\\)?:\\(\\(\s+-->\\)?$\\|[^ ]*?:\\(\s+-->\\)?$\\)"
  "Regexp to find the heading with the :toc: tag.
It misses the heading symbol which must be added depending on the
markup style (org vs markdown).")
(defconst toc-org-quote-tag-regexp ":quote:\\(\\(\s+-->\\)?$\\|[^ ]*?:\\(\s+-->\\)?$\\)"
  "Regexp to find the heading with the :quote: tag")
(defconst toc-org-noexport-regexp "\\(^*+\\)\s+.*:noexport\\([@_][0-9]\\)?:\\($\\|[^ ]*?:$\\)"
  "Regexp to find the extended version of :noexport: tag")
(defconst toc-org-tags-regexp "\s*:[[:word:]:@_]*:\s*$"
  "Regexp to find tags on the line")
(defconst toc-org-todo-custom-keywords-regexp "^#\\+\\(TODO\\|SEQ_TODO\\|TYP_TODO\\):\\(.*\\)$"
  "Regexp to find custom TODO keywords")
(defconst toc-org-COMMENT-regexp "\\(^*+\\)\s+\\(COMMENT\s+\\)"
  "Regexp to find COMMENT headlines")
(defconst toc-org-priorities-regexp "^*+\s+\\(\\[#.\\]\s+\\)"
  "Regexp to find states on the line")
(defconst toc-org-links-regexp "\\[\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]"
  "Regexp to find states on the line")
(defconst toc-org-special-chars-regexp "[^[:alnum:]_-]"
  "Regexp with the special characters (which are omitted in hrefs
  by GitHub)")
(defconst toc-org-statistics-cookies-regexp "\s*\\[[0-9]*\\(%\\|/[0-9]*\\)\\]\s*"
  "Regexp to find statistics cookies on the line")
(defconst toc-org-leave-todo-regexp "^#\\+OPTIONS:.*\stodo:t[\s\n]"
  "Regexp to find the todo export setting")
(defconst toc-org-drawer-regexp "^[ 	]*:\\(\\(?:\\w\\|[-_]\\)+\\):[ 	]*$"
  "Regexp to match org drawers. Note: generally, it should be
equal to `org-drawer-regexp'. However, some older versions of
org (notably, 8.2.10) restrict the values that can be placed
between the colons. So, the value here is set explicitly.")
(defconst toc-org-markdown-link-regexp ;; copy-paste from markdown-mode
  "\\(!\\)?\\(\\[\\)\\([^]^][^]]*\\|\\)\\(\\]\\)\\((\\)\\([^)]*?\\)\\(?:\\s-+\\(\"[^\"]*\"\\)\\)?\\()\\)"
  "Regular expression for a [text](file) or an image link ![text](file).
Group 1 matches the leading exclamation point (optional).
Group 2 matches the opening square bracket.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket.
Group 5 matches the opening parenthesis.
Group 6 matches the URL.
Group 7 matches the title (optional).
Group 8 matches the closing parenthesis.")

(defcustom toc-org-max-depth 2
  "Maximum depth of the headings to use in the table of
contents. The default of 2 uses only the highest level headings
and their subheadings (one and two stars)."
  :type 'integer
  :group 'toc-org)

(defcustom toc-org-hrefify-default "gh"
  "Default hrefify function to use."
  :type 'string
  :group 'toc-org)

(defcustom toc-org-enable-links-opening t
  "With this option, org-open-at-point (C-c C-o) should work on
the TOC links (even if the style is different from org)."
  :type 'boolean
  :group 'toc-org)

(defvar-local toc-org-hrefify-hash nil
  "Buffer local hash-table that is used to enable links
opening. The keys are hrefified headings, the values are original
headings.")

(defcustom toc-org-side-window-side 'left
  "Side of the frame where the TOC side window is displayed.
Possible values are `left', `right', `top', and `bottom'."
  :type '(choice (const left)
                 (const right)
                 (const top)
                 (const bottom))
  :group 'toc-org)

(defcustom toc-org-side-window-size 35
  "Size of the TOC side window.
This controls the width for `left'/`right' sides, and the height
for `top'/`bottom' sides.  An integer means the number of columns
or lines; a float between 0 and 1 means a fraction of the frame
size (e.g. 0.3 for 30%)."
  :type 'number
  :group 'toc-org)

(defconst toc-org-navigation-pane-buffer-name "*toc-org-navigation-pane*"
  "Name of the buffer used for the TOC navigation pane.")

(defvar-local toc-org--toc-buffer nil
  "TOC navigation pane buffer associated with this buffer.")

(defvar-local toc-org--nav-source-buffer nil
  "Source buffer associated with this navigation pane buffer.")

(defvar-local toc-org--nav-max-depth nil
  "Max depth shown in this navigation pane, or nil to use the default.")

(defun toc-org-raw-toc (markdown-syntax-p)
  "Return the \"raw\" table of contents of the current file,
i.e. simply flush everything that's not a heading and strip
auxiliary text."
  (let ((content (buffer-substring-no-properties
                  (point-min) (point-max)))
        (leave-states-p nil)
        (custom-keywords nil)
        (toc-org-states-regexp ""))
    (with-temp-buffer
      (insert content)

      ;; preprocess markdown-style headings
      (when markdown-syntax-p
        (save-excursion
          (let ((case-fold-search t))
            ;; remove ``` code fence blocks so their # lines aren't treated as headings
            (goto-char (point-min))
            (while (re-search-forward "^```" nil t)
              (let ((beg (line-beginning-position)))
                (when (re-search-forward "^```" nil t)
                  (forward-line 1)
                  (delete-region beg (point)))))
            (goto-char (point-min))
            (while (re-search-forward "^#+ " nil t)
              (replace-match (concat
                              (make-string (1- (length (match-string 0))) ?*)
                              " ") nil nil))
            (goto-char (point-min))
            (while (re-search-forward "\s+#+$" nil t)
              (replace-match "" nil nil))
            (goto-char (point-min))
            (while (re-search-forward "\\(^*.*\\)<-- \\(:toc[^ ]*:\\) -->\\($\\)" nil t)
              (replace-match (concat (match-string 1) (match-string 2) (match-string 3)) nil nil)))))

      ;; set leave-states-p variable
      (goto-char (point-min))
      (when (re-search-forward toc-org-leave-todo-regexp nil t)
        (setq leave-states-p t))

      ;; set toc-org-states-regexp (after collecting custom keywords)
      (goto-char (point-min))
      (while (re-search-forward toc-org-todo-custom-keywords-regexp nil t)
        (setq custom-keywords (append custom-keywords (split-string (match-string 2) "[ \f\t\n\r\v|]+" t))))
      (if custom-keywords
          (setq toc-org-states-regexp
                (concat "^*+\s+\\("
                        (mapconcat (lambda (x) (replace-regexp-in-string "(.+?)" "" x))
                                   custom-keywords "\s+\\|")
                        "\s+\\)"))
        (setq toc-org-states-regexp "^*+\s+\\(TODO\s+\\|DONE\s+\\)"))

      ;; keep only lines starting with *s
      (goto-char (point-min))
      (keep-lines "^\*+[ ]")

      ;; don't include the TOC itself
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\*" toc-org-toc-org-regexp) nil t)
        (beginning-of-line)
        (delete-region (point) (progn (forward-line 1) (point))))

      ;; strip states
      (unless leave-states-p
        (goto-char (point-min))
        (while (re-search-forward toc-org-states-regexp nil t)
          (replace-match "" nil nil nil 1)))

      ;; strip COMMENT headlines
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (re-search-forward toc-org-COMMENT-regexp nil t)
          (let ((skip-depth (concat (match-string 1) "*")))
            (while (progn
                     (beginning-of-line)
                     (delete-region (point) (min (1+ (line-end-position)) (point-max)))
                     (string-prefix-p skip-depth (or (current-word) "")))))))

      ;; strip headings with :noexport: tag
      (goto-char (point-min))
      (while (re-search-forward toc-org-noexport-regexp nil t)
        (save-excursion
          (let* ((tag  (match-string 2))
                 (depth (if tag (string-to-number (substring tag 1)) 0))
                 (subheading-depth (concat (match-string 1) "*"))
                 (skip-depth (concat subheading-depth (make-string (max (1- depth) 0) ?*))))
            (if (> depth 0)
                (forward-line)
              (beginning-of-line)
              (delete-region (point) (min (1+ (line-end-position)) (point-max))))
            (while (string-prefix-p subheading-depth (or (current-word) ""))
              (if (string-prefix-p skip-depth (or (current-word) ""))
                  (progn
                    (beginning-of-line)
                    (delete-region (point) (min (1+ (line-end-position)) (point-max))))
                (forward-line))))))

      ;; strip priorities
      (goto-char (point-min))
      (while (re-search-forward toc-org-priorities-regexp nil t)
        (replace-match "" nil nil nil 1))

      ;; strip tags
      (goto-char (point-min))
      (while (re-search-forward toc-org-tags-regexp nil t)
        (replace-match "" nil nil))

      ;; flatten links
      (goto-char (point-min))
      (while (re-search-forward toc-org-links-regexp nil t)
        (replace-match "\\2" nil nil))

      (buffer-substring-no-properties
       (point-min) (point-max)))))

(defun toc-org-hrefify-gh (str &optional hash)
  "Given a heading, transform it into a href using the GitHub
rules."
  (let* ((spc-fix (replace-regexp-in-string " " "-" str))
         (upcase-fix (downcase spc-fix))
         (special-chars-fix (replace-regexp-in-string toc-org-special-chars-regexp "" upcase-fix t))
         (hrefified-base (concat "#" special-chars-fix))
         (hrefified hrefified-base)
         (idx 0))
    ;; try appending -1, -2, -3, etc. until unique href is found
    (when hash
      (while (gethash hrefified hash)
        (setq hrefified
              (concat hrefified-base "-" (number-to-string (setq idx (1+ idx)))))))
    hrefified))

(defun toc-org-format-visible-link (str)
  "Formats the visible text of the link."
  (with-temp-buffer
    (insert str)

    ;; strip statistics cookies
    (goto-char (point-min))
    (while (re-search-forward toc-org-statistics-cookies-regexp nil t)
      (replace-match "" nil nil))
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun toc-org-hrefify-org (str &optional hash)
  "Given a heading, transform it into a href using the org-mode
rules."
  (toc-org-format-visible-link str))

(defun toc-org-unhrefify (type path)
  "Looks for a value in toc-org-hrefify-hash using path as a key."
  (let ((ret-type type)
        (ret-path path)
        (original-path (and (not (eq toc-org-hrefify-hash nil))
                            (gethash
                             (concat
                              ;; Org 8.3 and above provides type as "custom-id"
                              ;; and strips the leading hash symbol
                              (if (equal type "custom-id") "#" "")
                              (substring-no-properties path))
                             toc-org-hrefify-hash
                             nil))))
    (when toc-org-enable-links-opening
      (when original-path
        ;; Org 8.2 and below provides type as "thisfile"
        (when (equal type "thisfile")
          (setq ret-path original-path))
        (when (equal type "custom-id")
          (setq ret-type "fuzzy")
          (setq ret-path original-path))))
    (cons ret-type ret-path)))

(defun toc-org-hrefify-toc (toc hrefify markdown-syntax-p &optional hash)
  "Format the raw `toc' using the `hrefify' function to transform
each heading into a link."
  (with-temp-buffer
    (insert toc)
    (goto-char (point-min))
    (while
        (progn
          (when (looking-at "\\*")
            (delete-char 1)

            (while (looking-at "\\*")
              (delete-char 1)
              (insert (make-string
                       (+ 2 (or (bound-and-true-p org-list-indent-offset) 0))
                       ?\s)))

            (insert "-")
            (skip-chars-forward " ")

	    (save-excursion
	      (delete-trailing-whitespace (point) (line-end-position)))

            (let* ((beg (point))
                   (end (line-end-position))
                   (heading (buffer-substring-no-properties
                             beg end))
                   (hrefified (funcall hrefify heading hash))
		   (visible-link (toc-org-format-visible-link heading)))

              (if markdown-syntax-p
                  (progn
                    (insert "[")
                    (insert visible-link)
                    (delete-region (point) (line-end-position))
                    (insert "]")
                    (insert "(")
                    (insert hrefified)
                    (insert ")"))
                (insert "[[")
                (insert hrefified)
                (insert "][")
                (insert visible-link)
                (delete-region (point) (line-end-position))
                (insert "]]"))

              ;; maintain the hash table, if provided
              (when hash
                (puthash hrefified visible-link hash)))
            (= 0 (forward-line 1)))))

    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun toc-org-flush-subheadings (toc max-depth)
  "Flush subheadings of the raw `toc' deeper than `max-depth'."
  (with-temp-buffer
    (insert toc)
    (goto-char (point-min))

    (let ((re "^"))
      (dotimes (i (1+ max-depth))
        (setq re (concat re "\\*")))
      (flush-lines re))

    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun toc-org-insert-toc (&optional dry-run)
  "Update table of contents in heading tagged :TOC:.

When DRY-RUN is non-nil, the buffer is not modified, only the
internal hash-table is updated to enable `org-open-at-point' for
TOC links.

The table of contents heading may also be set with these tags:

- :TOC_#: Sets the maximum depth of the headlines in the table of
          contents to the number given, e.g. :TOC_3: for
          3 (default for plain :TOC: tag is 2).

- :TOC_#_gh: Sets the maximum depth as above and also uses
             GitHub-style anchors in the table of contents (the
             default).  The other supported style is :TOC_#_org:,
             which is the default org style.

Headings may be excluded from the TOC with these tags:

- :noexport: Exclude this heading.

- :noexport_#: Exclude this heading's children with relative
               level greater than number given (e.g. :noexport_1:
               causes all child headings to be excluded).

Note that :noexport: is also used by Org-mode's exporter, but
not :noexport_#:."

  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((case-fold-search t)
           (markdown-syntax-p (derived-mode-p 'markdown-mode))
           (heading-symbol-regexp (if markdown-syntax-p "^#" "^\\*")))
      ;; find the first heading with the :TOC: tag
      (when (re-search-forward (concat heading-symbol-regexp toc-org-toc-org-regexp) (point-max) t)
        (let* ((tag (match-string 2))
               (depth (if tag
                          (- (aref tag 1) ?0) ;; is there a better way to convert char to number?
                        toc-org-max-depth))
               (hrefify-tag (if (and tag (>= (length tag) 4))
                                (downcase (substring tag 3))
                              toc-org-hrefify-default))
               (hrefify-string (concat "toc-org-hrefify-" hrefify-tag))
               (hrefify (intern-soft hrefify-string))
               (put-quote (save-match-data (string-match toc-org-quote-tag-regexp (match-string 0))))
               (toc-prefix (if put-quote (if markdown-syntax-p "```\n" "#+BEGIN_QUOTE\n")  ""))
               (toc-suffix (if put-quote (if markdown-syntax-p "```\n" "#+END_QUOTE\n") "")))
          (if hrefify
              (let ((new-toc
                     (concat toc-prefix
                             (toc-org-hrefify-toc
                              (toc-org-flush-subheadings (toc-org-raw-toc markdown-syntax-p) depth)
                              hrefify
                              markdown-syntax-p
                              (when toc-org-hrefify-hash
                                (clrhash toc-org-hrefify-hash)))
                             toc-suffix)))
                (unless dry-run
                  (newline (forward-line 1))

                  ;; skip drawers
                  (let ((end
                         (save-excursion ;; limit to next heading
                           (search-forward-regexp heading-symbol-regexp (point-max) 'skip))))
                    (while (re-search-forward toc-org-drawer-regexp end t)
                      (skip-chars-forward "[:space:]")))
                  (beginning-of-line)

                  ;; insert newline if TOC is currently empty
                  (when (looking-at heading-symbol-regexp)
                    (open-line 1))

                  ;; find TOC boundaries
                  (let ((beg (point))
                        (end
                         (save-excursion
                           (when (search-forward-regexp heading-symbol-regexp (point-max) 'skip)
                             (forward-line -1))
                           (end-of-line)
                           (point))))
                    ;; update the TOC, but only if it's actually different
                    ;; from the current one
                    (unless (equal
                             (buffer-substring-no-properties beg end)
                             new-toc)
                      (delete-region beg end)
                      (insert new-toc)))))
            (message (concat "Hrefify function " hrefify-string " is not found"))))))))

(defun toc-org-follow-markdown-link ()
  "Follow the markdown link (mimics `org-open-at-point')"
  (interactive)
  (when (thing-at-point-looking-at toc-org-markdown-link-regexp)
    (let ((pos (point)))
      (goto-char (point-min))
      (if (re-search-forward (concat "^#+\s+" (match-string-no-properties 3)) (point-max) t)
          (beginning-of-line)
        (goto-char pos)))))

(defun toc-org-markdown-follow-thing-at-point (arg)
  "Try to follow the link with `toc-org-follow-markdown-link',
fallback to `markdown-follow-thing-at-point' on failure"
  (interactive "P")
  (let ((pos (point)))
    (toc-org-follow-markdown-link)
    (when (and (equal pos (point)) (fboundp 'markdown-follow-thing-at-point))
      (markdown-follow-thing-at-point arg))))

;;;###autoload
(defun toc-org-enable ()
  "Enable toc-org in this buffer."
  (add-hook 'before-save-hook 'toc-org-insert-toc nil t)

  ;; conservatively set org-link-translation-function
  (when (and (equal toc-org-enable-links-opening t)
             (or
              (not (fboundp org-link-translation-function))
              (equal org-link-translation-function 'toc-org-unhrefify)))
    (setq toc-org-hrefify-hash (make-hash-table :test 'equal))
    (setq org-link-translation-function 'toc-org-unhrefify)
    (toc-org-insert-toc t)))

(defvar toc-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") #'toc-org-navigation-pane)
    map)
  "Keymap for `toc-org-mode'.")

;;;###autoload
(define-minor-mode toc-org-mode
  "Toggle `toc-org' in this buffer."
  :group toc-org
  :keymap toc-org-mode-map
  (if toc-org-mode
      (toc-org-enable)
    (remove-hook 'before-save-hook 'toc-org-insert-toc t)
    ;; we would've set `org-link-translation-function' only if it's been nil
    (when (equal org-link-translation-function 'toc-org-unhrefify)
      (setq org-link-translation-function nil))))

(defun toc-org--effective-max-depth ()
  "Return the effective TOC max depth.
Uses the depth from the :TOC_N: tag if present, else `toc-org-max-depth'."
  (save-excursion
    (goto-char (point-min))
    (let* ((case-fold-search t)
           (heading-re (if (derived-mode-p 'markdown-mode) "^#" "^\\*")))
      (if (re-search-forward (concat heading-re toc-org-toc-org-regexp) nil t)
          (let ((tag (match-string 2)))
            (if tag (- (aref tag 1) ?0) toc-org-max-depth))
        toc-org-max-depth))))

(defun toc-org--close-toc-pane ()
  "Close TOC side windows whose source buffers are no longer visible."
  (let ((any-toc-p nil))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and toc-org--toc-buffer
                     (buffer-live-p toc-org--toc-buffer))
            (if (get-buffer-window buf)
                (setq any-toc-p t)
              (kill-buffer toc-org--toc-buffer)
              (setq toc-org--toc-buffer nil)
              (remove-hook 'after-save-hook #'toc-org--refresh-navigation-pane t))))))
    (unless any-toc-p
      (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane))))

(defun toc-org--close-toc-pane-on-kill ()
  "Close the TOC side window when the source buffer is killed."
  (when (and toc-org--toc-buffer
             (buffer-live-p toc-org--toc-buffer))
    (kill-buffer toc-org--toc-buffer)
    (setq toc-org--toc-buffer nil))
  (remove-hook 'after-save-hook #'toc-org--refresh-navigation-pane t))

(defun toc-org--refresh-navigation-pane (&optional max-depth)
  "Refresh the TOC navigation pane content for the current source buffer."
  (when (and toc-org--toc-buffer
             (buffer-live-p toc-org--toc-buffer))
    (let* ((source-buf   (current-buffer))
           (toc-buf      toc-org--toc-buffer)
           (source-file  (buffer-file-name source-buf))
           (markdown-p   (derived-mode-p 'markdown-mode))
           (raw-toc      (toc-org-flush-subheadings
                          (toc-org-raw-toc markdown-p)
                          (or max-depth
                              (with-current-buffer toc-org--toc-buffer
                                                   toc-org--nav-max-depth)
                              (toc-org--effective-max-depth))))
           (win          (get-buffer-window toc-buf))
           (saved-point  (when win (window-point win))))
      (with-current-buffer toc-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (line (split-string raw-toc "\n" t))
            (when (string-match "^\\(\\*+\\)[ \t]+\\(.*\\)" line)
              (let* ((depth    (1- (length (match-string 1 line))))
                     (title    (replace-regexp-in-string
                                 toc-org-statistics-cookies-regexp ""
                                 (match-string 2 line)))
                     (indent   (make-string (* 2 depth) ?\s))
                     (line-num (with-current-buffer source-buf
                                 (save-excursion
                                   (goto-char (point-min))
                                   (when (re-search-forward
                                          (concat (if markdown-p
                                                      "^#+[ \t]+"
                                                    "^\\*+[ \t]+\\(?:[A-Z]+[ \t]+\\)?\\(?:\\[#.\\][ \t]+\\)?")
                                                  (regexp-quote title))
                                          nil t)
                                     (line-number-at-pos))))))
                (when line-num
                  (insert indent "- "
                          (format "[[file:%s::%d][%s]]"
                                  source-file line-num title)
                          "\n")))))
          (if (and win saved-point)
              (set-window-point win (min saved-point (point-max)))
            (goto-char (point-min))))))))

(defun toc-org--goto-current-heading (&optional max-depth)
  "In the TOC buffer, position point at the entry for the heading at point."
  (when (and toc-org--toc-buffer
             (buffer-live-p toc-org--toc-buffer))
    (let* ((toc-buf    toc-org--toc-buffer)
           (markdown-p (derived-mode-p 'markdown-mode))
           (heading-re (if markdown-p "^\\(#+\\)[ \t]+" "^\\(\\*+\\)[ \t]+"))
           (max-depth  (or max-depth (toc-org--effective-max-depth)))
           (line-num
            (save-excursion
              (end-of-line)
              (catch 'found
                (while (re-search-backward heading-re nil t)
                  (when (<= (length (match-string 1)) max-depth)
                    (throw 'found (line-number-at-pos))))))))
      (when line-num
        (with-current-buffer toc-buf
          (goto-char (point-min))
          (when (re-search-forward (format "::%d\\]\\[" line-num) nil t)
            (beginning-of-line)))))))

(defun toc-org--follow-link ()
  "Follow the first org link on the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "\\[\\[" (line-end-position) t)
      (org-open-at-point))))

(defun toc-org--nav-show-heading ()
  "Move point in the source buffer to the heading on the current nav-pane line."
  (when (and toc-org--nav-source-buffer
             (buffer-live-p toc-org--nav-source-buffer))
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "::\\([0-9]+\\)\\]\\[" (line-end-position) t)
        (let ((line-num (string-to-number (match-string 1)))
              (source-win (get-buffer-window toc-org--nav-source-buffer)))
          (when source-win
            (with-selected-window source-win
              (goto-char (point-min))
              (forward-line (1- line-num))
              (recenter))))))))

(defun toc-org--nav-next ()
  "Move to next nav pane entry and show it in the source buffer."
  (interactive)
  (forward-line 1)
  (toc-org--nav-show-heading))

(defun toc-org--nav-prev ()
  "Move to previous nav pane entry and show it in the source buffer."
  (interactive)
  (forward-line -1)
  (toc-org--nav-show-heading))

(defun toc-org--max-heading-depth ()
  "Return the maximum heading depth present in the current buffer's TOC."
  (let ((raw (toc-org-raw-toc (derived-mode-p 'markdown-mode)))
        (depth 0))
    (dolist (line (split-string raw "\n" t))
      (when (string-match "^\\(\\*+\\)" line)
        (setq depth (max depth (length (match-string 1 line))))))
    depth))

(defun toc-org--nav-change-depth (delta)
  "Change the navigation pane max depth by DELTA and refresh."
  (when (and toc-org--nav-source-buffer
             (buffer-live-p toc-org--nav-source-buffer))
    (let* ((current (or toc-org--nav-max-depth
                        (with-current-buffer toc-org--nav-source-buffer
                          (toc-org--effective-max-depth))))
           (max-possible (with-current-buffer toc-org--nav-source-buffer
                           (toc-org--max-heading-depth)))
           (new-depth (max 1 (min (+ current delta) max-possible))))
      (setq toc-org--nav-max-depth new-depth)
      (with-current-buffer toc-org--nav-source-buffer
        (toc-org--refresh-navigation-pane new-depth)))))

(defun toc-org--nav-increase-depth ()
  "Increase the max depth shown in the navigation pane."
  (interactive)
  (toc-org--nav-change-depth 1))

(defun toc-org--nav-decrease-depth ()
  "Decrease the max depth shown in the navigation pane."
  (interactive)
  (toc-org--nav-change-depth -1))

;;;###autoload
(defun toc-org-navigation-pane ()
  "Show the table of contents of the current buffer as a side pane.

Works as a toggle: calling it again closes the pane.

The TOC buffer is read-only with these single-letter shortcuts:
  n / p           next / previous line
  f / b           forward / backward character
  k               close the pane
  RET             follow the link on the current line
  M-n / M-<down>  move to next entry and show it in the source buffer
  M-p / M-<up>    move to previous entry and show it in the source buffer
  = / +           increase max depth
  -               decrease max depth

Customize `toc-org-side-window-side' to set which side the pane
appears on (left, right, top, or bottom).  Customize
`toc-org-side-window-size' to set the width or height: an integer
for a fixed number of columns/lines, or a float (0.0–1.0) for a
fraction of the frame size."
  (interactive)
  (cond
   ((string= (buffer-name) toc-org-navigation-pane-buffer-name)
    (kill-buffer-and-window))
   ((and toc-org--toc-buffer
         (buffer-live-p toc-org--toc-buffer)
         (get-buffer-window toc-org--toc-buffer))
    (kill-buffer toc-org--toc-buffer)
    (setq toc-org--toc-buffer nil)
    (remove-hook 'kill-buffer-hook #'toc-org--close-toc-pane-on-kill t)
    (remove-hook 'after-save-hook  #'toc-org--refresh-navigation-pane t))
   (t
    (let* ((source-buf  (current-buffer))
           (source-file (buffer-file-name source-buf))
           (max-depth   (toc-org--effective-max-depth))
           (win-buf     (get-buffer-create toc-org-navigation-pane-buffer-name)))
      (if (not source-file)
          (progn
            (kill-buffer win-buf)
            (message "toc-org: Buffer is not visiting a file."))
        (with-current-buffer win-buf
          (org-mode)
          (display-line-numbers-mode -1)
          (setq-local mode-line-format nil)
          (setq-local buffer-read-only t)
          (setq-local toc-org--nav-source-buffer source-buf)
          (setq-local header-line-format
                      (format " Table of Contents - %s" (buffer-name source-buf)))
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map (current-local-map))
            (define-key map "n" #'next-line)
            (define-key map "p" #'previous-line)
            (define-key map "f" #'forward-char)
            (define-key map "b" #'backward-char)
            (define-key map "k" #'kill-buffer-and-window)
            (define-key map (kbd "RET") #'toc-org--follow-link)
            (define-key map (kbd "M-n") #'toc-org--nav-next)
            (define-key map (kbd "M-p") #'toc-org--nav-prev)
            (define-key map (kbd "M-<down>") #'toc-org--nav-next)
            (define-key map (kbd "M-<up>") #'toc-org--nav-prev)
            (define-key map "=" #'toc-org--nav-increase-depth)
            (define-key map "+" #'toc-org--nav-increase-depth)
            (define-key map "-" #'toc-org--nav-decrease-depth)
            (use-local-map map)))
        (with-current-buffer source-buf
          (setq toc-org--toc-buffer win-buf)
          (add-hook 'kill-buffer-hook  #'toc-org--close-toc-pane-on-kill nil t)
          (add-hook 'after-save-hook   #'toc-org--refresh-navigation-pane nil t)
          (toc-org--refresh-navigation-pane max-depth)
          ;; add after refresh so with-temp-buffer inside toc-org-raw-toc
          ;; doesn't fire the hook before the pane window is shown
          (add-hook 'buffer-list-update-hook #'toc-org--close-toc-pane))
        (if (with-current-buffer win-buf (zerop (buffer-size)))
            (progn
              (kill-buffer win-buf)
              (setq toc-org--toc-buffer nil)
              (remove-hook 'kill-buffer-hook #'toc-org--close-toc-pane-on-kill t)
              (remove-hook 'after-save-hook  #'toc-org--refresh-navigation-pane t)
              (message "toc-org: No headings found in this buffer."))
          (toc-org--goto-current-heading max-depth)
          (select-window
           (display-buffer
            win-buf
            (cons 'display-buffer-in-side-window
                  (list (cons 'side          toc-org-side-window-side)
                        (cons 'window-width  toc-org-side-window-size)
                        (cons 'window-height toc-org-side-window-size)
                        '(slot . 0)))))))))))

;; Local Variables:
;; compile-command: "emacs -batch -l ert -l toc-org.el -l toc-org-test.el -f ert-run-tests-batch-and-exit && emacs -batch -f batch-byte-compile toc-org.el 2>&1 | sed -n '/Warning\|Error/p' | xargs -r ls"
;; End:

(provide 'toc-org)
;;; toc-org.el ends here
