;;; org-toc.el --- add table of contents to org-mode files

;; Copyright (C) 2014 Sergei Nosov

;; Author: Sergei Nosov <sergei.nosov [at] gmail.com>
;; Version: 1.0
;; Keywords: org-mode org toc table of contents
;; URL: https://github.com/snosov1/org-toc

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; org-toc helps you to have an up-to-date table of contents in org files
;; without exporting (useful primarily for readme files on GitHub).

;; After installation put into your .emacs file something like

;; (eval-after-load "org-toc-autoloads"
;;   '(progn
;;      (if (require 'org-toc nil t)
;;          (add-hook 'org-mode-hook 'org-toc-enable)
;;        (warn "org-toc not found"))))

;; And every time you'll be saving an org file, the first headline with a :TOC:
;; tag will be updated with the current table of contents.

;; For details, see https://github.com/snosov1/org-toc

;;; Code:

(require 'ert)

(defgroup org-toc nil
  "org-toc is a utility to have an up-to-date table of contents
in the org files without exporting (useful primarily for readme
files on GitHub)"
  :group 'org)

;; just in case, simple regexp "^*.*:toc:\\($\\|[^ ]*:$\\)"
(defconst org-toc-org-toc-regexp "^*.*:toc\\([@_][0-9]\\|\\([@_][0-9][@_][a-zA-Z]+\\)\\)?:\\($\\|[^ ]*:$\\)"
  "Regexp to find the heading with the :toc: tag")
(defconst org-toc-tags-regexp "\s*:[[:word:]:@]*:\s*$"
  "Regexp to find tags on the line")
(defconst org-toc-special-chars-regexp "[][~`!@#$%^&*()+={}|\:;\"'<,>.?/]"
  "Regexp with the special characters (which are omitted in hrefs
  by GitHub)")

(defcustom org-toc-max-depth 2
  "Maximum depth of the headings to use in the table of
contents. The default of 2 uses only the highest level headings
and their subheadings (one and two stars)."
  :group 'org-toc)

(defcustom org-toc-hrefify-default "gh"
  "Default hrefify function to use."
  :group 'org-toc)

(defun org-toc-raw-toc ()
  "Return the \"raw\" table of contents of the current file,
i.e. simply flush everything that's not a heading and strip
tags."
  (let ((content (buffer-substring-no-properties
                  (point-min) (point-max))))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (keep-lines "^\*+[ ]")

      ;; don't include the TOC itself
      (goto-char (point-min))
      (re-search-forward org-toc-org-toc-regexp)
      (beginning-of-line)
      (delete-region (point) (progn (forward-line 1) (point)))

      ;; strip tags

      ;; TODO :export: and :noexport: tags semantic should be probably
      ;; implemented
      (goto-char (point-min))
      (while (re-search-forward org-toc-tags-regexp nil t)
        (replace-match "" nil nil))

      (buffer-substring-no-properties
       (point-min) (point-max)))))

(ert-deftest org-toc-test-raw-toc ()
  "Test the `org-toc-raw-toc' function"

  (defun org-toc-test-raw-toc-gold-test (content gold)
    (should (equal
             (with-temp-buffer
               (insert content)
               (org-toc-raw-toc))
             gold)))
  (declare-function org-toc-test-raw-toc-gold-test "org-toc") ;; suppress compiler warning

  (let ((beg "* About\n:TOC:\n drawer\n:END:\n\norg-toc is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n\n* Table of Contents                                                     ")
        (gold "* About\n"))

    ;; different TOC styles
    (org-toc-test-raw-toc-gold-test (concat beg ":TOC:"         ) gold)
    (org-toc-test-raw-toc-gold-test (concat beg ":TOC_1:"       ) gold)
    (org-toc-test-raw-toc-gold-test (concat beg ":TOC_1_qqq:"   ) gold)
    (org-toc-test-raw-toc-gold-test (concat beg ":TOC@1:"       ) gold)
    (org-toc-test-raw-toc-gold-test (concat beg ":TOC@1@cxv:"   ) gold)
    (org-toc-test-raw-toc-gold-test (concat beg ":TOC@1_hello:" ) gold)

    ;; trailing symbols
    (org-toc-test-raw-toc-gold-test (concat beg ":TOC@1_hello:" "\n\n\n") gold)
    (org-toc-test-raw-toc-gold-test (concat beg ":TOC@1_hello:" "\n\n\nsdfd") gold))

  ;; more complex case
  (org-toc-test-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\norg-toc is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n* Installation\n** via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"org-toc-autoloads\"\n    '(progn\n       (if (require 'org-toc nil t)\n           (add-hook 'org-mode-hook 'org-toc-enable)\n         (warn \"org-toc not found\"))))\n#+END_SRC\n** Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n- Go to it and clone org-toc there\n  #+BEGIN_SRC sh\n    git clone https://github.com/snosov1/org-toc.git\n  #+END_SRC\n- Put this in your ~/.emacs file\n  #+BEGIN_SRC elisp\n    (add-to-list 'load-path \"~/.emacs.d/org-toc\")\n    (when (require 'org-toc nil t)\n      (add-hook 'org-mode-hook 'org-toc-enable))\n  #+END_SRC\n\n* Use\n\nAfter the installation, every time you'll be saving an org file, the\nfirst headline with a :TOC: tag will be updated with the current table\nof contents.\n\nTo add a TOC tag, you can use the command =org-set-tags-command=.\n\nIn addition to the simple :TOC: tag, you can also use the following\ntag formats:\n\n- :TOC@2: - sets the max depth of the headlines in the table of\n  contents to 2 (the default)\n\n- :TOC@2@gh: - sets the max depth as in above and also uses the\n  GitHub-style hrefs in the table of contents (the default). The other\n  supported href style is 'org', which is the default org style (you\n  can use C-c C-o to go to the headline at point).\n\nYou can also use =_= as separator, instead of =@=.\n\n* Different href styles\n\nCurrently, only 2 href styles are supported: =gh= and =org=. You can easily\ndefine your own styles. If you use the tag =:TOC@2@STYLE:= (=STYLE= being a\nstyle name), then the package will look for a function named\n=org-toc-hrefify-STYLE=, which accepts a heading string and returns a href\ncorresponding to that heading.\n\nE.g. for =org= style it simply returns input as is:\n\n#+BEGIN_SRC emacs-lisp\n  (defun org-toc-hrefify-org (str)\n    \"Given a heading, transform it into a href using the org-mode\n  rules.\"\n    str)\n#+END_SRC\n\n* Example\n\n#+BEGIN_SRC org\n  * About\n  * Table of Contents                                           :TOC:\n    - [[#about][About]]\n    - [[#installation][Installation]]\n        - [[#via-packageel][via package.el]]\n        - [[#manual][Manual]]\n    - [[#use][Use]]\n  * Installation\n  ** via package.el\n  ** Manual\n  * Use\n  * Example\n#+END_SRC\n"
   "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n"))

(defun org-toc-hrefify-gh (str)
  "Given a heading, transform it into a href using the GitHub
rules."
  (let* ((spc-fix (replace-regexp-in-string " " "-" str))
         (upcase-fix (replace-regexp-in-string "[A-Z]" 'downcase spc-fix t))
         (special-chars-fix (replace-regexp-in-string org-toc-special-chars-regexp "" upcase-fix t)))
    (concat "#" special-chars-fix)))

(ert-deftest org-toc-test-hrefify-gh ()
  "Test the `org-toc-hrefify-gh' function"
  (should (equal (org-toc-hrefify-gh "About") "#about"))
  (should (equal (org-toc-hrefify-gh "!h@#$%^&*(){}|][:;\"'/?.>,<`~") "#h"))
  (should (equal (org-toc-hrefify-gh "!h@#$% ^&*(S){}|][:;\"'/?.>,<`~") "#h-s")))

(defun org-toc-hrefify-org (str)
  "Given a heading, transform it into a href using the org-mode
rules."
  str)

(defun org-toc-hrefify-toc (toc hrefify)
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
              (insert "    "))

            (skip-chars-forward " ")
            (insert "- ")

            (let* ((beg (point))
                   (end (line-end-position))
                   (heading (buffer-substring-no-properties
                             beg end)))
              (insert "[[")
              (insert (funcall hrefify heading))
              (insert "][")
              (end-of-line)
              (insert "]]"))
            (= 0 (forward-line 1)))))

    (buffer-substring-no-properties
     (point-min) (point-max))))

(ert-deftest org-toc-test-hrefify-toc ()
  (should (equal (org-toc-hrefify-toc "* About\n" 'upcase)
                 " - [[ABOUT][About]]\n"))
  (should (equal (org-toc-hrefify-toc "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 'upcase)
                 " - [[ABOUT][About]]\n - [[INSTALLATION][Installation]]\n     - [[VIA PACKAGE.EL][via package.el]]\n     - [[MANUAL][Manual]]\n - [[USE][Use]]\n - [[DIFFERENT HREF STYLES][Different href styles]]\n - [[EXAMPLE][Example]]\n")))

(defun org-toc-flush-subheadings (toc max-depth)
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

(ert-deftest org-toc-test-flush-subheadings ()
  (should (equal (org-toc-flush-subheadings "* About\n" 0)
                 ""))
  (should (equal (org-toc-flush-subheadings "* About\n" 1)
                 "* About\n"))
  (should (equal (org-toc-flush-subheadings "* About\n" 2)
                 "* About\n"))

  (should (equal (org-toc-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 0)
                 ""))
  (should (equal (org-toc-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 1)
                 "* About\n* Installation\n* Use\n* Different href styles\n* Example\n"))
  (should (equal (org-toc-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 2)
                 "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n"))
  (should (equal (org-toc-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 3)
                 "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n")))

(defun org-toc-insert-toc ()
  "Looks for a headline with the TOC tag and updates it with the
current table of contents.

To add a TOC tag, you can use the command
`org-set-tags-command'.

In addition to the simple :TOC: tag, you can also use the
following tag formats:

- :TOC@2: - sets the max depth of the headlines in the table of
  contents to 2 (the default)

- :TOC@2@gh: - sets the max depth as in above and also uses the
  GitHub-style hrefs in the table of contents (the default). The
  other supported href style is 'org', which is the default org
  style (you can use C-c C-o to go to the headline at point)."

  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        ;; find the first heading with the :TOC: tag
        (when (re-search-forward org-toc-org-toc-regexp (point-max) t)

          (let* ((tag (match-string 1))
                 (depth (if tag
                            (- (aref tag 1) ?0) ;; is there a better way to convert char to number?
                          org-toc-max-depth))
                 (hrefify-tag (if (and tag (>= (length tag) 4))
                                  (downcase (substring tag 3))
                                org-toc-hrefify-default))
                 (hrefify-string (concat "org-toc-hrefify-" hrefify-tag))
                 (hrefify (intern-soft hrefify-string)))
            (if hrefify
                (progn
                  (forward-line 1)

                  ;; insert newline if TOC is currently empty
                  (when (looking-at "^\\*")
                    (open-line 1))

                  ;; remove previous TOC
                  (delete-region (point)
                                 (save-excursion
                                   (search-forward-regexp "^\\*" (point-max) 0)
                                   (forward-line -1)
                                   (end-of-line)
                                   (point)))

                  (insert (org-toc-hrefify-toc (org-toc-flush-subheadings (org-toc-raw-toc) depth) hrefify)))
              (message (concat "Hrefify function " hrefify-string " is not found")))))))))

;;;###autoload
(defun org-toc-enable ()
  "Enable org-toc in this buffer."
  (add-hook 'before-save-hook 'org-toc-insert-toc nil t))

;; Local Variables:
;; compile-command: "emacs -batch -l ert -l *.el -f ert-run-tests-batch-and-exit && emacs -batch -f batch-byte-compile *.el 2>&1 | sed -n '/Warning\|Error/p' | xargs -r ls"
;; End:

(provide 'org-toc)
;;; org-toc.el ends here
