;; Copyright (C) 2017 Sergei Nosov

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

(require 'ert)
(require 'cl-lib)
(require 'toc-org)

(ert-deftest test-toc-org-raw-toc ()
  "Test the `toc-org-raw-toc' function"

  (defun test-toc-org-raw-toc-gold-test (content gold)
    (should (equal
             (with-temp-buffer
               (insert content)
               (toc-org-raw-toc nil))
             gold)))
  (declare-function test-toc-org-raw-toc-gold-test "toc-org") ;; suppress compiler warning

  (let ((beg "* TODO [[http://somewhere.com][About]]\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n\n* Table of Contents                                                     ")
        (gold "* About\n"))

    ;; different TOC styles
    (test-toc-org-raw-toc-gold-test (concat beg ":TOC:"         ) gold)
    (test-toc-org-raw-toc-gold-test (concat beg ":TOC_1:"       ) gold)
    (test-toc-org-raw-toc-gold-test (concat beg ":TOC_1_qqq:"   ) gold)
    (test-toc-org-raw-toc-gold-test (concat beg ":TOC@1:"       ) gold)
    (test-toc-org-raw-toc-gold-test (concat beg ":TOC@1@cxv:"   ) gold)
    (test-toc-org-raw-toc-gold-test (concat beg ":TOC@1_hello:" ) gold)

    ;; trailing symbols
    (test-toc-org-raw-toc-gold-test (concat beg ":TOC@1_hello:" "\n\n\n") gold)
    (test-toc-org-raw-toc-gold-test (concat beg ":TOC@1_hello:" "\n\n\nsdfd") gold))

  ;; more complex cases
  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n* Installation\n** via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n** Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n- Go to it and clone toc-org there\n  #+BEGIN_SRC sh\n    git clone https://github.com/snosov1/toc-org.git\n  #+END_SRC\n- Put this in your ~/.emacs file\n  #+BEGIN_SRC elisp\n    (add-to-list 'load-path \"~/.emacs.d/toc-org\")\n    (when (require 'toc-org nil t)\n      (add-hook 'org-mode-hook 'toc-org-enable))\n  #+END_SRC\n\n* [#B] Use\n\nAfter the installation, every time you'll be saving an org file, the\nfirst headline with a :TOC: tag will be updated with the current table\nof contents.\n\nTo add a TOC tag, you can use the command =org-set-tags-command=.\n\nIn addition to the simple :TOC: tag, you can also use the following\ntag formats:\n\n- :TOC@2: - sets the max depth of the headlines in the table of\n  contents to 2 (the default)\n\n- :TOC@2@gh: - sets the max depth as in above and also uses the\n  GitHub-style hrefs in the table of contents (the default). The other\n  supported href style is 'org', which is the default org style (you\n  can use C-c C-o to go to the headline at point).\n\nYou can also use =_= as separator, instead of =@=.\n\n* TODO [#a] Different href styles\n\nCurrently, only 2 href styles are supported: =gh= and =org=. You can easily\ndefine your own styles. If you use the tag =:TOC@2@STYLE:= (=STYLE= being a\nstyle name), then the package will look for a function named\n=toc-org-hrefify-STYLE=, which accepts a heading string and returns a href\ncorresponding to that heading.\n\nE.g. for =org= style it simply returns input as is:\n\n#+BEGIN_SRC emacs-lisp\n  (defun toc-org-hrefify-org (str)\n    \"Given a heading, transform it into a href using the org-mode\n  rules.\"\n    str)\n#+END_SRC\n\n* Example\n\n#+BEGIN_SRC org\n  * About\n  * Table of Contents                                           :TOC:\n    - [[#about][About]]\n    - [[#installation][Installation]]\n        - [[#via-packageel][via package.el]]\n        - [[#manual][Manual]]\n    - [[#use][Use]]\n  * Installation\n  ** via package.el\n  ** Manual\n  * Use\n  * DONE Example\n#+END_SRC\n"
   "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n")

  ;; test #+OPTIONS behavior
  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: todo:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n** TODO Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n"
   "* About\n* Installation\n** DONE via package.el\n** TODO Manual\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n** TODO Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n"
   "* About\n* Installation\n** DONE via package.el\n** TODO Manual\n")

  ;; test COMMENT behavior
  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n* COMMENT Rather commentious\n** TODO Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n"
   "* About\n* Installation\n** DONE via package.el\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n* Rather commentious\n** COMMENT Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n"
   "* About\n* Installation\n** DONE via package.el\n* Rather commentious\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n* Rather commentious\n** COMMENT Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n*** Commented\n** No remove\n* Test\n"
   "* About\n* Installation\n** DONE via package.el\n* Rather commentious\n** No remove\n* Test\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n*** Rather commentious\n** COMMENT Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n*** Commented\n** No remove\n* Test\n"
   "* About\n* Installation\n** DONE via package.el\n*** Rather commentious\n** No remove\n* Test\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n*** Rather commentious\n** COMMENT Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n*** Commented\n** No remove\n* Test ***\n"
   "* About\n* Installation\n** DONE via package.el\n*** Rather commentious\n** No remove\n* Test ***\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n* COMMENT Rather commentious\n** TODO Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n* Comment faire ?\nCapitalized\n"
   "* About\n* Installation\n** DONE via package.el\n* Comment faire ?\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n* COMMENT Rather commentious\n** TODO Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n* comment faire ?\nNot capitalized\n"
   "* About\n* Installation\n** DONE via package.el\n* comment faire ?\n")

  ;; test noexport behavior
  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n* Rather commentious   :taggy:noexport:\n** TODO Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n"
   "* About\n* Installation\n** DONE via package.el\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n* Rather commentious\n** Manual                                                             :noexport:Hello:\n- Create folder ~/.emacs.d if you don't have it\n"
   "* About\n* Installation\n** DONE via package.el\n* Rather commentious\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n* Rather commentious\n** Manual                                                             :noexport:\n- Create folder ~/.emacs.d if you don't have it\n*** Commented\n** No remove\n* Test\n"
   "* About\n* Installation\n** DONE via package.el\n* Rather commentious\n** No remove\n* Test\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n*** Rather commentious\n** Manual                                                             :noexport:\n- Create folder ~/.emacs.d if you don't have it\n*** Commented\n** No remove\n* Test\n"
   "* About\n* Installation\n** DONE via package.el\n*** Rather commentious\n** No remove\n* Test\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n*** Rather commentious\n** Manual                                                             :noexport:\n- Create folder ~/.emacs.d if you don't have it\n*** Commented\n** No remove\n* Test ***\n"
   "* About\n* Installation\n** DONE via package.el\n*** Rather commentious\n** No remove\n* Test ***\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n*** Rather commentious\n** Manual                                                             :noexport_1:\n- Create folder ~/.emacs.d if you don't have it\n*** Commented\n** No remove\n* Test ***\n"
   "* About\n* Installation\n** DONE via package.el\n*** Rather commentious\n** Manual\n** No remove\n* Test ***\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n*** Rather commentious\n** Manual                                                             :noexport_2:\n- Create folder ~/.emacs.d if you don't have it\n*** Commented\n** No remove\n* Test ***\n"
   "* About\n* Installation\n** DONE via package.el\n*** Rather commentious\n** Manual\n*** Commented\n** No remove\n* Test ***\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n*** Rather commentious\n** Manual                                                             :noexport_2:\n- Create folder ~/.emacs.d if you don't have it\n*** Commented\nAAA\nBBBC\n**** Commomentoded\nWEW\n** No remove\n* Test ***\n"
   "* About\n* Installation\n** DONE via package.el\n*** Rather commentious\n** Manual\n*** Commented\n** No remove\n* Test ***\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n*** Rather commentious\n** Manual                                                             :noexport_2:\n- Create folder ~/.emacs.d if you don't have it\n*** EWW\n*** Commented :noexport:\nAAA\nBBBC\n**** Commomentoded\nWEW\n** No remove\n* Test ***\n"
   "* About\n* Installation\n** DONE via package.el\n*** Rather commentious\n** Manual\n*** EWW\n** No remove\n* Test ***\n")

  (test-toc-org-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n#+OPTIONS: num: nil todo:t |:t\n\n* Installation\n** DONE via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n*** Rather commentious\n** Manual                                                             :noexport_2:\n- Create folder ~/.emacs.d if you don't have it\n*** EWW\n*** Commented :noexport_1:\nAAA\nBBBC\n**** Commomentoded\nWEW\n** No remove\n* Test ***\n"
   "* About\n* Installation\n** DONE via package.el\n*** Rather commentious\n** Manual\n*** EWW\n*** Commented\n** No remove\n* Test ***\n")

  ;; test custom todo keywords
  (test-toc-org-raw-toc-gold-test
   "#+STARTUP: showall align\n#+OPTIONS: todo:nil tasks:all tags:nil\n#+EXCLUDE_TAGS: TOC_3\n#+TODO: CANCER SHMANCER | PARTY\n#+TYP_TODO: FII IFF ADS\n\n* Summary                                                             :TOC_3:\n- [[#done-task1][DONE Task1]]\n- [[#todo-task3][TODO Task3]]\n- [[#task2][Task2]]\n  - [[#subtask][SubTask]]\n- [[#task3][Task3]]\n\n* DONE Task1\n  CLOSED: [2017-09-08 Fri 01:46]\n* TODO Task3\n* CANCER Task2\n** FII SubTask\n* CANCER Task3\n"
   "* DONE Task1\n* TODO Task3\n* Task2\n** SubTask\n* Task3\n")

  (test-toc-org-raw-toc-gold-test
   "#+STARTUP: showall align\n#+OPTIONS: todo:nil tasks:all tags:nil\n#+EXCLUDE_TAGS: TOC_3\n#+TODO: CANCER(c@) SHMANCER | PARTY\n#+TYP_TODO: FII IFF ADS\n\n* Summary                                                             :TOC_3:\n- [[#done-task1][DONE Task1]]\n- [[#todo-task3][TODO Task3]]\n- [[#task2][Task2]]\n  - [[#subtask][SubTask]]\n- [[#task3][Task3]]\n\n* DONE Task1\n  CLOSED: [2017-09-08 Fri 01:46]\n* TODO Task3\n* CANCER Task2\n** FII SubTask\n* CANCER Task3\n"
   "* DONE Task1\n* TODO Task3\n* Task2\n** SubTask\n* Task3\n")
  )

(ert-deftest test-toc-org-hrefify-gh ()
  "Test the `toc-org-hrefify-gh' function"
  (should (equal (toc-org-hrefify-gh "About") "#about"))
  (should (equal (toc-org-hrefify-gh "!h@#$%^&*(){}|][:;\"'/?.>,<`~") "#h"))
  (should (equal (toc-org-hrefify-gh "!h@#$% ^&*(S){}|][:;\"'/?.>,<`~") "#h-s"))
  (should (equal (toc-org-hrefify-gh "[60%] Context Extraction Service [60%] [3/5]") "#60-context-extraction-service-60-35"))
  (should (equal (toc-org-hrefify-gh "Context Extraction Service [60%] [3/5]") "#context-extraction-service-60-35"))
  (should (equal (toc-org-hrefify-gh "Context Extraction Service [3/5]")       "#context-extraction-service-35"))
  (should (equal (toc-org-hrefify-gh "Context Extraction Service [60%]")       "#context-extraction-service-60"))

  (let ((hash (make-hash-table :test 'equal)))
    (should (equal (toc-org-hrefify-gh "About" hash) "#about"))
    (puthash "#about" "About" hash)
    (should (equal (toc-org-hrefify-gh "About" hash) "#about-1"))
    (puthash "#about-1" "About" hash)
    (should (equal (toc-org-hrefify-gh "About" hash) "#about-2"))
    (puthash "#about-2" "About" hash)
    (should (equal (toc-org-hrefify-gh "About" hash) "#about-3"))))

(ert-deftest test-toc-org-format-visible-link ()
  "Test the `toc-org-format-visible-link' function"
  (should (equal (toc-org-format-visible-link "About") "About"))
  (should (equal (toc-org-format-visible-link "[60%] Context Extraction Service [60%] [3/5]") "Context Extraction Service"))
  (should (equal (toc-org-format-visible-link "Context Extraction Service [60%] [3/5]") "Context Extraction Service"))
  (should (equal (toc-org-format-visible-link "Context Extraction Service [3/5]") "Context Extraction Service"))
  (should (equal (toc-org-format-visible-link "Context Extraction Service [60%]") "Context Extraction Service")))

(ert-deftest test-toc-org-hrefify-toc ()
  (dolist (params '((nil . "- [[ABOUT][About]]\n") ('t . "- [About](ABOUT)\n")))
    (let ((hash (make-hash-table :test 'equal)))
      (should (equal (toc-org-hrefify-toc "* About\n"
                                          (lambda (str &optional hash) (upcase str))
                                          (car params)
                                          hash)
                     (cdr params)))
      (should (equal (gethash "ABOUT" hash) "About"))))

  ;; check trailing space: https://github.com/snosov1/toc-org/pull/31
  (let ((hash (make-hash-table :test 'equal)))
    (should (equal (toc-org-hrefify-toc "* About  \n"
                                        (lambda (str &optional hash) (upcase str))
                                        nil
                                        hash)
                   "- [[ABOUT][About]]\n"))
    (should (equal (gethash "ABOUT" hash) "About")))
  (let ((hash (make-hash-table :test 'equal)))
    (should (equal (toc-org-hrefify-toc "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" (lambda (str &optional hash) (upcase str)) nil hash)
                   "- [[ABOUT][About]]\n- [[INSTALLATION][Installation]]\n  - [[VIA PACKAGE.EL][via package.el]]\n  - [[MANUAL][Manual]]\n- [[USE][Use]]\n- [[DIFFERENT HREF STYLES][Different href styles]]\n- [[EXAMPLE][Example]]\n"))
    (should (equal (gethash "ABOUT" hash) "About"))
    (should (equal (gethash "INSTALLATION" hash) "Installation"))
    (should (equal (gethash "VIA PACKAGE.EL" hash) "via package.el"))
    (should (equal (gethash "MANUAL" hash) "Manual"))
    (should (equal (gethash "USE" hash) "Use"))
    (should (equal (gethash "DIFFERENT HREF STYLES" hash) "Different href styles"))
    (should (equal (gethash "EXAMPLE" hash) "Example"))))

(ert-deftest test-toc-org-flush-subheadings ()
  (should (equal (toc-org-flush-subheadings "* About\n" 0)
                 ""))
  (should (equal (toc-org-flush-subheadings "* About\n" 1)
                 "* About\n"))
  (should (equal (toc-org-flush-subheadings "* About\n" 2)
                 "* About\n"))

  (should (equal (toc-org-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 0)
                 ""))
  (should (equal (toc-org-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 1)
                 "* About\n* Installation\n* Use\n* Different href styles\n* Example\n"))
  (should (equal (toc-org-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 2)
                 "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n"))
  (should (equal (toc-org-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 3)
                 "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n")))

(ert-deftest test-toc-org-insert-toc ()
  "Test the `toc-org-insert-toc' function"

  (defun test-toc-org-insert-toc-gold-test (content gold)
    (with-temp-buffer
      (org-mode)
      (insert content)
      (toc-org-insert-toc)
      (should (equal
               (buffer-substring-no-properties
                (point-min) (point-max))
               gold))))
  (declare-function test-toc-org-insert-toc-gold-test "toc-org") ;; suppress compiler warning

  (let ((beg "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     "))
    (test-toc-org-insert-toc-gold-test
     (concat beg ":TOC:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC:\n- [[#about][About]]\n- [[#hello][Hello]]\n  - [[#good-bye][Good-bye]]\n")

    (test-toc-org-insert-toc-gold-test
     (concat beg ":TOC_1:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC_1:\n- [[#about][About]]\n- [[#hello][Hello]]\n")

    (test-toc-org-insert-toc-gold-test
     (concat beg ":TOC_3:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC_3:\n- [[#about][About]]\n- [[#hello][Hello]]\n  - [[#good-bye][Good-bye]]\n    - [[#salut][Salut]]\n")

    (test-toc-org-insert-toc-gold-test
     (concat beg ":TOC_1_org:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC_1_org:\n- [[About][About]]\n- [[Hello][Hello]]\n")

    (test-toc-org-insert-toc-gold-test
     (concat beg ":TOC_3_org:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC_3_org:\n- [[About][About]]\n- [[Hello][Hello]]\n  - [[Good-bye][Good-bye]]\n    - [[Salut][Salut]]\n")

    (test-toc-org-insert-toc-gold-test
     (concat beg ":TOC:\n:PROPERTIES:\n:VISIBILITY: content\n:END:\n")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC:\n:PROPERTIES:\n:VISIBILITY: content\n:END:\n- [[#about][About]]\n- [[#hello][Hello]]\n  - [[#good-bye][Good-bye]]\n")

    (test-toc-org-insert-toc-gold-test
     (concat beg ":TOC_3:QUOTE:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC_3:QUOTE:\n#+BEGIN_QUOTE\n- [[#about][About]]\n- [[#hello][Hello]]\n  - [[#good-bye][Good-bye]]\n    - [[#salut][Salut]]\n#+END_QUOTE\n")

    (test-toc-org-insert-toc-gold-test
     (concat beg ":QUOTE:hello:TOC_3_gh:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :QUOTE:hello:TOC_3_gh:\n#+BEGIN_QUOTE\n- [[#about][About]]\n- [[#hello][Hello]]\n  - [[#good-bye][Good-bye]]\n    - [[#salut][Salut]]\n#+END_QUOTE\n")

    (test-toc-org-insert-toc-gold-test
     "* H1\n* H2\n* TOC           :TOC:\n - [[#header-1][Header 1]]\n - [[#header-2][Header 2]]\n"
     "* H1\n* H2\n* TOC           :TOC:\n- [[#h1][H1]]\n- [[#h2][H2]]\n")

    (test-toc-org-insert-toc-gold-test
     "* H1\n* TODO H2\n* TOC           :TOC:\n \n"
     "* H1\n* TODO H2\n* TOC           :TOC:\n- [[#h1][H1]]\n- [[#h2][H2]]\n")
    ))

(ert-deftest test-toc-org-insert-toc-markdown ()
  "Test the `toc-org-insert-toc' function"

  (define-derived-mode markdown-mode text-mode "Markdown")

  (defun test-toc-org-insert-toc-gold-test-markdown (content gold)
    (with-temp-buffer
      (markdown-mode)
      (insert content)
      (toc-org-insert-toc)
      (should (equal
               (buffer-substring-no-properties
                (point-min) (point-max))
               gold))))
  ;; suppress compiler warnings
  (declare-function test-toc-org-insert-toc-gold-test-markdown "toc-org")
  (declare-function markdown-mode "toc-org")

  (let ((beg "# About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n# Hello\n## Good-bye ##\n### Salut\n# Table of Contents                                                     "))
    (test-toc-org-insert-toc-gold-test-markdown
     (concat beg "<-- :TOC: -->")
     "# About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n# Hello\n## Good-bye ##\n### Salut\n# Table of Contents                                                     <-- :TOC: -->\n- [About](#about)\n- [Hello](#hello)\n  - [Good-bye](#good-bye)\n")

    ;; headings inside ``` code fences must not appear in the TOC
    (test-toc-org-insert-toc-gold-test-markdown
     "# Real heading\n\n```bash\n# Not a heading\n## Also not a heading\n```\n\n## Sub heading\n\n# Table of Contents <-- :TOC: -->\n"
     "# Real heading\n\n```bash\n# Not a heading\n## Also not a heading\n```\n\n## Sub heading\n\n# Table of Contents <-- :TOC: -->\n- [Real heading](#real-heading)\n  - [Sub heading](#sub-heading)\n")
    ))

;; Tests for toc-org-navigation-pane

(ert-deftest test-toc-org-navigation-pane-first-heading-shown ()
  "Navigation pane shows all headings including the first one.
Regression: toc-org-raw-toc always deleted the line at point after the
search for the :toc: heading, even when no :toc: heading was present,
silently dropping the first heading."
  (when (get-buffer toc-org-navigation-pane-buffer-name) (kill-buffer toc-org-navigation-pane-buffer-name))
  (let* ((tmpfile (make-temp-file "toc-org-test" nil ".org"))
         (buf (find-file-noselect tmpfile)))
    (unwind-protect
        (with-current-buffer buf
          (insert "* First\n* Second\n")
          (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'select-window)  #'ignore))
            (toc-org-navigation-pane))
          (with-current-buffer (get-buffer toc-org-navigation-pane-buffer-name)
            (should (string-match-p "First" (buffer-string)))
            (should (string-match-p "Second" (buffer-string)))))
      (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
      (when (get-buffer toc-org-navigation-pane-buffer-name) (kill-buffer (get-buffer toc-org-navigation-pane-buffer-name)))
      (kill-buffer buf)
      (delete-file tmpfile))))

(ert-deftest test-toc-org-navigation-pane-point-preserved ()
  "Navigation pane restores window-point on refresh, not reset to beginning.
Regression: toc-org--toc-buffer is defvar-local, so referencing it inside
with-current-buffer (where the TOC buffer is current) yields nil instead of
the TOC buffer.  get-buffer-window then received nil, skipping the
window-point path and always resetting point to the beginning."
  (let* ((tmpfile (make-temp-file "toc-org-test" nil ".org"))
         (buf (find-file-noselect tmpfile))
         (fake-win (cons 'fake 'window))
         captured-set-point)
    (unwind-protect
        (with-current-buffer buf
          (insert "* Heading 1\n* Heading 2\n* Heading 3\n")
          (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'select-window)  #'ignore))
            (toc-org-navigation-pane))
          ;; Mock get-buffer-window to return fake-win only for a real buffer
          ;; object — nil (the value toc-org--toc-buffer has inside
          ;; with-current-buffer) would return nil, skipping set-window-point.
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (b &rest _) (when (bufferp b) fake-win)))
                    ((symbol-function 'window-point)
                     (lambda (_) 5))
                    ((symbol-function 'set-window-point)
                     (lambda (_ pos) (setq captured-set-point pos))))
            (toc-org--refresh-navigation-pane))
          (should (eql captured-set-point 5)))
      (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
      (when (get-buffer toc-org-navigation-pane-buffer-name)
        (kill-buffer (get-buffer toc-org-navigation-pane-buffer-name)))
      (kill-buffer buf)
      (delete-file tmpfile))))

(ert-deftest test-toc-org-navigation-pane-non-file-buffer ()
  "Navigation pane leaves no *org-toc* buffer when buffer is not visiting a file."
  (when (get-buffer toc-org-navigation-pane-buffer-name) (kill-buffer toc-org-navigation-pane-buffer-name))
  (with-temp-buffer
    (org-mode)
    (insert "* Heading 1\n* Heading 2\n")
    (toc-org-navigation-pane))
  (should (null (get-buffer toc-org-navigation-pane-buffer-name))))

(ert-deftest test-toc-org-navigation-pane-no-headings-no-leak ()
  "Navigation pane leaves no *org-toc* buffer when buffer has no headings."
  (when (get-buffer toc-org-navigation-pane-buffer-name) (kill-buffer toc-org-navigation-pane-buffer-name))
  (let* ((tmpfile (make-temp-file "toc-org-test" nil ".org"))
         (buf (find-file-noselect tmpfile)))
    (unwind-protect
        (with-current-buffer buf
          (toc-org-navigation-pane)
          (should (null (get-buffer toc-org-navigation-pane-buffer-name))))
      (kill-buffer buf)
      (delete-file tmpfile))))

(ert-deftest test-toc-org-navigation-pane-hook-added ()
  "buffer-list-update-hook is added when the navigation pane is opened."
  (let* ((tmpfile (make-temp-file "toc-org-test" nil ".org"))
         (buf (find-file-noselect tmpfile)))
    (unwind-protect
        (with-current-buffer buf
          (insert "* Heading 1\n* Heading 2\n")
          (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'select-window)  #'ignore))
            (toc-org-navigation-pane))
          (should (memq #'toc-org--close-toc-pane buffer-list-update-hook)))
      (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
      (when (get-buffer toc-org-navigation-pane-buffer-name) (kill-buffer (get-buffer toc-org-navigation-pane-buffer-name)))
      (kill-buffer buf)
      (delete-file tmpfile))))

(ert-deftest test-toc-org-navigation-pane-hook-removed ()
  "buffer-list-update-hook is removed once all TOC windows are closed."
  (let* ((tmpfile (make-temp-file "toc-org-test" nil ".org"))
         (buf (find-file-noselect tmpfile)))
    (unwind-protect
        (with-current-buffer buf
          (insert "* Heading 1\n* Heading 2\n")
          (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'select-window)  #'ignore))
            (toc-org-navigation-pane))
          (when toc-org--toc-buffer (kill-buffer toc-org--toc-buffer))
          (toc-org--close-toc-pane)
          (should (null (memq #'toc-org--close-toc-pane buffer-list-update-hook))))
      (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
      (when (get-buffer toc-org-navigation-pane-buffer-name) (kill-buffer (get-buffer toc-org-navigation-pane-buffer-name)))
      (kill-buffer buf)
      (delete-file tmpfile))))

(ert-deftest test-toc-org-navigation-pane-no-error-on-buffer-switch ()
  "Switching away from the source buffer closes *org-toc* without error.
Regression: toc-org--close-toc-pane iterated buffer-list and killed
*org-toc* mid-loop, then tried to with-current-buffer the now-dead entry,
producing \"Selecting deleted buffer\"."
  (let* ((tmpfile (make-temp-file "toc-org-test" nil ".org"))
         (buf (find-file-noselect tmpfile)))
    (unwind-protect
        (with-current-buffer buf
          (insert "* Heading 1\n* Heading 2\n")
          (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'select-window)  #'ignore))
            (toc-org-navigation-pane))
          ;; TOC buffer is still alive; hook kills it mid-iteration — must not error
          (should (buffer-live-p toc-org--toc-buffer))
          (should-not (eq (toc-org--close-toc-pane) 'error))
          (should (null (get-buffer toc-org-navigation-pane-buffer-name))))
      (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
      (when (get-buffer toc-org-navigation-pane-buffer-name) (kill-buffer (get-buffer toc-org-navigation-pane-buffer-name)))
      (kill-buffer buf)
      (delete-file tmpfile))))

(ert-deftest test-toc-org-nav-depth-change ()
  "= / + / - adjust nav pane depth, clamped to [1, max-heading-depth]."
  (when (get-buffer toc-org-navigation-pane-buffer-name)
    (kill-buffer toc-org-navigation-pane-buffer-name))
  (let* ((tmpfile (make-temp-file "toc-org-test" nil ".org"))
         (buf (find-file-noselect tmpfile)))
    (unwind-protect
        (with-current-buffer buf
          (insert "* H1\n** H2\n*** H3\n")
          (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'select-window)  #'ignore))
            (toc-org-navigation-pane))
          (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
          (let ((nav-buf (get-buffer toc-org-navigation-pane-buffer-name)))
            (defun nav-content () (with-current-buffer nav-buf (buffer-string)))
            ;; default depth 2: H1 and H2 visible, H3 not
            (should     (string-match-p "H2" (nav-content)))
            (should-not (string-match-p "H3" (nav-content)))
            ;; increase to 3: H3 appears
            (with-current-buffer nav-buf (toc-org--nav-increase-depth))
            (should (string-match-p "H3" (nav-content)))
            (should (= (buffer-local-value 'toc-org--nav-max-depth nav-buf) 3))
            ;; increase beyond max heading depth (3): clamped at 3
            (with-current-buffer nav-buf (toc-org--nav-increase-depth))
            (should (= (buffer-local-value 'toc-org--nav-max-depth nav-buf) 3))
            ;; decrease to 1: H2 and H3 gone
            (with-current-buffer nav-buf (toc-org--nav-decrease-depth))
            (with-current-buffer nav-buf (toc-org--nav-decrease-depth))
            (should-not (string-match-p "H2" (nav-content)))
            (should (= (buffer-local-value 'toc-org--nav-max-depth nav-buf) 1))
            ;; decrease below minimum: clamped at 1
            (with-current-buffer nav-buf (toc-org--nav-decrease-depth))
            (should (= (buffer-local-value 'toc-org--nav-max-depth nav-buf) 1))))
      (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
      (when (get-buffer toc-org-navigation-pane-buffer-name)
        (kill-buffer (get-buffer toc-org-navigation-pane-buffer-name)))
      (kill-buffer buf)
      (delete-file tmpfile))))

(ert-deftest test-toc-org-nav-next-and-prev ()
  "M-n / M-p move the pane point and update the source window position."
  (when (get-buffer toc-org-navigation-pane-buffer-name)
    (kill-buffer toc-org-navigation-pane-buffer-name))
  (let* ((tmpfile (make-temp-file "toc-org-test" nil ".org"))
         (buf (find-file-noselect tmpfile))
         (win (selected-window)))
    (unwind-protect
        (with-current-buffer buf
          (insert "* First\n* Second\n* Third\n")
          (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'select-window)  #'ignore))
            (toc-org-navigation-pane))
          (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
          (let ((nav-buf (get-buffer toc-org-navigation-pane-buffer-name)))
            (set-window-buffer win buf)
            (cl-letf (((symbol-function 'get-buffer-window)
                       (lambda (b &rest _) (when (eq b buf) win))))
              (with-current-buffer nav-buf
                (goto-char (point-min))
                (toc-org--nav-next)
                (should (= (line-number-at-pos) 2)))
              ;; window-point is a position in buf — check it in buf's context
              (should (= (with-current-buffer buf (line-number-at-pos (window-point win))) 2))
              (with-current-buffer nav-buf
                (toc-org--nav-prev)
                (should (= (line-number-at-pos) 1)))
              (should (= (with-current-buffer buf (line-number-at-pos (window-point win))) 1)))))
      (remove-hook 'buffer-list-update-hook #'toc-org--close-toc-pane)
      (when (get-buffer toc-org-navigation-pane-buffer-name)
        (kill-buffer (get-buffer toc-org-navigation-pane-buffer-name)))
      (kill-buffer buf)
      (delete-file tmpfile))))
