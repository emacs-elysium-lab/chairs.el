;;; chairs.el --- CHAnge or add surrounding pAIRS -*- lexical-binding: t -*-


;; Copyright (C) 2025 Ditto <ditto@mf.me>

;; Package-Version: 0.1.0
;; Keywords: extensions, convenience
;; Package-Requires: ((emacs "28.1") (expand-region "0.7"))
;; Homepage: https://github.com/emacs-elysium-lab/chairs.el

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

;; * chairs README                                                                 :README:
;; ** What is chairs
;; Chairs means CHAnging pAIRS.
;;
;; Chairs provides functionality of changing pairs or adding simple pairs commands,
;; based on [expand-region](https://github.com/magnars/expand-region.el).
;;
;; ** Usage
;; *** Installation
;; **** manual
;; Add to ~load-path~
;; #+begin_src elisp
;;   (add-to-list 'load-path "/path/to/chairs.el/")
;;   (require 'chairs)
;; #+end_src
;;
;; *** Commands
;; Chairs provides two commands: ~chairs-rewrap~ and ~chairs-add~
;;
;; ~chairs-rewrap~ is to change pairs of current sexp, ~chairs-add~ is to add pairs
;; around current sexp.
;;

;; Use with [[https://github.com/AmaiKinono/puni][puni]].
;; For complex pairs like html tags, we can use ~puni-squeeze~ instead, and for
;; deleting pairs, we can use ~puni-splice~.

;; There are a few examples
;; **** ~chairs-rewrap~
;; #+begin_src elisp
;;   (defun foo (arg)
;;     (bar (cons "fo|o-bar" bar-foo)))
;; #+end_src
;; ~|~ means cursor postion.
;;
;; When we use ~chairs-rewrap~ command, it will ask to insert a char first, and after we
;; insert =(= or =)=, the code snippet above will turn into:
;;
;; #+begin_src elisp
;;   (defun foo (arg)
;;     (bar (cons (fo|o-bar) bar-foo)))
;; #+end_src
;;
;; **** chairs-add
;; #+begin_src elisp
;;   (defun foo (arg)
;;     (bar (cons "foo-bar" ba|r-foo)))
;; #+end_src
;; This time we use ~chairs-add~ instead, after we insert =(= or =)=:
;; #+begin_src elisp
;;   (defun foo (arg)
;;     (bar (cons "foo-bar" (ba|r-foo))))
;; #+end_src
;;
;; *** User options
;; **** keybindings
;; #+begin_src emacs-lisp
;; (global-set-key (kbd "s-r") #'chairs-rewrap)
;; (global-set-key (kbd "s-s") #'chairs-add)
;; #+end_src
;; **** ~chairs-pairs-alist~
;; ~chair-pairs-alist~ is an alist for defining pairs
;; #+begin_src elisp
;;   (setq chair-pairs-alist
;;   '((?\( . ?\))
;;     (?\[ . ?\])
;;     (?\{ . ?\})
;;     (?\< . ?\>)
;;     (?\" . ?\")
;;     (?\' . ?\')
;;     (?\` . ((t . ?\`)
;;             (emacs-lisp-mode . ?\')))))
;; #+end_src
;; We can define our own mode specific pairs like that.
;;
;; =C-h f= ~chair-pairs-alist~ for details.
;;
;; **** ~chairs-highlight-pairs~
;; ~chairs-highlight-pairs~ is a boolean to enable highlight pairs or region when invoke
;; ~chairs-rewrap~ and ~chair-add~ command. See Demos section for more information.
;;
;; **** ~chairs-overlay-highlight-face~
;; Face of overlay created by chairs.
;; ** Demos
;; *** ~chairs-rewrap~
;; [[file:demos/chairs-rewrap.mov][chairs-rewrap demo]]
;; *** ~chairs-add~
;; [[file:demos/chairs-add.mov][chairs-add demo]]


;;; Code:
;; * chairs code

(require 'expand-region)

(defgroup chairs nil
  "CHAnge and Add pAIRS."
  :group 'tools
  :prefix "chairs-"
  :link '(url-link "https://github.com/emacs-elysium-lab/chairs.el"))

(defcustom chairs-pairs-alist
  '((?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
    (?\< . ?\>)
    (?\" . ?\")
    (?\' . ?\')
    (?\` . ((t . ?\`)
            (emacs-lisp-mode . ?\'))))
  "Alist of chairs pairs.

car of its elements is a char of open pair, cdr of its elements
is either a char of cloing pair or an alist.  The alist is in
the form of ((t . CHAR) (MODE . CHAR) ... to determine a
mode-specific closing pair."
  :type 'alist
  :group 'chairs)

(defcustom chairs-highlight-pairs t
  "Whether to highlight paris."
  :type 'boolean
  :group 'chairs)

(defface chairs-overlay-highlight-face
  '((t (:inherit pulse-highlight-face)))
  "Face used for chairs pulse.")

(defvar-local chairs--overlay-list nil)

(defun chairs--overlay-create (bounds &optional region-end)
  "Make overlay between (car BOUNDS) and (cdr BOUNDS).
If REGION-END is non nil, `make-overlay' between BOUNDS and REGION-END instead."
  (let ((beg (if region-end bounds (car bounds)))
        (end (if region-end region-end (cdr bounds))))
    (when chairs-highlight-pairs
      (let ((overlay (make-overlay beg end)))
        (overlay-put overlay 'face 'chairs-overlay-highlight-face)
        (add-to-list 'chairs--overlay-list overlay)))))

(defun chairs--overlay-clear ()
  "Clear overlay created by chairs."
  (while (and chairs-highlight-pairs
              chairs--overlay-list)
    (delete-overlay (pop chairs--overlay-list))))

(defun chairs--get-closing-from-opening (char)
  "Return char of closing pair according to CHAR.
Return nil if there is no match in `chairs-pairs-alist'"
  (let ((closing (alist-get char chairs-pairs-alist)))
    (cond
     ((and closing (listp closing))
      (or (cdr (assoc major-mode closing (lambda (x _) (derived-mode-p x))))
          (alist-get t closing)
          char))
     (closing))))

(defun chairs--get-opening-from-closing (char)
  "Return char of opening pair according to CHAR.
Return nil if there is no match in `chairs-pairs-alist'."
  (cond ((car (seq-find (lambda (x)
                          (when-let* ((match (cdr x))
                                      (match-list-p (listp match)))
                            (seq-find
                             (pcase-lambda (`(,m . ,c))
                               (and (or (derived-mode-p m) (eq m t))
                                     (= c char)))
                             match)))
                        chairs-pairs-alist)))
        ((car (rassq char chairs-pairs-alist)))))

(defun chairs--pairs (char)
  "Return cons of string (opening . closing)  according to CHAR."
  (let* ((opening
          (cond ((chairs--get-closing-from-opening char)
                 char)
                ((chairs--get-opening-from-closing char))
                (t char)))
         (closing
          (cond ((chairs--get-closing-from-opening opening))
                (t opening))))
    (cons (char-to-string opening) (char-to-string closing))))

(defun chairs--insert-pairs-around-delete-region (pairs bounds1 bounds2)
  "Delete BOUNDS1 then insert PAIRS around BPOUNDS2."
    (let ((string (buffer-substring-no-properties (car bounds2) (cdr bounds2))))
      (delete-region (car bounds1) (cdr bounds1))
      (insert
       (format "%s%s%s" (car pairs) string (cdr pairs)))))

(defun chairs--general (bounds1 bounds2)
  "Insert pairs ,create and delete overlay on BOUNDS1 and BOUNDS2."
  (catch 'done
    (unless (and bounds1 bounds2)
      (user-error "CHAIRS: No available sexp!"))
    (unless (use-region-p)
      (if (equal bounds1 bounds2)
          (chairs--overlay-create bounds1)
        (chairs--overlay-create (car bounds1) (car bounds2))
        (chairs--overlay-create (cdr bounds2) (cdr bounds1))))
    (let (char pairs)
      (setq char (read-key "CHAIRS: Insert pair: "))
      (unless (and (>= char 32) (<= char 126)) ; printable
        (chairs--overlay-clear)
        (throw 'done 'abort))
      (chairs--overlay-clear)
      (setq pairs (chairs--pairs char))
      (chairs--insert-pairs-around-delete-region pairs bounds1 bounds2))))

;;;###autoload
(defun chairs-rewrap ()
  "This can be used to \"rewrap\" a sexp.

When there's an active balanced region, rewrap it surroundings
with insert pairs."
  (interactive)
  (let* ((orig-point (point))
         (bounds-around
          (save-mark-and-excursion
            (unless (use-region-p)
              (er--expand-region-1)
              (while (not (or (= (point) (point-min))
                                 (cl-some (lambda (x)
                                            (and (= (char-after) (car x))
                                                 (= (char-before (region-end))
                                                    (chairs--get-closing-from-opening (car x)))))
                                          chairs-pairs-alist)))
                (er--expand-region-1)))
            (bounds-of-thing-at-point 'region)))
         (bounds-inside (cons (1+ (car bounds-around)) (1- (cdr bounds-around)))))
    (chairs--general bounds-around bounds-inside)
    (goto-char orig-point)))

;;;###autoload
(defun chairs-add ()
  "Add parirs to region or sexp at point or region if active."
  (interactive)
  (let ((orig-point (point))
        (bounds-at-pt
         (save-mark-and-excursion
           (unless (use-region-p)
             (er--expand-region-1))
           (bounds-of-thing-at-point 'region))))
    (chairs--general bounds-at-pt bounds-at-pt)
    (goto-char orig-point)))

(provide 'chairs)
;;; chairs.el ends here
