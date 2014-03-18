;; COMMENTED ;;; nimrod-mode.el --- A major mode for the Nimrod programming language
;; COMMENTED ;;
;; COMMENTED ;; Filename: nimrod-mode.el
;; COMMENTED ;; Description: A major mode for the Nimrod programming language
;; COMMENTED ;; Author: Simon Hafner
;; COMMENTED ;; Maintainer: Simon Hafner <hafnersimon@gmail.com>
;; COMMENTED ;; Version: 0.1.5
;; COMMENTED ;; Keywords: nimrod
;; COMMENTED ;; Compatibility: GNU Emacs 24
;; COMMENTED ;; Package-Requires: ((auto-complete "1.4"))
;; COMMENTED ;;
;; COMMENTED ;; Taken over from James H. Fisher <jameshfisher@gmail.com>
;; COMMENTED ;;
;; COMMENTED ;; This program is free software; you can redistribute it and/or modify
;; COMMENTED ;; it under the terms of the GNU General Public License as published by
;; COMMENTED ;; the Free Software Foundation; either version 2, or (at your option)
;; COMMENTED ;; any later version.
;; COMMENTED ;;
;; COMMENTED ;; This program is distributed in the hope that it will be useful,
;; COMMENTED ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; COMMENTED ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; COMMENTED ;; GNU General Public License for more details.
;; COMMENTED ;;
;; COMMENTED ;; You should have received a copy of the GNU General Public License
;; COMMENTED ;; along with this program; see the file COPYING.  If not, write to
;; COMMENTED ;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; COMMENTED ;; Floor, Boston, MA 02110-1301, USA.
;; COMMENTED ;;
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED ;;
;; COMMENTED ;; Todo:
;; COMMENTED ;;
;; COMMENTED ;; -- Make things non-case-sensitive and ignore underscores
;; COMMENTED ;; -- Identifier following "proc" gets font-lock-function-name-face
;; COMMENTED ;; -- Treat parameter lists separately
;; COMMENTED ;; -- Treat pragmas inside "{." and ".}" separately
;; COMMENTED ;; -- Make double-# comments get font-lock-doc-face
;; COMMENTED ;; -- Highlight tabs as syntax error
;; COMMENTED ;;
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED ;;
;; COMMENTED ;;; Code:
;; COMMENTED
;; COMMENTED (require 'auto-complete)
;; COMMENTED
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED ;;                                Helpers                                     ;;
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED
;; COMMENTED (defun nimrod-glue-strings (glue strings)
;; COMMENTED   "Given a list of strings and some glue, concatenate."
;; COMMENTED   (mapconcat 'identity strings glue))
;; COMMENTED
;; COMMENTED (defun nimrod-regexp-choice (strings)
;; COMMENTED   "Given a list of strings, construct a regexp multiple-choice."
;; COMMENTED   (concat "\\(" (nimrod-glue-strings "\\|" strings) "\\)"))
;; COMMENTED
;; COMMENTED
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED ;;                             Simple keywords                                ;;
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED
;; COMMENTED
;; COMMENTED ;; Define keywords, etc.
;; COMMENTED ;; ---------------------
;; COMMENTED
;; COMMENTED (defvar nimrod-keywords
;; COMMENTED   (split-string "
;; COMMENTED addr and as asm atomic
;; COMMENTED bind block break
;; COMMENTED case cast const continue converter
;; COMMENTED discard distinct div do
;; COMMENTED elif else end enum except export
;; COMMENTED finally for from
;; COMMENTED generic
;; COMMENTED if import in include interface is isnot iterator
;; COMMENTED lambda let
;; COMMENTED macro method mixin mod
;; COMMENTED nil not notin
;; COMMENTED object of or out
;; COMMENTED proc ptr
;; COMMENTED raise ref return
;; COMMENTED shared shl shr static
;; COMMENTED template try tuple type
;; COMMENTED var
;; COMMENTED when while with without
;; COMMENTED xor
;; COMMENTED yield
;; COMMENTED ")
;; COMMENTED   "Nimrod keywords. The above string is taken from
;; COMMENTED <http://force7.de/nimrod/manual.html#identifiers-keywords>,
;; COMMENTED for easy updating.")
;; COMMENTED
;; COMMENTED (defvar nimrod-types
;; COMMENTED   '("int" "int8" "int16" "int32" "int64" "float" "float32" "float64"
;; COMMENTED     "bool" "char" "string" "cstring" "pointer" "ordinal" "nil" "expr"
;; COMMENTED     "stmt" "typedesc" "range" "array" "openarray" "seq" "set"
;; COMMENTED     "tgenericseq" "pgenericseq" "nimstringdesc" "nimstring" "byte"
;; COMMENTED     "natural" "positive" "tobject" "pobject" "tresult" "tendian"
;; COMMENTED     "taddress" "biggestint" "biggestfloat" "cchar" "cschar" "cshort"
;; COMMENTED     "cint" "clong" "clonglong" "cfloat" "cdouble" "clongdouble"
;; COMMENTED     "cstringarray" "pfloat32" "pfloat64" "pint64" "pint32"
;; COMMENTED     "tgc_strategy" "tfile" "tfilemode")
;; COMMENTED   "Nimrod types defined in <lib/system.nim>."
;; COMMENTED
;; COMMENTED   )
;; COMMENTED
;; COMMENTED (defvar nimrod-exceptions
;; COMMENTED   '("e_base" "easynch" "esynch" "esystem" "eio" "eos"
;; COMMENTED     "einvalidlibrary" "eresourceexhausted" "earithmetic" "edivbyzero"
;; COMMENTED     "eoverflow" "eaccessviolation" "eassertionfailed" "econtrolc"
;; COMMENTED     "einvalidvalue" "eoutofmemory" "einvalidindex" "einvalidfield"
;; COMMENTED     "eoutofrange" "estackoverflow" "enoexceptiontoreraise"
;; COMMENTED     "einvalidobjectassignment" "einvalidobjectconversion"
;; COMMENTED     "efloatingpoint" "efloatinginvalidop" "efloatdivbyzero"
;; COMMENTED     "efloatoverflow" "efloatunderflow" "efloatinexact")
;; COMMENTED   "Nimrod exceptions defined in <lib/system.nim>.")
;; COMMENTED
;; COMMENTED (defvar nimrod-constants
;; COMMENTED   '("ismainmodule" "compiledate" "compiletime" "nimrodversion"
;; COMMENTED     "nimrodmajor" "nimrodminor" "nimrodpatch" "cpuendian" "hostos"
;; COMMENTED     "hostcpu" "apptype" "inf" "neginf" "nan" "quitsuccess"
;; COMMENTED     "quitfailure" "stdin" "stdout" "stderr" "true" "false" )
;; COMMENTED   "Nimrod constants defined in <lib/system.nim>.")
;; COMMENTED
;; COMMENTED (defvar nimrod-builtins
;; COMMENTED   '("defined" "definedinscope" "not" "+" "-" "=" "<" ">" "@" "&" "*"
;; COMMENTED     ">=" "<=" "$" ">=%" ">%" "<%" "<=%" "," ":" "==" "/"  "div" "mod"
;; COMMENTED     "shr" "shl" "and" "or" "xor" "abs" "+%" "-%" "*%" "/%" "%%" "-+-"
;; COMMENTED     "not_in" "is_not" "cmp" "high" "low" "sizeof" "succ" "pred" "inc"
;; COMMENTED     "dec" "newseq" "len" "incl" "excl" "card" "ord" "chr" "ze" "ze64"
;; COMMENTED     "tou8" "tou16" "tou32" "min" "max" "setlen" "newstring" "add"
;; COMMENTED     "compileoption" "del" "delete" "insert" "repr" "tofloat"
;; COMMENTED     "tobiggestfloat" "toint" "tobiggestint" "addquitproc" "copy"
;; COMMENTED     "zeromem" "copymem" "movemem" "equalmem" "alloc" "alloc0"
;; COMMENTED     "realloc" "dealloc" "assert" "swap" "getrefcount" "getoccupiedmem"
;; COMMENTED     "getfreemem" "gettotalmem" "countdown" "countup" "items"
;; COMMENTED     "enumerate" "isnil" "find" "contains" "pop" "each" "gc_disable"
;; COMMENTED     "gc_enable" "gc_fullcollect" "gc_setstrategy"
;; COMMENTED     "gc_enablemarkandsweep" "gc_disablemarkandsweep"
;; COMMENTED     "gc_getstatistics" "gc_ref" "gc_unref" "accumulateresult" "echo"
;; COMMENTED     "newexception" "quit" "open" "reopen" "close" "endoffile"
;; COMMENTED     "readchar" "flushfile" "readfile" "write" "readline" "writeln"
;; COMMENTED     "getfilesize" "readbytes" "readchars" "readbuffer" "writebytes"
;; COMMENTED     "writechars" "writebuffer" "setfilepos" "getfilepos" "lines"
;; COMMENTED     "filehandle" "cstringarraytoseq" "getdiscriminant" "selectbranch"
;; COMMENTED     "getcurrentexception" "getcurrentexceptionmsg" "likely" "unlikely"
;; COMMENTED     )
;; COMMENTED   "Standard library functions fundamental enough to count as builtins.
;; COMMENTED Magic functions."
;; COMMENTED   )
;; COMMENTED
;; COMMENTED (defvar nimrod-operators
;; COMMENTED   '( "`" "{." ".}" "[" "]" "{" "}" "(" ")" )
;; COMMENTED   "Nimrod standard operators.")
;; COMMENTED
;; COMMENTED
;; COMMENTED ;; Custom faces
;; COMMENTED ;; ------------
;; COMMENTED
;; COMMENTED ;; TODO: make work!?
;; COMMENTED (defface nimrod-tab-face
;; COMMENTED   '((((class color) (background dark))
;; COMMENTED      (:background "grey22" :foreground "darkgray"))
;; COMMENTED     (((class color) (background light))
;; COMMENTED      (:background "beige"  :foreground "lightgray"))
;; COMMENTED     (t (:inverse-video t)))
;; COMMENTED   "Face used to visualize TAB."
;; COMMENTED   :group 'whitespace)
;; COMMENTED
;; COMMENTED
;; COMMENTED ;; Create regular expressions
;; COMMENTED ;; --------------------------
;; COMMENTED
;; COMMENTED
;; COMMENTED ;; regexp-opt'ed expressions
;; COMMENTED ;; '''''''''''''''''''''''''
;; COMMENTED
;; COMMENTED (defvar nimrod-keywords-regexp (regexp-opt nimrod-keywords 'words))
;; COMMENTED (defvar nimrod-types-regexp (regexp-opt nimrod-types 'words))
;; COMMENTED (defvar nimrod-types-regexp (regexp-opt nimrod-exceptions 'words))
;; COMMENTED (defvar nimrod-constants-regexp (regexp-opt nimrod-constants 'words))
;; COMMENTED (defvar nimrod-builtins-regexp (regexp-opt nimrod-builtins 'words))
;; COMMENTED (defvar nimrod-operators-regexp (regexp-opt nimrod-operators 'words))
;; COMMENTED
;; COMMENTED ;; Free memory
;; COMMENTED (defvar nimrod-keywords nil)
;; COMMENTED (defvar nimrod-types nil)
;; COMMENTED (defvar nimrod-exceptions nil)
;; COMMENTED (defvar nimrod-constants nil)
;; COMMENTED (defvar nimrod-builtins nil)
;; COMMENTED (defvar nimrod-operators nil)
;; COMMENTED
;; COMMENTED
;; COMMENTED ;; Hand-reared expressions
;; COMMENTED ;; '''''''''''''''''''''''
;; COMMENTED
;; COMMENTED (defvar nimrod-decimal-regexp
;; COMMENTED   "\\<[0-9_]+\\(\\.[0-9_]+\\)?\\([eE][0-9]+\\)?\\(\'\\(i8\\|i16\\|i32\\|i64\\|f32\\|f64\\)\\)?\\>"
;; COMMENTED   "Regular expression for matching decimal literals in Nimrod."
;; COMMENTED   )
;; COMMENTED
;; COMMENTED (defvar nimrod-hex-regexp
;; COMMENTED   "\\<\\0x[0-9a-fA-F_]+\\(\\.[0-9a-fA-F_]+\\)?\\([eE][0-9a-fA-F]+\\)?\\(\'\\(i8\\|i16\\|i32\\|i64\\|f32\\|f64\\)\\)?\\>"
;; COMMENTED   "Regular expression for matching hexadecimal literals in Nimrod."
;; COMMENTED   )
;; COMMENTED
;; COMMENTED (defvar nimrod-octal-regexp
;; COMMENTED   "\\<\\0o[0-7_]+\\(\\.[0-7_]+\\)?\\([eE][0-7]+\\)?\\(\'\\(i8\\|i16\\|i32\\|i64\\|f32\\|f64\\)\\)?\\>"
;; COMMENTED   "Regular expression for matching octal literals in Nimrod."
;; COMMENTED   )
;; COMMENTED
;; COMMENTED (defvar nimrod-binary-regexp
;; COMMENTED   "\\<\\0b[01_]+\\(\\.[01_]+\\)?\\([eE][01]+\\)?\\(\'\\(i8\\|i16\\|i32\\|i64\\|f32\\|f64\\)\\)?\\>"
;; COMMENTED   "Regular expression for matching binary literals in Nimrod."
;; COMMENTED   )
;; COMMENTED
;; COMMENTED (defvar nimrod-variables-regexp
;; COMMENTED   "\\<[a-zA-Z][a-zA-Z0-9_]+\\>"
;; COMMENTED   "Regular expression for matching variable identifiers in Nimrod."
;; COMMENTED   )
;; COMMENTED
;; COMMENTED (defvar nimrod-character-literal-regexp
;; COMMENTED   "\\<\'\\(.\\|\\\\.*\\)'\\>"  ;; TODO: make more precise
;; COMMENTED   "Regular expression for matching character literal tokens."
;; COMMENTED   )
;; COMMENTED
;; COMMENTED (defvar nimrod-single-quote-string-regexp
;; COMMENTED   "\\<\".*\"\\>"
;; COMMENTED   "Regular expression for matching single-quoted strings."
;; COMMENTED   )
;; COMMENTED
;; COMMENTED (defvar nimrod-raw-string-regexp
;; COMMENTED   "\\<r\".*\"\\>"
;; COMMENTED   "Regular expression for matching raw strings."
;; COMMENTED   )
;; COMMENTED
;; COMMENTED (defconst nimrod-tab-regexp "\\(\t+\\)")
;; COMMENTED
;; COMMENTED (defconst nimrod-blank-line-regexp "^ *$"
;; COMMENTED   "Regexp matching a line containing only (valid) whitespace.")
;; COMMENTED
;; COMMENTED (defconst nimrod-new-block-regexp
;; COMMENTED   (concat ".*"                                          ;; Anything
;; COMMENTED           (nimrod-regexp-choice '("=" "var" "type" "const" "enum" "\\:")) ;; ending in a new block indicator,
;; COMMENTED           " *"                                          ;; then non-syntactic whitespace,
;; COMMENTED           "\\(#.*\\)?"                                  ;; then possibly a comment.
;; COMMENTED           "$")
;; COMMENTED   "Regexp matching a line that precedes a new block.")
;; COMMENTED
;; COMMENTED
;; COMMENTED (setq nimrod-font-lock-keywords
;; COMMENTED       `(  ;; note the BACKTICK, `
;; COMMENTED         (,nimrod-raw-string-regexp . font-lock-string-face)
;; COMMENTED         (,nimrod-character-literal-regexp . font-lock-constant-face)
;; COMMENTED         (,nimrod-single-quote-string-regexp . font-lock-string-face)
;; COMMENTED         (,nimrod-tab-regexp . nimrod-tab-face) ;; TODO: make work!
;; COMMENTED         (,nimrod-keywords-regexp . font-lock-keyword-face)
;; COMMENTED         (,nimrod-types-regexp . font-lock-type-face)
;; COMMENTED         (,nimrod-constants-regexp . font-lock-constant-face)
;; COMMENTED         (,nimrod-builtins-regexp . font-lock-builtin-face)
;; COMMENTED         (,nimrod-decimal-regexp . font-lock-constant-face)
;; COMMENTED         (,nimrod-hex-regexp . font-lock-constant-face)
;; COMMENTED         (,nimrod-octal-regexp . font-lock-constant-face)
;; COMMENTED         (,nimrod-binary-regexp . font-lock-constant-face)
;; COMMENTED         (,nimrod-operators-regexp . font-lock-variable-name-face)
;; COMMENTED         (,nimrod-variables-regexp . font-lock-variable-name-face)
;; COMMENTED         ))
;; COMMENTED
;; COMMENTED ;; Free memory
;; COMMENTED (defvar nimrod-character-literal-regexp nil)
;; COMMENTED (defvar nimrod-raw-string-regexp nil)
;; COMMENTED (defvar nimrod-triple-quote-string-regexp nil)
;; COMMENTED (defvar nimrod-single-quote-string-regexp nil)
;; COMMENTED (defvar nimrod-tab-regexp nil)
;; COMMENTED (defvar nimrod-keywords-regexp nil)
;; COMMENTED (defvar nimrod-types-regexp nil)
;; COMMENTED (defvar nimrod-constants-regexp nil)
;; COMMENTED (defvar nimrod-builtins-regexp nil)
;; COMMENTED (defvar nimrod-decimal-regexp nil)
;; COMMENTED (defvar nimrod-hex-regexp nil)
;; COMMENTED (defvar nimrod-octal-regexp nil)
;; COMMENTED (defvar nimrod-binary-regexp nil)
;; COMMENTED (defvar nimrod-operators-regexp nil)
;; COMMENTED (defvar nimrod-variables-regexp nil)
;; COMMENTED
;; COMMENTED (defun nimrod-setup-font-lock ()
;; COMMENTED   "This will be called when defining nimrod-node, below."
;; COMMENTED   (setq font-lock-defaults '((nimrod-font-lock-keywords))))
;; COMMENTED
;; COMMENTED
;; COMMENTED
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED ;;                                Comments                                    ;;
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED
;; COMMENTED ;; the command to comment/uncomment text
;; COMMENTED (defun nimrod-comment-dwim (arg)
;; COMMENTED   "Comment or uncomment current line or region in a smart way.
;; COMMENTED For detail, see `comment-dwim'."
;; COMMENTED   (interactive "*P")
;; COMMENTED   (require 'newcomment)
;; COMMENTED   (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
;; COMMENTED     (comment-dwim arg)))
;; COMMENTED
;; COMMENTED
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED ;;                               Indentation                                  ;;
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED
;; COMMENTED ;; Desired indentation logic:
;; COMMENTED ;; 1. When a newline is entered, or <TAB> is pressed, do:
;; COMMENTED ;;
;; COMMENTED ;;    1. If this line is all comment (i.e. ^\w*#.*$ ),
;; COMMENTED ;;
;; COMMENTED ;;       1. Find previous non-blank line.
;; COMMENTED ;;
;; COMMENTED ;;       2. If contains comment, set expected indentation to there.
;; COMMENTED ;;          Else, set to first non-whitespace character.
;; COMMENTED ;;
;; COMMENTED ;;    2. Else if this line starts as a string (i.e. ^\w*\".*$ ),
;; COMMENTED ;;       Find expected indentation based on previous line,
;; COMMENTED ;;       ignoring blank lines between:
;; COMMENTED ;;
;; COMMENTED
;; COMMENTED ;; -- Indent to previous # character if we're on a commented line
;; COMMENTED ;; -- Indent to previous start-of-string if line starts with string
;; COMMENTED
;; COMMENTED ;;; TODO: indent after object
;; COMMENTED ;;; TODO: unindent after else:
;; COMMENTED
;; COMMENTED
;; COMMENTED (defconst nimrod-indent-offset 2 "Number of spaces per level of indentation.")
;; COMMENTED
;; COMMENTED (defun nimrod-skip-blank-lines ()
;; COMMENTED   (progn
;; COMMENTED     (forward-line -1)                                  ;; Go back one line.
;; COMMENTED     (while (and (looking-at nimrod-blank-line-regexp)  ;; While it's a blank line,
;; COMMENTED                 (> (point) (point-min)))               ;; and there are other lines,
;; COMMENTED       (forward-line -1))))                             ;; skip back.
;; COMMENTED
;; COMMENTED (defun nimrod-compute-indentation-of-char (char)
;; COMMENTED   ""
;; COMMENTED   (progn
;; COMMENTED     (nimrod-skip-blank-lines)
;; COMMENTED     (skip-chars-forward (concat "^\n" char))
;; COMMENTED     (if (looking-at char)
;; COMMENTED         (current-column)
;; COMMENTED       (+ (progn
;; COMMENTED            (beginning-of-line)
;; COMMENTED            (if (looking-at nimrod-new-block-regexp) nimrod-indent-offset 0))
;; COMMENTED          (current-indentation)))))
;; COMMENTED
;; COMMENTED (defun nimrod-compute-indentation ()
;; COMMENTED   "Calculate the maximum sensible indentation for the current line."
;; COMMENTED   (save-excursion
;; COMMENTED     (beginning-of-line)
;; COMMENTED
;; COMMENTED     (cond ((looking-at "^ *#")  (nimrod-compute-indentation-of-char "#" )) ;; Comment line; look for a comment
;; COMMENTED           ((looking-at "^ *\"") (nimrod-compute-indentation-of-char "\"")) ;; String
;; COMMENTED           ((looking-at "^ *'")  (nimrod-compute-indentation-of-char "'" )) ;; Char
;; COMMENTED           (t                   (progn
;; COMMENTED                                  (nimrod-skip-blank-lines)
;; COMMENTED                                  (+ (current-indentation)
;; COMMENTED                                     (if (looking-at nimrod-new-block-regexp) nimrod-indent-offset 0)))))))
;; COMMENTED
;; COMMENTED
;; COMMENTED (defun nimrod-indent-line ()
;; COMMENTED   "Indent the current line.  The first time this command is used, the line
;; COMMENTED will be indented to the maximum sensible indentation.
;; COMMENTED Each immediately subsequent usage will back-dent the line by
;; COMMENTED `nimrod-indent-offset' spaces.
;; COMMENTED On reaching column 0, it will cycle back to the maximum sensible indentation."
;; COMMENTED
;; COMMENTED   (interactive "*")
;; COMMENTED
;; COMMENTED   (let ((ci (current-indentation))
;; COMMENTED         (cc (current-column))
;; COMMENTED         (need (nimrod-compute-indentation)))
;; COMMENTED
;; COMMENTED     (save-excursion
;; COMMENTED       (beginning-of-line)
;; COMMENTED       (delete-horizontal-space)
;; COMMENTED       (if (and (equal last-command this-command) (/= ci 0))
;; COMMENTED           (indent-to (* (/ (- ci 1) nimrod-indent-offset) nimrod-indent-offset))
;; COMMENTED         (indent-to need)))
;; COMMENTED
;; COMMENTED     (if (< (current-column) (current-indentation))
;; COMMENTED         (forward-to-indentation 0))))
;; COMMENTED
;; COMMENTED
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED ;;                             Wrap it all up ...                             ;;
;; COMMENTED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENTED
;; COMMENTED (define-derived-mode nimrod-mode prog-mode
;; COMMENTED   "nimrod mode"
;; COMMENTED   "A major mode for the Nimrod programming language."
;; COMMENTED
;; COMMENTED   (setq mode-name "Nimrod")  ;; This will display in the mode line.
;; COMMENTED
;; COMMENTED   (nimrod-setup-font-lock)
;; COMMENTED
;; COMMENTED   ;; modify the keymap
;; COMMENTED   (define-key nimrod-mode-map [remap comment-dwim] 'nimrod-comment-dwim)
;; COMMENTED   (define-key nimrod-mode-map (kbd "M-.") 'nimrod-goto-sym)
;; COMMENTED   (define-key nimrod-mode-map (kbd "C-c h") 'nimrod-explain-sym)
;; COMMENTED
;; COMMENTED   (set (make-local-variable 'indent-line-function) 'nimrod-indent-line)
;; COMMENTED
;; COMMENTED   ;; Documentation comment highlighting
;; COMMENTED   ;; (modify-syntax-entry ?\# ". 12b" nimrod-mode-syntax-table)
;; COMMENTED   ;; (modify-syntax-entry ?\n "> b" nimrod-mode-syntax-table)
;; COMMENTED
;; COMMENTED   ;; Comment highlighting
;; COMMENTED   (modify-syntax-entry ?# "< b"  nimrod-mode-syntax-table)
;; COMMENTED   (modify-syntax-entry ?\n "> b" nimrod-mode-syntax-table)
;; COMMENTED
;; COMMENTED   (modify-syntax-entry ?\' "w"  nimrod-mode-syntax-table)
;; COMMENTED   (modify-syntax-entry ?\" "|"  nimrod-mode-syntax-table)
;; COMMENTED
;; COMMENTED   (modify-syntax-entry ?\[ "("  nimrod-mode-syntax-table)
;; COMMENTED   (modify-syntax-entry ?\] ")"  nimrod-mode-syntax-table)
;; COMMENTED
;; COMMENTED   (setq indent-tabs-mode nil) ;; Always indent with SPACES!
;; COMMENTED )
;; COMMENTED
;; COMMENTED (defcustom nimrod-compiled-buffer-name "*nimrod-js*"
;; COMMENTED   "The name of the scratch buffer used to compile Javascript from Nimrod."
;; COMMENTED   :type 'string
;; COMMENTED   :group 'nimrod)
;; COMMENTED
;; COMMENTED (defcustom nimrod-command "nimrod"
;; COMMENTED   "Path to the nimrod executable. You don't need to set this if
;; COMMENTED the nimrod executable is inside your PATH."
;; COMMENTED   :type 'string
;; COMMENTED   :group 'nimrod)
;; COMMENTED
;; COMMENTED (defcustom nimrod-args-compile '()
;; COMMENTED   "The arguments to pass to `nimrod-command' to compile a file."
;; COMMENTED   :type 'list
;; COMMENTED   :group 'nimrod)
;; COMMENTED
;; COMMENTED (defcustom nimrod-type-abbrevs '(
;; COMMENTED                                  ("skProc" . "f")
;; COMMENTED                                  ("skIterator" . "i")
;; COMMENTED                                  ("skTemplate" . "T")
;; COMMENTED                                  ("skType" . "t")
;; COMMENTED                                  ("skMethod" . "f")
;; COMMENTED                                  ("skEnumField" . "e")
;; COMMENTED                                  ("skGenericParam" . "p")
;; COMMENTED                                  ("skParam" . "p")
;; COMMENTED                                  ("skModule" . "m")
;; COMMENTED                                  ("skConverter" . "C")
;; COMMENTED                                  ("skMacro" . "M")
;; COMMENTED                                  ("skField" . "F")
;; COMMENTED                                  ("skForVar" . "v")
;; COMMENTED                                  ("skVar" . "v")
;; COMMENTED                                  ("skLet" . "v")
;; COMMENTED                                  ("skLabel" . "l")
;; COMMENTED                                  ("skConst" . "c")
;; COMMENTED                                  ("skResult" . "r")
;; COMMENTED                                  )
;; COMMENTED   "Abbrevs for auto-complete."
;; COMMENTED   :type 'assoc
;; COMMENTED   :group 'nimrod)
;; COMMENTED
;; COMMENTED (defvar nimrod-idetools-modes '(suggest def context usages)
;; COMMENTED   "Which modes are available to use with the idetools.")
;; COMMENTED
;; COMMENTED (defun nimrod-compile-file-to-js (&optional callback)
;; COMMENTED   "Saves current file and compiles it. Uses the project
;; COMMENTED directory, so it will work best with external libraries where
;; COMMENTED `nimrod-compile-region-to-js` does not. Returns the filename of
;; COMMENTED the compiled file. The callback is executed on success with the
;; COMMENTED filename of the compiled file."
;; COMMENTED   (interactive)
;; COMMENTED   (save-buffer)
;; COMMENTED   (let ((default-directory (or (nimrod-get-project-root) default-directory)))
;; COMMENTED     (lexical-let ((callback callback))
;; COMMENTED       (nimrod-compile (list "js" (buffer-file-name))
;; COMMENTED                       (lambda () (when callback
;; COMMENTED                               (funcall callback (concat default-directory
;; COMMENTED                                                         "nimcache/"
;; COMMENTED                                                         (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
;; COMMENTED                                                         ".js"))))))))
;; COMMENTED
;; COMMENTED (defun nimrod-compile-region-to-js (start end)
;; COMMENTED   "Compiles the current region to javascript into the buffer
;; COMMENTED `nimrod-compiled-buffer-name'."
;; COMMENTED   (interactive "r")
;; COMMENTED
;; COMMENTED   (lexical-let ((buffer (get-buffer-create nimrod-compiled-buffer-name))
;; COMMENTED                 (tmpdir (file-name-as-directory (make-temp-file "nimrod-compile" t))))
;; COMMENTED     (let ((default-directory tmpdir))
;; COMMENTED       (write-region start end "tmp.nim" nil 'foo)
;; COMMENTED       (with-current-buffer buffer
;; COMMENTED         (erase-buffer)
;; COMMENTED         (let ((default-directory tmpdir))
;; COMMENTED           (nimrod-compile '("js" "tmp.nim")
;; COMMENTED                           (lambda () (with-current-buffer buffer
;; COMMENTED                                   (insert-file
;; COMMENTED                                    (concat tmpdir (file-name-as-directory "nimcache") "tmp.js"))
;; COMMENTED                                   (display-buffer buffer)))))))))
;; COMMENTED
;; COMMENTED (defun nimrod-compile (args &optional on-success)
;; COMMENTED   "Invokes the compiler and calls on-success in case of
;; COMMENTED successful compile."
;; COMMENTED   (lexical-let ((on-success (or on-success (lambda () (message "Compilation successful.")))))
;; COMMENTED     (if (bufferp "*nimrod-compile*")
;; COMMENTED         (with-current-buffer "*nimrod-compile*"
;; COMMENTED           (erase-buffer)))
;; COMMENTED     (set-process-sentinel
;; COMMENTED      (apply
;; COMMENTED       (apply-partially 'start-file-process "nimrod" "*nimrod-compile*" nimrod-command)
;; COMMENTED       (append nimrod-args-compile args))
;; COMMENTED      (lambda (process-name status)
;; COMMENTED        (cond ((string= status "finished\n")
;; COMMENTED               (when on-success
;; COMMENTED                 (funcall on-success)))
;; COMMENTED              ((string= status "exited abnormally with code 1\n")
;; COMMENTED               (display-buffer "*nimrod-compile*"))
;; COMMENTED              (t (error status)))))))
;; COMMENTED
;; COMMENTED (defun nimrod-ac-enable ()
;; COMMENTED   "Enable Autocompletion. Default settings. If you don't like
;; COMMENTED them, kick this hook with
;; COMMENTED  `(remove-hook 'nimrod-mode-hook 'nimrod-ac-enable)`
;; COMMENTED and write your own. I discurage using autostart, as the
;; COMMENTED completion candidates need to be loaded from outside emacs."
;; COMMENTED   (when (not (executable-find nimrod-command))
;; COMMENTED     (error "NimRod executable not found. Please customize nimrod-command"))
;; COMMENTED
;; COMMENTED   (make-local-variable 'ac-sources)
;; COMMENTED   (setq ac-sources '(ac-source-nimrod-completions))
;; COMMENTED
;; COMMENTED   (make-local-variable 'ac-use-comphist)
;; COMMENTED   (setq ac-use-comphist nil)
;; COMMENTED
;; COMMENTED   (make-local-variable 'ac-use-quick-help)
;; COMMENTED   (setq ac-use-quick-help t)
;; COMMENTED
;; COMMENTED   (make-local-variable 'ac-delete-dups)
;; COMMENTED   (setq ac-delete-dups nil)
;; COMMENTED
;; COMMENTED   (make-local-variable 'ac-ignore-case)
;; COMMENTED   (setq ac-ignore-case t)
;; COMMENTED
;; COMMENTED   (make-local-variable 'ac-auto-show-menu)
;; COMMENTED   (setq ac-auto-show-menu 0.5)
;; COMMENTED
;; COMMENTED   (make-local-variable 'ac-auto-start)
;; COMMENTED   (setq ac-auto-start nil)
;; COMMENTED
;; COMMENTED   (make-local-variable 'ac-trigger-key)
;; COMMENTED   (setq ac-trigger-key "TAB")
;; COMMENTED
;; COMMENTED   (auto-complete-mode)
;; COMMENTED )
;; COMMENTED
;; COMMENTED (add-hook 'nimrod-mode-hook 'nimrod-ac-enable)
;; COMMENTED
;; COMMENTED ;;; Some copy/paste from ensime.
;; COMMENTED (ac-define-source nimrod-completions
;; COMMENTED   '((candidates . (nimrod-ac-completion-candidates ac-prefix))
;; COMMENTED     (prefix . nimrod-ac-completion-prefix)
;; COMMENTED     (action . (lambda ()))                   ; TODO
;; COMMENTED     (requires . 0)
;; COMMENTED     ))
;; COMMENTED
;; COMMENTED (defun nimrod-ac-completion-prefix ()
;; COMMENTED   "Starting at current point, find the point of completion."
;; COMMENTED   (let ((point (re-search-backward "\\(\\W\\|[\t ]\\)\\([^\\. ]*\\)?"
;; COMMENTED 				   (point-at-bol) t)))
;; COMMENTED     (if point (1+ point))))
;; COMMENTED
;; COMMENTED (defun nimrod-ac-completion-candidates (prefix)
;; COMMENTED   (let ((suggestions (nimrod-call-and-parse-idetools 'suggest)))
;; COMMENTED     (mapcar (lambda (entry)
;; COMMENTED               (propertize (nimrod-ide-name entry)
;; COMMENTED                           'value entry
;; COMMENTED                           'symbol (assoc-default (nimrod-ide-type entry)
;; COMMENTED                                                  nimrod-type-abbrevs)
;; COMMENTED                           'type-sig (nimrod-ide-signature entry)
;; COMMENTED                           'summary (nimrod-ac-trunc-summary (nimrod-ide-comment entry))
;; COMMENTED                           ))
;; COMMENTED             suggestions)))
;; COMMENTED
;; COMMENTED ;;; Copy/pasted from ensime
;; COMMENTED (defun nimrod-ac-trunc-summary (str)
;; COMMENTED   (let ((len (length str)))
;; COMMENTED     (if (> len 40)
;; COMMENTED 	(concat (substring str 0 40) "...")
;; COMMENTED       str)))
;; COMMENTED
;; COMMENTED (defun nimrod-call-and-parse-idetools (mode)
;; COMMENTED   "Call idetools and get `nimrod-ide' structs back."
;; COMMENTED   (nimrod-parse-idetools-buffer (nimrod-call-idetools mode)))
;; COMMENTED
;; COMMENTED (defstruct nimrod-ide type namespace name signature path line column comment)
;; COMMENTED
;; COMMENTED (defun nimrod-parse-idetools-buffer (buffer)
;; COMMENTED   "Returns a list of `nimrod-ide' structs, based on the contents of `buffer'."
;; COMMENTED   (with-current-buffer buffer
;; COMMENTED     (mapcar (lambda (line)
;; COMMENTED               (destructuring-bind (_ type fn sig path line col comment) (split-string line "\t")
;; COMMENTED                 (make-nimrod-ide
;; COMMENTED                  :type type
;; COMMENTED                  :namespace (first (split-string fn "\\."))
;; COMMENTED                  :name (second (split-string fn "\\."))
;; COMMENTED                  :signature sig
;; COMMENTED                  :path path
;; COMMENTED                  :line (string-to-number line)
;; COMMENTED                  :column (string-to-number col)
;; COMMENTED                  :comment comment)))
;; COMMENTED             (split-string (buffer-string) "[\r\n]" t))))
;; COMMENTED
;; COMMENTED (defun nimrod-call-idetools (mode)
;; COMMENTED   "ARGS should be one of `nimrod-idetools-modes'. Grab the data
;; COMMENTED from the returned buffer."
;; COMMENTED   (when (not (memq mode nimrod-idetools-modes))
;; COMMENTED     (error (concat mode " not one from `nimrod-idetools-modes'.")))
;; COMMENTED   (let ((tempfile (nimrod-save-buffer-temporarly))
;; COMMENTED         (file (buffer-file-name))
;; COMMENTED         (buffer (get-buffer-create (format "*nimrod-idetools-%s*" mode))))
;; COMMENTED     ;; There can only be one. Useful for suggest, not sure about the
;; COMMENTED     ;; other modes. Change as needed.
;; COMMENTED     (when (bufferp buffer)
;; COMMENTED       (with-current-buffer buffer
;; COMMENTED         (erase-buffer)))
;; COMMENTED     (let ((args (append (list nimrod-command nil (list buffer (concat temporary-file-directory "nimrod-idetools-stderr")) nil)
;; COMMENTED                    (remove nil (list
;; COMMENTED                                 "idetools"
;; COMMENTED                                 "--stdout"
;; COMMENTED                                 (nimrod-format-cursor-position file tempfile) ; --trackDirty
;; COMMENTED                                 (when (nimrod-get-project-root)
;; COMMENTED                                   (format "--include:%s" (nimrod-get-project-root)))
;; COMMENTED                                 (concat "--" (symbol-name mode))
;; COMMENTED                                 ;; in case of no project main file, use the tempfile. Might be
;; COMMENTED                                 ;; useful for repl.
;; COMMENTED                                 (or (nimrod-get-project-main-file) tempfile))))))
;; COMMENTED       ;; (message (format "%S" args))      ; Debugging
;; COMMENTED       (apply 'call-process args))
;; COMMENTED     (delete-directory (file-name-directory tempfile) t)
;; COMMENTED     buffer))
;; COMMENTED
;; COMMENTED (defun nimrod-save-buffer-temporarly ()
;; COMMENTED   "This saves the current buffer and returns the location, so we
;; COMMENTED   can pass it to idetools."
;; COMMENTED   (let* ((dirname (make-temp-file "nimrod-suggest" t))
;; COMMENTED          (filename (concat (file-name-as-directory dirname)
;; COMMENTED                            (file-name-nondirectory (buffer-file-name)))))
;; COMMENTED     (save-restriction
;; COMMENTED       (widen)
;; COMMENTED       (write-region (point-min) (point-max) filename) nil 'foo)
;; COMMENTED     filename))
;; COMMENTED
;; COMMENTED ;; From http://stackoverflow.com/questions/14095189/walk-up-the-directory-tree
;; COMMENTED
;; COMMENTED (defun nimrod-parent-directory (dir)
;; COMMENTED   (unless (equal "/" dir)
;; COMMENTED     (file-name-directory (directory-file-name dir))))
;; COMMENTED
;; COMMENTED (defun nimrod-find-file-in-heirarchy (current-dir pattern)
;; COMMENTED   "Search for a file matching PATTERN upwards through the directory
;; COMMENTED hierarchy, starting from CURRENT-DIR"
;; COMMENTED   (let ((parent (nimrod-parent-directory (expand-file-name current-dir))))
;; COMMENTED     (or (directory-files current-dir t pattern nil)
;; COMMENTED       (when parent
;; COMMENTED         (nimrod-find-file-in-heirarchy parent pattern)))))
;; COMMENTED
;; COMMENTED (defun nimrod-get-project-main-file ()
;; COMMENTED   "Get the main file for the project."
;; COMMENTED   (let ((main-file (nimrod-find-file-in-heirarchy
;; COMMENTED                 (file-name-directory (buffer-file-name))
;; COMMENTED                 ".*\.nimrod\.cfg")))
;; COMMENTED     (when main-file (concat
;; COMMENTED                      (replace-regexp-in-string "\.nimrod\.cfg$" "" (first main-file))
;; COMMENTED                      ".nim"))))
;; COMMENTED
;; COMMENTED (defun nimrod-get-project-root ()
;; COMMENTED   "Get the project root. Uses `nimrod-get-project-main-file' or git. "
;; COMMENTED   (or (let ((main-file (nimrod-get-project-main-file)))
;; COMMENTED         (when main-file (file-name-directory main-file)))
;; COMMENTED       (let ((git-output (replace-regexp-in-string "\n$" ""
;; COMMENTED                                         (with-output-to-string
;; COMMENTED                                           (with-current-buffer
;; COMMENTED                                               standard-output
;; COMMENTED                                             (process-file shell-file-name nil (list t nil) nil shell-command-switch "git rev-parse --show-toplevel"))))))
;; COMMENTED         (if (< 0 (length git-output))
;; COMMENTED             git-output
;; COMMENTED           nil))))
;; COMMENTED
;; COMMENTED (defun nimrod-format-cursor-position (file tempfile)
;; COMMENTED   "Formats the position of the cursor to a nice little
;; COMMENTED --trackDirty statement, referencing the file in the temprorary
;; COMMENTED directory."
;; COMMENTED   (format "--trackDirty:%s,%s,%d,%d" tempfile file (line-number-at-pos) (current-column)))
;; COMMENTED
;; COMMENTED (defun nimrod-goto-sym ()
;; COMMENTED   "Go to the definition of the symbol currently under the cursor."
;; COMMENTED   (interactive)
;; COMMENTED   (let ((def (first (nimrod-call-and-parse-idetools 'def))))
;; COMMENTED     (when (not def) (error "Symbol not found."))
;; COMMENTED     (find-file (nimrod-ide-path def))
;; COMMENTED     (goto-line (nimrod-ide-line def))))
;; COMMENTED
;; COMMENTED (provide 'nimrod-mode)
;; COMMENTED
;; COMMENTED (setq auto-mode-alist (cons '("\\.nim$" . nimrod-mode) auto-mode-alist))
;; COMMENTED
;; COMMENTED ;;; nimrod-mode.el ends here
