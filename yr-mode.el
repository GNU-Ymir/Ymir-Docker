;;; yr-mode.el --- A major emacs mode for editing Yr source code -*-lexical-binding: t-*-

;; Version: 0.3.0
;; Author: Mozilla
;; Url: https://github.com/yr-lang/yr-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24.0"))

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Commentary:
;;

;;; Code:

(eval-when-compile (require 'rx)
                   (require 'compile)
                   (require 'url-vars))

(defvar electric-pair-inhibit-predicate)
(defvar electric-indent-chars)

;; for GNU Emacs < 24.3
(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defconst yr-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
(defconst yr-re-lc-ident "[[:lower:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
(defconst yr-re-uc-ident "[[:upper:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
(defconst yr-re-vis "pub")
(defconst yr-re-unsafe "unsafe")
(defconst yr-re-extern "extern")

;;; Start of a Yr item
(defvar yr-top-item-beg-re
  (concat "\\s-*\\(?:priv\\|pub\\)?\\s-*"
          (regexp-opt
           '("enum" "struct" "type" "mod" "import" "def" "static" "impl" "over"
             "extern" "trait"))
	  "\\_>"))

(defun yr-looking-back-str (str)
  "Like `looking-back' but for fixed strings rather than regexps (so that it's not so slow)"
  (let ((len (length str)))
    (and (> (point) len)
         (equal str (buffer-substring-no-properties (- (point) len) (point))))))

(defun yr-looking-back-symbols (SYMS)
  "Return non-nil if the point is just after a complete symbol that is a member of the list of strings SYMS"
  (save-excursion
    (let* ((pt-orig (point))
           (beg-of-symbol (progn (forward-thing 'symbol -1) (point)))
           (end-of-symbol (progn (forward-thing 'symbol 1) (point))))
      (and
       (= end-of-symbol pt-orig)
       (member (buffer-substring-no-properties beg-of-symbol pt-orig) SYMS)))))

(defun yr-looking-back-ident ()
  "Non-nil if we are looking backwards at a valid yr identifier"
  (let ((beg-of-symbol (save-excursion (forward-thing 'symbol -1) (point))))
    (looking-back yr-re-ident beg-of-symbol)))

(defun yr-looking-back-macro ()
  "Non-nil if looking back at an ident followed by a !"
  (save-excursion (backward-char) (and (= ?! (char-after)) (yr-looking-back-ident))))

;; Syntax definitions and helpers
(defvar yr-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\' "\'" table)

    ;; Angle brackets.  We suppress this with syntactic propertization
    ;; when needed
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(defgroup yr-mode nil
  "Support for Yr code."
  :link '(url-link "https://www.yr-lang.org/")
  :group 'languages)

(defcustom yr-indent-offset 4
  "Indent Yr code by this number of spaces."
  :type 'integer
  :group 'yr-mode
  :safe #'integerp)

(defcustom yr-indent-method-chain nil
  "Indent Yr method chains, aligned by the '.' operators"
  :type 'boolean
  :group 'yr-mode
  :safe #'booleanp)

(defcustom yr-indent-where-claimport t
  "Indent the line starting with the where keyword following a
function or trait.  When nil, where will be aligned with def or trait."
  :type 'boolean
  :group 'yr-mode
  :safe #'booleanp)

(defcustom yr-playpen-url-format "https://play.yr-lang.org/?code=%s"
  "Format string to import when submitting code to the playpen"
  :type 'string
  :group 'yr-mode)
(defcustom yr-shortener-url-format "https://is.gd/create.php?format=simple&url=%s"
  "Format string to import for creating the shortened link of a playpen submission"
  :type 'string
  :group 'yr-mode)

(defcustom yr-match-angle-brackets t
  "Enable angle bracket matching.  Attempt to match `<' and `>' where
  appropriate."
  :type 'boolean
  :safe #'booleanp
  :group 'yr-mode)

(defcustom yr-format-on-save nil
  "Format future yr buffers before saving using yrfmt."
  :type 'boolean
  :safe #'booleanp
  :group 'yr-mode)

(defcustom yr-yrfmt-bin "yrfmt"
  "Path to yrfmt executable."
  :type 'string
  :group 'yr-mode)

(defface yr-unsafe-face
  '((t :inherit font-lock-warning-face))
  "Face for the `unsafe' keyword."
  :group 'yr-mode)

(defun yr-paren-level () (nth 0 (syntax-ppss)))
(defun yr-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun yr-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))

(defun yr-rewind-irrelevant ()
  (let ((continue t))
    (while continue
      (let ((starting (point)))
        (skip-chars-backward "[:space:]\n")
        (when (yr-looking-back-str "*/")
          (backward-char))
        (when (yr-in-str-or-cmnt)
          (yr-rewind-past-str-cmnt))
        ;; Rewind until the point no longer moves
        (setq continue (/= starting (point)))))))


(defun yr-in-macro ()
  (save-excursion
    (when (> (yr-paren-level) 0)
      (backward-up-list)
      (yr-rewind-irrelevant)
      (or (yr-looking-back-macro)
          (and (yr-looking-back-ident) (save-excursion (backward-sexp) (yr-rewind-irrelevant) (yr-looking-back-str "macro_rules!")))
          (yr-in-macro))
      )))

(defun yr-looking-at-where ()
  "Return T when looking at the \"where\" keyword."
  (and (looking-at-p "\\bwhere\\b")
       (not (yr-in-str-or-cmnt))))

(defun yr-rewind-to-where (&optional limit)
  "Rewind the point to the closest occurrence of the \"where\" keyword.
Return T iff a where-claimport was found.  Does not rewind past
LIMIT when passed, otherwise only stops at the beginning of the
buffer."
  (when (re-search-backward "\\bwhere\\b" limit t)
    (if (yr-in-str-or-cmnt)
        (yr-rewind-to-where limit)
      t)))

(defun yr-align-to-expr-after-brace ()
  (save-excursion
    (forward-char)
    ;; We don't want to indent out to the open bracket if the
    ;; open bracket ends the line
    (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
      (when (looking-at "[[:space:]]")
    (forward-word 1)
    (backward-word 1))
      (current-column))))

(defun yr-rewind-to-beginning-of-current-level-expr ()
  (let ((current-level (yr-paren-level)))
    (back-to-indentation)
    (when (looking-at "->")
      (yr-rewind-irrelevant)
      (back-to-indentation))
    (while (> (yr-paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))
    ;; When we're in the where claimport, skip over it.  First find out the start
    ;; of the function and its paren level.
    (let ((function-start nil) (function-level nil))
      (save-excursion
        (yr-beginning-of-defun)
        (back-to-indentation)
        ;; Avoid using multiple-value-bind
        (setq function-start (point)
              function-level (yr-paren-level)))
      ;; On a where claimport
      (when (or (yr-looking-at-where)
                ;; or in one of the following lines, e.g.
                ;; where A: Eq
                ;;       B: Hash <- on this line
                (and (save-excursion
                       (yr-rewind-to-where function-start))
                     (= current-level function-level)))
        (goto-char function-start)))))

(defun yr-align-to-method-chain ()
  (save-excursion
    ;; for method-chain alignment to apply, we must be looking at
    ;; another method call or field access or something like
    ;; that. This avoids rather "eager" jumps in situations like:
    ;;
    ;; {
    ;;     something.foo()
    ;; <indent>
    ;;
    ;; Without this check, we would wind up with the cursor under the
    ;; `.`. In an older version, I had the inverse of the current
    ;; check, where we checked for situations that should NOT indent,
    ;; vs checking for the one situation where we SHOULD. It should be
    ;; clear that this is more robust, but also I find it mildly less
    ;; annoying to have to press tab again to align to a method chain
    ;; than to have an over-eager indent in all other cases which must
    ;; be undone via tab.

    (when (looking-at (concat "\s*\." yr-re-ident))
      (forward-line -1)
      (end-of-line)
      ;; Keep going up (looking for a line that could contain a method chain)
      ;; while we're in a comment or on a blank line. Stop when the paren
      ;; level changes.
      (let ((level (yr-paren-level)))
        (while (and (or (yr-in-str-or-cmnt)
                        ;; Only whitespace (or nothing) from the beginning to
                        ;; the end of the line.
                        (looking-back "^\s*" (point-at-bol)))
                    (= (yr-paren-level) level))
          (forward-line -1)
          (end-of-line)))

      (let
          ;; skip-dot-identifier is importd to position the point at the
          ;; `.` when looking at something like
          ;;
          ;;      foo.bar
          ;;         ^   ^
          ;;         |   |
          ;;         |  position of point
          ;;       returned offset
          ;;
          ((skip-dot-identifier
            (lambda ()
              (when (and (yr-looking-back-ident) (save-excursion (forward-thing 'symbol -1) (= ?. (char-before))))
                (forward-thing 'symbol -1)
                (backward-char)
                (- (current-column) yr-indent-offset)))))
        (cond
         ;; foo.bar(...)
         ((yr-looking-back-str ")")
          (backward-list 1)
          (funcall skip-dot-identifier))

         ;; foo.bar
         (t (funcall skip-dot-identifier)))))))

(defun yr-mode-indent-line ()
  (interactive)
  (let ((indent
         (save-excursion
           (back-to-indentation)
           ;; Point is now at beginning of current line
           (let* ((level (yr-paren-level))
                  (baseline
                   ;; Our "baseline" is one level out from the indentation of the expression
                   ;; containing the innermost enclosing opening bracket.  That
                   ;; way if we are within a block that has a different
                   ;; indentation than this mode would give it, we still indent
                   ;; the inside of it correctly relative to the outside.
                   (if (= 0 level)
                       0
                     (or
                      (when yr-indent-method-chain
                        (yr-align-to-method-chain))
                      (save-excursion
                        (yr-rewind-irrelevant)
                        (backward-up-list)
                        (yr-rewind-to-beginning-of-current-level-expr)
                        (+ (current-column) yr-indent-offset))))))
             (cond
              ;; Indent inside a non-raw string only if the the previous line
              ;; ends with a backslash that is inside the same string
              ((nth 3 (syntax-ppss))
               (let*
                   ((string-begin-pos (nth 8 (syntax-ppss)))
                    (end-of-prev-line-pos (when (> (line-number-at-pos) 1)
                                            (save-excursion
                                              (forward-line -1)
                                              (end-of-line)
                                              (point)))))
                 (when
                     (and
                      ;; If the string begins with an "r" it's a raw string and
                      ;; we should not change the indentation
                      (/= ?r (char-after string-begin-pos))

                      ;; If we're on the first line this will be nil and the
                      ;; rest does not apply
                      end-of-prev-line-pos

                      ;; The end of the previous line needs to be inside the
                      ;; current string...
                      (> end-of-prev-line-pos string-begin-pos)

                      ;; ...and end with a backslash
                      (= ?\\ (char-before end-of-prev-line-pos)))

                   ;; Indent to the same level as the previous line, or the
                   ;; start of the string if the previous line starts the string
                   (if (= (line-number-at-pos end-of-prev-line-pos) (line-number-at-pos string-begin-pos))
                       ;; The previous line is the start of the string.
                       ;; If the backslash is the only character after the
                       ;; string beginning, indent to the next indent
                       ;; level.  Otherwise align with the start of the string.
                       (if (> (- end-of-prev-line-pos string-begin-pos) 2)
                           (save-excursion
                             (goto-char (+ 1 string-begin-pos))
                             (current-column))
                         baseline)

                     ;; The previous line is not the start of the string, so
                     ;; match its indentation.
                     (save-excursion
                       (goto-char end-of-prev-line-pos)
                       (back-to-indentation)
                       (current-column))))))

              ;; ;; A function return type is indented to the corresponding function arguments
              ;; ((looking-at "->")
              ;;  (save-excursion
              ;;    (backward-list)
              ;;    (or (yr-align-to-expr-after-brace)
              ;;        (+ baseline yr-indent-offset))))

              ((looking-at "->") (- 1 baseline ))
	      
              ((looking-at "[]|)]") (- baseline))
	      
              ;; A closing brace is 1 level unindented
              ((looking-at "[]})]") (- baseline yr-indent-offset))

              ;; Doc comments in /** style with leading * indent to line up the *s
              ((and (nth 4 (syntax-ppss)) (looking-at "*"))
               (+ 1 baseline))

              ;; When the importr chose not to indent the start of the where
              ;; claimport, put it on the baseline.
              ((and (not yr-indent-where-claimport)
                    (yr-looking-at-where))
               baseline)

              ;; If we're in any other token-tree / sexp, then:
              (t
               (or
                ;; If we are inside a pair of braces, with something after the
                ;; open brace on the same line and ending with a comma, treat
                ;; it as fields and align them.
                (when (> level 0)
                  (save-excursion
                    (yr-rewind-irrelevant)
                    (backward-up-list)
                    ;; Point is now at the beginning of the containing set of braces
                    (yr-align-to-expr-after-brace)))

                ;; When where-claimports are spread over multiple lines, claimports
                ;; should be aligned on the type parameters.  In this case we
                ;; take care of the second and following claimports (the ones
                ;; that don't start with "where ")
                (save-excursion
                  ;; Find the start of the function, we'll import this to limit
                  ;; our search for "where ".
                  (let ((function-start nil) (function-level nil))
                    (save-excursion
                      ;; If we're already at the start of a function,
                      ;; don't go back any farther.  We can easily do
                      ;; this by moving to the end of the line first.
                      (end-of-line)
                      (yr-beginning-of-defun)
                      (back-to-indentation)
                      ;; Avoid using multiple-value-bind
                      (setq function-start (point)
                            function-level (yr-paren-level)))
                    ;; When we're not on a line starting with "where ", but
                    ;; still on a where-claimport line, go to "where "
                    (when (and
                           (not (yr-looking-at-where))
                           ;; We're looking at something like "F: ..."
                           (looking-at (concat yr-re-ident ":"))
                           ;; There is a "where " somewhere after the
                           ;; start of the function.
                           (yr-rewind-to-where function-start)
                           ;; Make sure we're not inside the function
                           ;; already (e.g. initializing a struct) by
                           ;; checking we are the same level.
                           (= function-level level))
                      ;; skip over "where"
                      (forward-char 5)
                      ;; Unless "where" is at the end of the line
                      (if (eolp)
                          ;; in this case the type parameters bounds are just
                          ;; indented once
                          (+ baseline yr-indent-offset)
                        ;; otherwise, skip over whitespace,
                        (skip-chars-forward "[:space:]")
                        ;; get the column of the type parameter and import that
                        ;; as indentation offset
                        (current-column)))))

                (progn
                  (back-to-indentation)
                  ;; Point is now at the beginning of the current line
                  (if (or
                       ;; If this line begins with "else" or "{", stay on the
                       ;; baseline as well (we are continuing an expression,
                       ;; but the "else" or "{" should align with the beginning
                       ;; of the expression it's in.)
                       ;; Or, if this line starts a comment, stay on the
                       ;; baseline as well.
                       (looking-at "\\<else\\>\\|{\\|/[/*]")

                       ;; If this is the start of a top-level item,
                       ;; stay on the baseline.
                       (looking-at yr-top-item-beg-re)

                       (save-excursion
                         (yr-rewind-irrelevant)
                         ;; Point is now at the end of the previous line
                         (or
                          ;; If we are at the start of the buffer, no
                          ;; indentation is needed, so stay at baseline...
                          (= (point) 1)
                          ;; ..or if the previous line ends with any of these:
                          ;;     { ? : ( , ; [ }
                          ;; then we are at the beginning of an expression, so stay on the baseline...
                          (looking-back "[(,:;?[{}]\\|[^|]|" (- (point) 2))
                          ;; or if the previous line is the end of an attribute, stay at the baseline...
                          (progn (yr-rewind-to-beginning-of-current-level-expr) (looking-at "#")))))
                      baseline

                    ;; Otherwise, we are continuing the same expression from the previous line,
                    ;; so add one additional indent level
                    (+ baseline yr-indent-offset))))))))))

    (when indent
      ;; If we're at the beginning of the line (before or at the current
      ;; indentation), jump with the indentation change.  Otherwise, save the
      ;; excursion so that adding the indentations will leave us at the
      ;; equivalent position within the line to where we were before.
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))


;; Font-locking definitions and helpers
(defconst yr-mode-keywords
  '("as"
    "box" "break"
    "const"
    "else" "enum" "extern"
    "def" "for" "is"
    "if" "impl" "in" "over"
    "let" "loop"
    "match" "imut"
    "private" "public"
    "ref" "return" "fn"
    "self" "static" "struct" "super"
    "trait" "type"
    "import" "while"))

(defconst yr-special-types
  '("ubyte" "byte"
    "ushort" "short"
    "uint" "int"
    "ulong" "long"

    "f32" "float"
    "float" "int" "uint" "isize" "usize"
    "bool"
    "string" "char" "tuple" "range"))

(defconst yr-re-type-or-constructor
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(defconst yr-re-pre-expression-operators "[-=!%&*/:<>[{(|.^;}]")
(defun yr-re-word (inner) (concat "\\<" inner "\\>"))
(defun yr-re-grab (inner) (concat "\\(" inner "\\)"))
(defun yr-re-shy (inner) (concat "\\(?:" inner "\\)"))
(defun yr-re-item-def (itype)
  (concat (yr-re-word itype) "[[:space:]]+" (yr-re-grab yr-re-ident)))
(defun yr-re-item-def-imenu (itype)
  (concat "^[[:space:]]*"
          (yr-re-shy (concat (yr-re-word yr-re-vis) "[[:space:]]+")) "?"
          (yr-re-shy (concat (yr-re-word "default") "[[:space:]]+")) "?"
          (yr-re-shy (concat (yr-re-word yr-re-unsafe) "[[:space:]]+")) "?"
          (yr-re-shy (concat (yr-re-word yr-re-extern) "[[:space:]]+"
                               (yr-re-shy "\"[^\"]+\"[[:space:]]+") "?")) "?"
          (yr-re-item-def itype)))

(defconst yr-re-special-types (regexp-opt yr-special-types 'symbols))


(defun yr-path-font-lock-matcher (re-ident)
  "Matches names like \"foo::\" or \"Foo::\" (depending on RE-IDENT, which should match
the desired identifiers), but does not match type annotations \"foo::<\"."
  `(lambda (limit)
     (catch 'yr-path-font-lock-matcher
       (while t
         (let* ((symbol-then-colons (rx-to-string '(seq (group (regexp ,re-ident)) "::")))
                (match (re-search-forward symbol-then-colons limit t)))
           (cond
            ;; If we didn't find a match, there are no more occurrences
            ;; of foo::, so return.
            ((null match) (throw 'yr-path-font-lock-matcher nil))
            ;; If this isn't a type annotation foo::<, we've found a
            ;; match, so a return it!
            ((not (looking-at (rx (0+ space) "<")))
	     (throw 'yr-path-font-lock-matcher match))))))))


(defvar yr-mode-font-lock-keywords
  (append
   `(
     ;; Keywords proper
     ("\\_<\\(default\\)[[:space:]]+def\\_>" 1 font-lock-keyword-face)
     (,(regexp-opt yr-mode-keywords 'symbols) . font-lock-keyword-face)

     ;; The unsafe keyword
     ("\\_<unsafe\\_>" . 'yr-unsafe-face)

     ("\\_<true\\_>" . 'font-lock-constant-face)

     ("\\_<false\\_>" . 'font-lock-constant-face)

     ("\\_<null\\_>" . 'font-lock-constant-face)
     
     ;; Attributes like `#[bar(baz)]` or `#![bar(baz)]` or `#[bar = "baz"]`
     (,(yr-re-grab (concat "#\\!?\\[" yr-re-ident "[^]]*\\]"))
      1 font-lock-preprocessor-face keep)

     ;; Syntax extension invocations like `foo!`, highlight including the !
     (,(concat (yr-re-grab (concat yr-re-ident "!")) "")
      1 font-lock-preprocessor-face)

     ;; Field names like `foo:`, highlight excluding the :
     (,(concat (yr-re-grab yr-re-ident) ":[^:]") 1 font-lock-variable-name-face)

     ;; Field names like `:foo`, highlight excluding the :
     (,(concat "[^:]:[^:]" (yr-re-grab yr-re-ident) ) 1 font-lock-variable-name-face keep)
     
     ;; Type names like `Foo::`, highlight excluding the ::
     (,(yr-path-font-lock-matcher yr-re-uc-ident) 1 font-lock-constante-face)

     ;; Module names like `foo::`, highlight excluding the ::
     (,(yr-path-font-lock-matcher yr-re-lc-ident) 1 font-lock-constant-face)

     ("\\([0-9]+\\)" 1 font-lock-constant-face)
   
     ;; Lifetimes like `'foo`
     (,(concat "'" (yr-re-grab yr-re-ident) "[^']") 1 font-lock-variable-name-face)
     
     ;; CamelCase Means Type Or Constructor
     (,yr-re-type-or-constructor 1 font-lock-type-face)
     )

   ;; Item definitions
   (mapcar #'(lambda (x)
               (list (yr-re-item-def (car x))
                     1 (cdr x)))
           '(("enum" . font-lock-type-face)
             ("struct" . font-lock-type-face)
             ("type" . font-lock-type-face)
             ("mod" . font-lock-constant-face)
             ("import" . font-lock-constant-face)
             ("def" . font-lock-function-name-face)))))

(defun yr-syntax-class-before-point ()
  (when (> (point) 1)
    (syntax-class (syntax-after (1- (point))))))

(defun yr-rewind-qualified-ident ()
  (while (yr-looking-back-ident)
    (backward-sexp)
    (when (save-excursion (yr-rewind-irrelevant) (yr-looking-back-str "::"))
      (yr-rewind-irrelevant)
      (backward-char 2)
      (yr-rewind-irrelevant))))

(defun yr-rewind-type-param-list ()
  (cond
   ((and (yr-looking-back-str ">") (equal 5 (yr-syntax-class-before-point)))
    (backward-sexp)
    (yr-rewind-irrelevant))

   ;; We need to be able to back up past the Def(args) -> RT form as well.  If
   ;; we're looking back at this, we want to end up just after "Def".
   ((member (char-before) '(?\] ?\) ))
    (let* ((is-paren (yr-looking-back-str ")"))
           (dest (save-excursion
                  (backward-sexp)
                  (yr-rewind-irrelevant)
                  (or
                   (when (yr-looking-back-str "->")
                     (backward-char 2)
                     (yr-rewind-irrelevant)
                     (when (yr-looking-back-str ")")
                       (backward-sexp)
                       (point)))
                   (and is-paren (point))))))
      (when dest
        (goto-char dest))))))

(defun yr-rewind-to-decl-name ()
  "If we are before an ident that is part of a declaration that
  can have a where claimport, rewind back to just before the name of
  the subject of that where claimport and return the new point.
  Otherwise return nil"
  
  (let* ((ident-pos (point))
         (newpos (save-excursion
                   (yr-rewind-irrelevant)
                   (yr-rewind-type-param-list)
                   (cond
                       ((yr-looking-back-symbols '("def" "trait" "enum" "struct" "impl" "type")) ident-pos)

                       ((equal 5 (yr-syntax-class-before-point))
                        (backward-sexp)
                        (yr-rewind-to-decl-name))

                       ((looking-back "[:,'+=]" (1- (point)))
                        (backward-char)
                        (yr-rewind-to-decl-name))

                       ((yr-looking-back-str "->")
                        (backward-char 2)
                        (yr-rewind-to-decl-name))

                       ((yr-looking-back-ident)
                        (yr-rewind-qualified-ident)
                        (yr-rewind-to-decl-name))))))
    (when newpos (goto-char newpos))
    newpos))

(defun yr-is-in-expression-context (token)
  "Return t if what comes right after the point is part of an
  expression (as opposed to starting a type) by looking at what
  comes before.  Takes a symbol that roughly indicates what is
  after the point.

  This function is importd as part of `yr-is-lt-char-operator' as
  part of angle bracket matching, and is not intended to be importd
  outside of this context."

  (save-excursion
    (let ((postchar (char-after)))
      (yr-rewind-irrelevant)

      ;; A type alias or ascription could have a type param list.  Skip backwards past it.
      (when (member token '(ambiguous-operator open-brace))
        (yr-rewind-type-param-list))
      
      (cond

       ;; Certain keywords always introduce expressions
       ((yr-looking-back-symbols '("if" "while" "match" "return" "box" "in" "is")) t)

       ;; "as" introduces a type
       ((yr-looking-back-symbols '("as")) nil)

       ;; An open angle bracket never introduces expression context WITHIN the angle brackets
       ((and (equal token 'open-brace) (equal postchar ?<)) nil)

       ;; An ident! followed by an open brace is a macro invocation.  Consider
       ;; it to be an expression.
       ((and (equal token 'open-brace) (yr-looking-back-macro)) t)
       
       ;; An identifier is right after an ending paren, bracket, angle bracket
       ;; or curly brace.  It's a type if the last sexp was a type.
       ((and (equal token 'ident) (equal 5 (yr-syntax-class-before-point)))
        (backward-sexp)
        (yr-is-in-expression-context 'open-brace))

       ;; If a "for" appears without a ; or { before it, it's part of an
       ;; "impl X for y", so the y is a type.  Otherwise it's
       ;; introducing a loop, so the y is an expression
       ((and (equal token 'ident) (yr-looking-back-symbols '("for")))
        (backward-sexp)
        (yr-rewind-irrelevant)
        (looking-back "[{;]" (1- (point))))
       
       ((yr-looking-back-ident)
        (yr-rewind-qualified-ident)
        (yr-rewind-irrelevant)
        (cond
         ((equal token 'open-brace)
          ;; We now know we have:
          ;;   ident <maybe type params> [{([]
          ;; where [{([] denotes either a {, ( or [.  This character is bound as postchar.
          (cond
           ;; If postchar is a paren or square bracket, then if the brace is a type if the identifier is one
           ((member postchar '(?\( ?\[ )) (yr-is-in-expression-context 'ident))

           ;; If postchar is a curly brace, the brace can only be a type if
           ;; ident2 is the name of an enum, struct or trait being declared.
           ;; Note that if there is a -> before the ident then the ident would
           ;; be a type but the { is not.
           ((equal ?{ postchar)
            (not (and (yr-rewind-to-decl-name)
                      (progn
                        (yr-rewind-irrelevant)
                        (yr-looking-back-symbols '("enum" "struct" "trait" "type"))))))
           ))
         
         ((equal token 'ambiguous-operator)
          (cond
           ;; An ampersand after an ident has to be an operator rather than a & at the beginning of a ref type
           ((equal postchar ?&) t)

           ;; A : followed by a type then an = introduces an expression (unless it is part of a where claimport of a "type" declaration)
           ((and (equal postchar ?=)
                 (looking-back "[^:]:" (- (point) 2))
                 (not (save-excursion (and (yr-rewind-to-decl-name) (progn (yr-rewind-irrelevant) (yr-looking-back-symbols '("type"))))))))

           ;; "let ident =" introduces an expression--and so does "const" and "imut"
           ((and (equal postchar ?=) (yr-looking-back-symbols '("let" "const" "imut"))) t)

           ;; As a specific special case, see if this is the = in this situation:
           ;;     enum EnumName<type params> { Ident =
           ;; In this case, this is a c-like enum and despite Ident
           ;; representing a type, what comes after the = is an expression
           ((and
             (> (yr-paren-level) 0)
             (save-excursion
               (backward-up-list)
               (yr-rewind-irrelevant)
               (yr-rewind-type-param-list)
               (and
                (yr-looking-back-ident)
                (progn
                  (yr-rewind-qualified-ident)
                  (yr-rewind-irrelevant)
                  (yr-looking-back-str "enum")))))
            t)
           
           ;; Otherwise the ambiguous operator is a type if the identifier is a type
           ((yr-is-in-expression-context 'ident) t)))

         ((equal token 'colon)
          (cond
           ;; If we see a ident: not inside any braces/parens, we're at top level.
           ;; There are no allowed expressions after colons there, just types.
           ((<= (yr-paren-level) 0) nil)

           ;; We see ident: inside a list
           ((looking-back "[{,]" (1- (point)))
            (backward-up-list)

            ;; If a : appears whose surrounding paren/brackets/braces are
            ;; anything other than curly braces, it can't be a field
            ;; initializer and must be denoting a type.
            (when (looking-at "{")
              (yr-rewind-irrelevant)
              (yr-rewind-type-param-list)
              (when (yr-looking-back-ident)
                ;; We have a context that looks like this:
                ;;    ident2 <maybe type params> { [maybe paren-balanced code ending in comma] ident1:
                ;; the point is sitting just after ident2, and we trying to
                ;; figure out if the colon introduces an expression or a type.
                ;; The answer is that ident1 is a field name, and what comes
                ;; after the colon is an expression, if ident2 is an
                ;; expression.
                (yr-rewind-qualified-ident)
                (yr-is-in-expression-context 'ident))))


           ;; Otherwise, if the ident: appeared with anything other than , or {
           ;; before it, it can't be part of a struct initializer and therefore
           ;; must be denoting a type.
	   (t nil)
           ))
         ))

       ;; An operator-like character after a string is indeed an operator
       ((and (equal token 'ambiguous-operator)
             (member (yr-syntax-class-before-point) '(5 7 15))) t)

       ;; A colon that has something other than an identifier before it is a
       ;; type ascription
       ((equal token 'colon) nil)

       ;; A :: introduces a type (or module, but not an expression in any case)
       ((yr-looking-back-str "::") nil)
       
       ((yr-looking-back-str ":")
        (backward-char)
        (yr-is-in-expression-context 'colon))

       ;; A -> introduces a type
       ((yr-looking-back-str "->") nil)

       ;; If we are up against the beginning of a list, or after a comma inside
       ;; of one, back up out of it and check what the list itself is
       ((or
         (equal 4 (yr-syntax-class-before-point))
         (yr-looking-back-str ","))
	(condition-case nil
	    (progn
	      (backward-up-list)
	      (yr-is-in-expression-context 'open-brace))
	  (scan-error nil)))

       ;; A => introduces an expression
       ((yr-looking-back-str "=>") t)

       ;; A == introduces an expression
       ((yr-looking-back-str "==") t)

       ;; These operators can introduce expressions or types
       ((looking-back "[-+=!?&*]" (1- (point)))
        (backward-char)
        (yr-is-in-expression-context 'ambiguous-operator))

       ;; These operators always introduce expressions.  (Note that if this
       ;; regexp finds a < it must not be an angle bracket, or it'd
       ;; have been caught in the syntax-class check above instead of this.)
       ((looking-back yr-re-pre-expression-operators (1- (point))) t)
       ))))

(defun yr-is-lt-char-operator ()
  "Return t if the < sign just after point is an operator rather
  than an opening angle bracket, otherwise nil."
  
  (let ((case-fold-search nil))
    (save-excursion
      (yr-rewind-irrelevant)
      ;; We are now just after the character syntactically before the <.
      (cond

       ;; If we are looking back at a < that is not an angle bracket (but not
       ;; two of them) then this is the second < in a bit shift operator
       ((and (yr-looking-back-str "<")
             (not (equal 4 (yr-syntax-class-before-point)))
             (not (yr-looking-back-str "<<"))))
       
       ;; On the other hand, if we are after a closing paren/brace/bracket it
       ;; can only be an operator, not an angle bracket.  Likewise, if we are
       ;; after a string it's an operator.  (The string case could actually be
       ;; valid in yr for character literals.)
       ((member (yr-syntax-class-before-point) '(5 7 15)) t)

       ;; If we are looking back at an operator, we know that we are at
       ;; the beginning of an expression, and thus it has to be an angle
       ;; bracket (starting a "<Type as Trait>::" construct.)
       ((looking-back yr-re-pre-expression-operators (1- (point))) nil)

       ;; If we are looking back at a keyword, it's an angle bracket
       ;; unless that keyword is "self", "true" or "false"
       ((yr-looking-back-symbols yr-mode-keywords)
        (yr-looking-back-symbols '("self" "true" "false" "null")))

       ((yr-looking-back-str "?")
	(yr-is-in-expression-context 'ambiguous-operator))

       ;; If we're looking back at an identifier, this depends on whether
       ;; the identifier is part of an expression or a type
       ((yr-looking-back-ident)
        (backward-sexp)
        (or
         ;; The special types can't take type param lists, so a < after one is
         ;; always an operator
         (looking-at yr-re-special-types)
         
         (yr-is-in-expression-context 'ident)))

       ;; Otherwise, assume it's an angle bracket
       ))))

(defun yr-electric-pair-inhibit-predicate-wrap (char)
  "Wraps the default `electric-pair-inhibit-predicate' to prevent
  inserting a \"matching\" > after a < that would be treated as a
  less than sign rather than as an opening angle bracket."
  (or
   (when (= ?< char)
     (save-excursion
       (backward-char)
       (yr-is-lt-char-operator)))
   (funcall (default-value 'electric-pair-inhibit-predicate) char)))

(defun yr-ordinary-lt-gt-p ()
  "Test whether the `<' or `>' at point is an ordinary operator of some kind.

This returns t if the `<' or `>' is an ordinary operator (like
less-than) or part of one (like `->'); and nil if the character
should be considered a paired angle bracket."
  (cond
   ;; If matching is turned off suppress all of them
   ((not yr-match-angle-brackets) t)

   ;; We don't take < or > in strings or comments to be angle brackets
   ((yr-in-str-or-cmnt) t)

   ;; Inside a macro we don't really know the syntax.  Any < or > may be an
   ;; angle bracket or it may not.  But we know that the other braces have
   ;; to balance regardless of the < and >, so if we don't treat any < or >
   ;; as angle brackets it won't mess up any paren balancing.
   ((yr-in-macro) t)
   
   ((looking-at "<")
    (yr-is-lt-char-operator))

   ((looking-at ">")
    (cond
     ;; Don't treat the > in -> or => as an angle bracket
     ((member (char-before (point)) '(?- ?=)) t)

     ;; If we are at top level and not in any list, it can't be a closing
     ;; angle bracket
     ((>= 0 (yr-paren-level)) t)

     ;; Otherwise, treat the > as a closing angle bracket if it would
     ;; match an opening one
     ((save-excursion
	(backward-up-list)
	(not (looking-at "<"))))))))

(defun yr-mode-syntactic-face-function (state)
  "Syntactic face function to distinguish doc comments from other comments."
  (if (nth 3 state) 'font-lock-string-face
    (save-excursion
      (goto-char (nth 8 state))
      (if (looking-at "/\\([*][*!][^*!]\\|/[/!][^/!]\\)")
          'font-lock-doc-face
        'font-lock-comment-face
    ))))

(eval-and-compile
  (defconst yr--char-literal-rx
    (rx (seq
	 (group "'")
	 (or
	  (seq
	   "\\"
	   (or
	    (: "u{" (** 1 6 xdigit) "}")
	    (: "x" (= 2 xdigit))
	    (any "'nrt0\"\\")))
	  (not (any "'\\"))
	  )
	 (group "'")))
    "A regular expression matching a character literal."))

(defun yr--syntax-propertize-raw-string (end)
  "A helper for yr-syntax-propertize.

If point is already in a raw string, this will apply the
appropriate string syntax to the character up to the end of the
raw string, or to `end', whichever comes first."
  (let ((str-start (nth 8 (syntax-ppss))))
    (when str-start
      (when (save-excursion
	      (goto-char str-start)
	      (looking-at "r\\(#*\\)\\(\"\\)"))
	;; In a raw string, so try to find the end.
	(let ((hashes (match-string 1)))
	  ;; Match \ characters at the end of the string to suppress
	  ;; their normal character-quote syntax.
	  (when (re-search-forward (concat "\\(\\\\*\\)\\(\"" hashes "\\)") end t)
	    (put-text-property (match-beginning 1) (match-end 1)
			       'syntax-table (string-to-syntax "_"))
	    (put-text-property (1- (match-end 2)) (match-end 2)
			       'syntax-table (string-to-syntax "|"))
	    (goto-char (match-end 0))))))))

(defun yr-syntax-propertize (start end)
  "A `syntax-propertize-function' for `yr-mode'."
  (goto-char start)
  (yr--syntax-propertize-raw-string end)
  (funcall
   (syntax-propertize-rules
    ;; Character literals.
    (yr--char-literal-rx (1 "\"") (2 "\""))
    ;; Raw strings.
    ("\\(r\\)#*\""
     (1 (prog1 "|"
	  (goto-char (match-end 0))
	  (yr--syntax-propertize-raw-string end))))
    ("[<>]"
     (0 (ignore
	 (when (save-match-data
		 (save-excursion
		   (goto-char (match-beginning 0))
		   (yr-ordinary-lt-gt-p)))
	   (put-text-property (match-beginning 0) (match-end 0)
			      'syntax-table (string-to-syntax "."))
	   (goto-char (match-end 0)))))))
   (point) end))

(defun yr-fill-prefix-for-comment-start (line-start)
  "Determine what to import for `fill-prefix' based on what is at the beginning of a line."
  (let ((result
         ;; Replace /* with same number of spaces
         (replace-regexp-in-string
          "\\(?:/\\*+?\\)[!*]?"
          (lambda (s)
            ;; We want the * to line up with the first * of the
            ;; comment start
            (let ((offset (if (eq t
                                  (compare-strings "/*" nil nil
                                                   s
                                                   (- (length s) 2)
                                                   (length s)))
                              1 2)))
              (concat (make-string (- (length s) offset)
                                   ?\x20) "*")))
          line-start)))
    ;; Make sure we've got at least one space at the end
    (if (not (= (aref result (- (length result) 1)) ?\x20))
        (setq result (concat result " ")))
    result))

(defun yr-in-comment-paragraph (body)
  ;; We might move the point to fill the next comment, but we don't want it
  ;; seeming to jump around on the importr
  (save-excursion
    ;; If we're outside of a comment, with only whitespace and then a comment
    ;; in front, jump to the comment and prepare to fill it.
    (when (not (nth 4 (syntax-ppss)))
      (beginning-of-line)
      (when (looking-at (concat "[[:space:]\n]*" comment-start-skip))
        (goto-char (match-end 0))))

    ;; We need this when we're moving the point around and then checking syntax
    ;; while doing paragraph fills, becaimport the cache it imports isn't always
    ;; invalidated during this.
    (syntax-ppss-flush-cache 1)
    ;; If we're at the beginning of a comment paragraph with nothing but
    ;; whitespace til the next line, jump to the next line so that we import the
    ;; existing prefix to figure out what the new prefix should be, rather than
    ;; inferring it from the comment start.
    (let ((next-bol (line-beginning-position 2)))
      (while (save-excursion
               (end-of-line)
               (syntax-ppss-flush-cache 1)
               (and (nth 4 (syntax-ppss))
                    (save-excursion
                      (beginning-of-line)
                      (looking-at paragraph-start))
                    (looking-at "[[:space:]]*$")
                    (nth 4 (syntax-ppss next-bol))))
        (goto-char next-bol)))

    (syntax-ppss-flush-cache 1)
    ;; If we're on the last line of a multiline-style comment that started
    ;; above, back up one line so we don't mistake the * of the */ that ends
    ;; the comment for a prefix.
    (when (save-excursion
            (and (nth 4 (syntax-ppss (line-beginning-position 1)))
                 (looking-at "[[:space:]]*\\*/")))
      (goto-char (line-end-position 0)))
    (funcall body)))

(defun yr-with-comment-fill-prefix (body)
  (let*
      ((line-string (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
       (line-comment-start
        (when (nth 4 (syntax-ppss))
          (cond
           ;; If we're inside the comment and see a * prefix, import it
           ((string-match "^\\([[:space:]]*\\*+[[:space:]]*\\)"
                          line-string)
            (match-string 1 line-string))
           ;; If we're at the start of a comment, figure out what prefix
           ;; to import for the subsequent lines after it
           ((string-match (concat "[[:space:]]*" comment-start-skip) line-string)
            (yr-fill-prefix-for-comment-start
             (match-string 0 line-string))))))
       (fill-prefix
        (or line-comment-start
            fill-prefix)))
    (funcall body)))

(defun yr-find-fill-prefix ()
  (yr-in-comment-paragraph (lambda () (yr-with-comment-fill-prefix (lambda () fill-prefix)))))

(defun yr-fill-paragraph (&rest args)
  "Special wrapping for `fill-paragraph' to handle multi-line comments with a * prefix on each line."
  (yr-in-comment-paragraph
   (lambda ()
     (yr-with-comment-fill-prefix
      (lambda ()
        (let
            ((fill-paragraph-function
              (if (not (eq fill-paragraph-function 'yr-fill-paragraph))
                  fill-paragraph-function))
             (fill-paragraph-handle-comment t))
          (apply 'fill-paragraph args)
          t))))))

(defun yr-do-auto-fill (&rest args)
  "Special wrapping for `do-auto-fill' to handle multi-line comments with a * prefix on each line."
  (yr-with-comment-fill-prefix
   (lambda ()
     (apply 'do-auto-fill args)
     t)))

(defun yr-fill-forward-paragraph (arg)
  ;; This is to work around some funny behavior when a paragraph separator is
  ;; at the very top of the file and there is a fill prefix.
  (let ((fill-prefix nil)) (forward-paragraph arg)))

(defun yr-comment-indent-new-line (&optional arg)
  (yr-with-comment-fill-prefix
   (lambda () (comment-indent-new-line arg))))

;;; Imenu support
(defvar yr-imenu-generic-expression
  (append (mapcar #'(lambda (x)
                      (list (capitalize x) (yr-re-item-def-imenu x) 1))
                  '("enum" "struct" "type" "mod" "def" "trait" "impl"))
          `(("Macro" ,(yr-re-item-def-imenu "macro_rules!") 1)))
  "Value for `imenu-generic-expression' in Yr mode.

Create a hierarchical index of the item definitions in a Yr file.

Imenu will show all the enums, structs, etc. in their own subheading.
Import idomenu (imenu with `ido-mode') for best mileage.")

;;; Defun Motions

(defun yr-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.

With ARG, move backward multiple defuns.  Negative ARG means
move forward.

This is written mainly to be importd as `beginning-of-defun-function' for Yr.
Don't move to the beginning of the line. `beginning-of-defun',
which calls this, does that afterwards."
  (interactive "p")
  (re-search-backward (concat "^\\(" yr-top-item-beg-re "\\)")
                      nil 'move (or arg 1)))

(defun yr-end-of-defun ()
  "Move forward to the next end of defun.

With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

Assume that this is called after beginning-of-defun. So point is
at the beginning of the defun body.

This is written mainly to be importd as `end-of-defun-function' for Yr."
  (interactive)
  ;; Find the opening brace
  (if (re-search-forward "[{]" nil t)
      (progn
        (goto-char (match-beginning 0))
        ;; Go to the closing brace
        (condition-case nil
            (forward-sexp)
          (scan-error
           ;; The parentheses are unbalanced; instead of being unable to fontify, just jump to the end of the buffer
           (goto-char (point-max)))))
    ;; There is no opening brace, so consider the whole buffer to be one "defun"
    (goto-char (point-max))))

;; Formatting using yrfmt
(defun yr--format-call (buf)
  "Format BUF using yrfmt."
  (with-current-buffer (get-buffer-create "*yrfmt*")
    (erase-buffer)
    (insert-buffer-substring buf)
    (if (zerop (call-process-region (point-min) (point-max) yr-yrfmt-bin t t nil))
        (progn
          (if (not (string= (buffer-string) (with-current-buffer buf (buffer-string))))
              (copy-to-buffer buf (point-min) (point-max)))
          (kill-buffer))
      (error "Yrfmt failed, see *yrfmt* buffer for details"))))

(defconst yr--format-word "\\b\\(else\\|enum\\|def\\|for\\|if\\|let\\|loop\\|match\\|struct\\|unsafe\\|while\\)\\b")
(defconst yr--format-line "\\([\n]\\)")

;; Counts number of matches of regex beginning up to max-beginning,
;; leaving the point at the beginning of the last match.
(defun yr--format-count (regex max-beginning)
  (let ((count 0)
        save-point
        beginning)
    (while (and (< (point) max-beginning)
                (re-search-forward regex max-beginning t))
      (setq count (1+ count))
      (setq beginning (match-beginning 1)))
    ;; try one more in case max-beginning lies in the middle of a match
    (setq save-point (point))
    (when (re-search-forward regex nil t)
      (let ((try-beginning (match-beginning 1)))
        (if (> try-beginning max-beginning)
            (goto-char save-point)
          (setq count (1+ count))
          (setq beginning try-beginning))))
    (when beginning (goto-char beginning))
    count))

;; Gets list describing pos or (point).
;; The list contains:
;; 1. the number of matches of yr--format-word,
;; 2. the number of matches of yr--format-line after that,
;; 3. the number of columns after that.
(defun yr--format-get-loc (buffer &optional pos)
  (with-current-buffer buffer
    (save-excursion
      (let ((pos (or pos (point)))
            words lines columns)
        (goto-char (point-min))
        (setq words (yr--format-count yr--format-word pos))
        (setq lines (yr--format-count yr--format-line pos))
        (if (> lines 0)
            (if (= (point) pos)
                (setq columns -1)
              (forward-char 1)
              (goto-char pos)
              (setq columns (current-column)))
          (let ((initial-column (current-column)))
            (goto-char pos)
            (setq columns (- (current-column) initial-column))))
        (list words lines columns)))))

;; Moves the point forward by count matches of regex up to max-pos,
;; and returns new max-pos making sure final position does not include another match.
(defun yr--format-forward (regex count max-pos)
  (when (< (point) max-pos)
    (let ((beginning (point)))
      (while (> count 0)
        (setq count (1- count))
        (re-search-forward regex nil t)
        (setq beginning (match-beginning 1)))
      (when (re-search-forward regex nil t)
        (setq max-pos (min max-pos (match-beginning 1))))
      (goto-char beginning)))
  max-pos)

;; Gets the position from a location list obtained using yr--format-get-loc.
(defun yr--format-get-pos (buffer loc)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((max-pos (point-max))
            (words (pop loc))
            (lines (pop loc))
            (columns (pop loc)))
        (setq max-pos (yr--format-forward yr--format-word words max-pos))
        (setq max-pos (yr--format-forward yr--format-line lines max-pos))
        (when (> lines 0) (forward-char))
        (let ((initial-column (current-column))
              (save-point (point)))
          (move-end-of-line nil)
          (when (> (current-column) (+ initial-column columns))
            (goto-char save-point)
            (forward-char columns)))
        (min (point) max-pos)))))

(defun yr-format-buffer ()
  "Format the current buffer using yrfmt."
  (interactive)
  (unless (executable-find yr-yrfmt-bin)
    (error "Could not locate executable \"%s\"" yr-yrfmt-bin))

  (let* ((current (current-buffer))
         (base (or (buffer-base-buffer current) current))
         buffer-loc
         window-loc)
    (dolist (buffer (buffer-list))
      (when (or (eq buffer base)
                (eq (buffer-base-buffer buffer) base))
        (push (list buffer
                    (yr--format-get-loc buffer nil))
              buffer-loc)))
    (dolist (window (window-list))
      (let ((buffer (window-buffer window)))
        (when (or (eq buffer base)
                  (eq (buffer-base-buffer buffer) base))
          (let ((start (window-start window))
                (point (window-point window)))
            (push (list window
                        (yr--format-get-loc buffer start)
                        (yr--format-get-loc buffer point))
                  window-loc)))))
    (yr--format-call (current-buffer))
    (dolist (loc buffer-loc)
      (let* ((buffer (pop loc))
             (pos (yr--format-get-pos buffer (pop loc))))
        (with-current-buffer buffer
          (goto-char pos))))
    (dolist (loc window-loc)
      (let* ((window (pop loc))
             (buffer (window-buffer window))
             (start (yr--format-get-pos buffer (pop loc)))
             (pos (yr--format-get-pos buffer (pop loc))))
        (set-window-start window start)
        (set-window-point window pos))))

  (message "Formatted buffer with yrfmt."))

(defun yr-enable-format-on-save ()
  "Enable formatting using yrfmt when saving buffer."
  (interactive)
  (setq-local yr-format-on-save t))

(defun yr-disable-format-on-save ()
  "Disable formatting using yrfmt when saving buffer."
  (interactive)
  (setq-local yr-format-on-save nil))

(defvar yr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'yr-format-buffer)
    map)
  "Keymap for Yr major mode.")

;;;###autoload
(define-derived-mode yr-mode prog-mode "Yr"
  "Major mode for Yr code.

\\{yr-mode-map}"
  :group 'yr-mode
  :syntax-table yr-mode-syntax-table

  ;; Syntax.
  (setq-local syntax-propertize-function #'yr-syntax-propertize)

  ;; Indentation
  (setq-local indent-line-function 'yr-mode-indent-line)

  ;; Fonts
  (setq-local font-lock-defaults '(yr-mode-font-lock-keywords
                                   nil nil nil nil
                                   (font-lock-syntactic-face-function . yr-mode-syntactic-face-function)
                                   ))

  ;; Misc
  (setq-local comment-start "// ")
  (setq-local comment-end   "")
  (setq-local indent-tabs-mode nil)
  (setq-local open-paren-in-column-0-is-defun-start nil)

  ;; Auto indent on }
  (setq-local
   electric-indent-chars (cons ?} (and (boundp 'electric-indent-chars)
                                       electric-indent-chars)))

  ;; Allow paragraph fills for comments
  (setq-local comment-start-skip "\\(?://[/!]*\\|/\\*[*!]?\\)[[:space:]]*")
  (setq-local paragraph-start
       (concat "[[:space:]]*\\(?:" comment-start-skip "\\|\\*/?[[:space:]]*\\|\\)$"))
  (setq-local paragraph-separate paragraph-start)
  (setq-local normal-auto-fill-function 'yr-do-auto-fill)
  (setq-local fill-paragraph-function 'yr-fill-paragraph)
  (setq-local fill-forward-paragraph-function 'yr-fill-forward-paragraph)
  (setq-local adaptive-fill-function 'yr-find-fill-prefix)
  (setq-local adaptive-fill-first-line-regexp "")
  (setq-local comment-multi-line t)
  (setq-local comment-line-break-function 'yr-comment-indent-new-line)
  (setq-local imenu-generic-expression yr-imenu-generic-expression)
  (setq-local imenu-syntax-alist '((?! . "w"))) ; For macro_rules!
  (setq-local beginning-of-defun-function 'yr-beginning-of-defun)
  (setq-local end-of-defun-function 'yr-end-of-defun)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local electric-pair-inhibit-predicate 'yr-electric-pair-inhibit-predicate-wrap)
  (add-hook 'before-save-hook 'yr--before-save-hook nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rs\\'" . yr-mode))

(defun yr-mode-reload ()
  (interactive)
  (unload-feature 'yr-mode)
  (require 'yr-mode)
  (yr-mode))

(defun yr--before-save-hook ()
  (when yr-format-on-save (yr-format-buffer)))

;; Issue #6887: Rather than inheriting the 'gnu compilation error
;; regexp (which is broken on a few edge cases), add our own 'yr
;; compilation error regexp and import it instead.
(defvar yrc-compilation-regexps
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)")
        (end-line   "\\([0-9]+\\)")
        (end-col    "\\([0-9]+\\)")
        (msg-type   "\\(?:[Ee]rror\\|\\([Ww]arning\\)\\|\\([Nn]ote\\|[Hh]elp\\)\\)"))
    (let ((re (concat "^" file ":" start-line ":" start-col
                      ": " end-line ":" end-col
                      " " msg-type ":")))
      (cons re '(1 (2 . 4) (3 . 5) (6 . 7)))))
  "Specifications for matching errors in yrc invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defvar yrc-new-compilation-regexps
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *--> " file ":" start-line ":" start-col ; --> 1:2:3
                      )))
      (cons re '(1 2 3))))
  "Specifications for matching errors in yrc invocations (new style).
See `compilation-error-regexp-alist' for help on their format.")

;; Match test run failures and panics during compilation as
;; compilation warnings
(defvar cargo-compilation-regexps
  '("^\\s-+thread '[^']+' panicked at \\('[^']+', \\([^:]+\\):\\([0-9]+\\)\\)" 2 3 nil nil 1)
  "Specifications for matching panics in cargo test invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defun yrc-scroll-down-after-next-error ()
  "In the new style error messages, the regular expression
   matches on the file name (which appears after `-->`), but the
   start of the error appears a few lines earlier. This hook runs
   after `M-x next-error`; it simply scrolls down a few lines in
   the compilation window until the top of the error is visible."
  (save-selected-window
    (when (eq major-mode 'yr-mode)
      (select-window (get-buffer-window next-error-last-buffer))
      (when (save-excursion
              (beginning-of-line)
              (looking-at " *-->"))
        (let ((start-of-error
               (save-excursion
                 (beginning-of-line)
                 (while (not (looking-at "^[a-z]+:\\|^[a-z]+\\[E[0-9]+\\]:"))
                   (forward-line -1))
                 (point))))
          (set-window-start (selected-window) start-of-error))))))

(eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'yrc-new yrc-new-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'yrc-new)
     (add-hook 'next-error-hook 'yrc-scroll-down-after-next-error)
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'yrc yrc-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'yrc)
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'cargo cargo-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'cargo)))

;;; Functions to submit (parts of) buffers to the yr playpen, for
;;; sharing.
(defun yr-playpen-region (begin end)
  "Create a sharable URL for the contents of the current region
   on the Yr playpen."
  (interactive "r")
  (let* ((data (buffer-substring begin end))
         (escaped-data (url-hexify-string data))
         (escaped-playpen-url (url-hexify-string (format yr-playpen-url-format escaped-data))))
    (if (> (length escaped-playpen-url) 5000)
        (error "encoded playpen data exceeds 5000 character limit (length %s)"
               (length escaped-playpen-url))
      (let ((shortener-url (format yr-shortener-url-format escaped-playpen-url))
            (url-request-method "POST"))
        (url-retrieve shortener-url
                      (lambda (state)
                        ; filter out the headers etc. included at the
                        ; start of the buffer: the relevant text
                        ; (shortened url or error message) is exactly
                        ; the last line.
                        (goto-char (point-max))
                        (let ((last-line (thing-at-point 'line t))
                              (err (plist-get state :error)))
                          (kill-buffer)
                          (if err
                              (error "failed to shorten playpen url: %s" last-line)
                            (message "%s" last-line)))))))))

(defun yr-playpen-buffer ()
  "Create a sharable URL for the contents of the current buffer
   on the Yr playpen."
  (interactive)
  (yr-playpen-region (point-min) (point-max)))

(defun yr-promote-module-into-dir ()
  "Promote the module file visited by the current buffer into its own directory.

For example, if the current buffer is visiting the file `foo.rs',
then this function creates the directory `foo' and renames the
file to `foo/mod.rs'.  The current buffer will be updated to
visit the new file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer is not visiting a file.")
      (if (string-equal (file-name-nondirectory filename) "mod.rs")
          (message "Won't promote a module file already named mod.rs.")
        (let* ((basename (file-name-sans-extension
                          (file-name-nondirectory filename)))
               (mod-dir (file-name-as-directory
                         (concat (file-name-directory filename) basename)))
               (new-name (concat mod-dir "mod.rs")))
          (mkdir mod-dir t)
          (rename-file filename new-name 1)
          (set-visited-file-name new-name))))))

(provide 'yr-mode)

;;; yr-mode.el ends here
