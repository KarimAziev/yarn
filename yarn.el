;;; yarn.el --- Transient interface for Yarn Classic -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/yarn
;; Keywords: tools
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient interface for Yarn

;; Usage

;; (require 'yarn)

;; And then run in your project:
;;   M-x `yarn-menu'

;; If project directory contains .nvmrc file,
;; a variable `yarn-use-nvm' is non-nil and nvm installed,
;; execute nvm-use before running command.

;;
;; Customization

;; `yarn-global-config-directory'
;;    The path to the node and yarn directory with links.

;; `yarn-use-nvm'
;;     Whether to execute nvm-use if .nvmrc exists and nvm installed.

;;; Commands

;; M-x `yarn-outdated'
;;      Command dispatcher for yarn outdated.

;; M-x `yarn-why'
;;      Command dispatcher for yarn why.

;; M-x `yarn-run'
;;      Command dispatcher for yarn run.

;; M-x `yarn-owner'
;;      Command dispatcher for yarn owner.

;; M-x `yarn-versions'
;;      Command dispatcher for yarn versions.

;; M-x `yarn-licences'
;;      Command dispatcher for yarn licences.

;; M-x `yarn-import'
;;      Command dispatcher for yarn import.

;; M-x `yarn-help'
;;      Command dispatcher for yarn generate-lock-entry.

;; M-x `yarn-generate-lock-entry'
;;      Command dispatcher for yarn generate-lock-entry.

;; M-x `yarn-config-delete' (&optional global)
;;      Run yarn config delete.
;;      With argument GLOBAL is non nil, globally.

;; M-x `yarn-config-get' (&optional global)
;;      Run yarn config get.
;;      With argument GLOBAL is non nil, globally.

;; M-x `yarn-config-set' (&optional global)
;;      Run yarn config set.
;;      With argument GLOBAL is non nil, globally.

;; M-x `yarn-jump-to-cache-dir'
;;      Jump to yarn cache dir.

;; M-x `yarn-bin'
;;      Command dispatcher for yarn audit options.

;; M-x `yarn-done'
;;      Search from transient.

;; M-x `yarn-transient-show-args'
;;      Wave at the user.

;; M-x `yarn-add-read-dependency' (&optional initial-input)
;;      Read dependency to install.
;;      INITIAL-INPUT can be given as the initial minibuffer input.

;; M-x `yarn-unlink'
;;      Unlink and reinstall linked package in current project.

;; M-x `yarn-read-new-dependency' (&optional initial-input)
;;      Search for dependencies

;;; Customization

;; `yarn-use-nvm'
;;      Whether to prepend nvm-use to command if .nvmrc exists.

;; `yarn-global-config-directory'
;;      The path to the yarn config.

;;; Functions

;;; Code:

(require 'json)

(require 'transient)
(declare-function vterm "ext:vterm")
(require 'compile)

(defconst yarn-nvm-version-re
  "v[0-9]+\.[0-9]+\.[0-9]+"
  "Regex matching a Node version.")

(defcustom yarn-nvm-dir (expand-file-name "~/.nvm")
  "Full path to Nvm installation directory."
  :group 'nvm
  :type 'directory)

(defun yarn-nvm--version-name (runtime path)
  "Make RUNTIME names match those in nvm PATH ls."
  (if (string= "node" runtime)
      (file-name-nondirectory (directory-file-name path))
    (concat (replace-regexp-in-string
             (regexp-quote "io.js")
             "iojs"
             (file-name-nondirectory
              (directory-file-name runtime)))
            "-"
            (file-name-nondirectory
             (directory-file-name
              path)))))

(defun yarn-nvm--installed-versions-dirs ()
  "Return list of directories with installed versions of node."
  (let* ((files (mapcan
                 (lambda (versions-dir)
                   (directory-files versions-dir t
                                    directory-files-no-dot-files-regexp))
                 (delq nil
                       (append
                        (list (yarn-expand-when-exists
                               yarn-nvm-dir))
                        (list (yarn-expand-when-exists
                               "versions"
                               yarn-nvm-dir)))))))
    (mapcan
     (lambda (it)
       (when-let ((name (when (file-directory-p it)
                          (file-name-nondirectory
                           (directory-file-name
                            it)))))
         (if (string-match-p (concat yarn-nvm-version-re "$")
                             name)
             (list it)
           (directory-files it t yarn-nvm-version-re))))
     files)))

(defun yarn-nvm--installed-versions ()
  "Return list of directories with installed versions of node."
  (mapcar (lambda (it)
            (cons (file-name-nondirectory it) it))
          (yarn-nvm--installed-versions-dirs)))

(defun yarn-nvm--version-from-string (version-string)
  "Split a VERSION-STRING into a list of (major, minor, patch) numbers."
  (mapcar 'string-to-number (split-string version-string "[^0-9]" t)))

(defun yarn-nvm--version-match-p (matcher version)
  "Does this VERSION satisfy the requirements in MATCHER?"
  (or (eq (car matcher) nil)
      (and (eq (car matcher)
               (car version))
           (yarn-nvm--version-match-p (cdr matcher)
                                      (cdr version)))))

(defun yarn-nvm-version-compare (a b)
  "Comparator for sorting NVM versions, return t if A < B."
  (if (eq (car a)
          (car b))
      (yarn-nvm-version-compare (cdr a)
                                (cdr b))
    (< (car a)
       (car b))))

(defun yarn-nvm-find-exact-version-for (short)
  "Find most suitable version for SHORT.

SHORT is a string containing major and optionally minor version.
This function will return the most recent version whose major
and (if supplied, minor) match."
  (when (and short
             (string-match-p "v?[0-9]+\\(\.[0-9]+\\(\.[0-9]+\\)?\\)?$" short))
    (unless (or (string-prefix-p "v" short)
                (string-prefix-p "node" short)
                (string-prefix-p "iojs" short))
      (setq short (concat "v" short)))
    (let* ((versions (yarn-nvm--installed-versions))
           (requested (yarn-nvm--version-from-string short))
           (first-version
            (seq-find (lambda (it)
                        (string= (car it) short))
                      versions)))
      (or
       first-version
       (let ((possible-versions
              (seq-filter
               (lambda (version)
                 (yarn-nvm--version-match-p
                  requested
                  (yarn-nvm--version-from-string (car version))))
               versions)))
         (if (eq possible-versions nil)
             nil
           (car (sort possible-versions
                      (lambda (a b)
                        (not (yarn-nvm-version-compare
                              (yarn-nvm--version-from-string (car a))
                              (yarn-nvm--version-from-string (car b)))))))))))))


(declare-function vterm--invalidate "ext:vterm")
(declare-function vterm-send-string "ext:vterm")

(defgroup yarn nil
  "Run yarn commands."
  :group 'tools)

(defcustom yarn-global-config-directory "~/.config/yarn"
  "The path to the yarn config."
  :type 'directory
  :group 'yarn)

(defcustom yarn-use-nvm nil
  "Whether to prepend `nvm-use' to command if .nvmrc exists."
  :type 'boolean
  :group 'yarn)

(defcustom yarn-common-buffer-name-function 'yarn-common-create-unique-buffer-name
  "Buffer name for `npm' command, or function which return buffer name.
The function takes three arguments, ROOT, NPM-COMMAND, ARGS.
ROOT is project root directory.  NPM-COMMAND is npm command string.
ARGS is list of arguments passed to npm command.

You can use `yarn-common-create-unique-buffer-name' to use unique buffer name
among all sesstions."
  :group 'yarn
  :type '(choice
          (string :tag "Use same buffer through all sessions")
          (const :tag "Use unique buffer name among all sessions"
                 yarn-common-create-unique-buffer-name)
          function))

(defvar yarn-json-hash (make-hash-table :test 'equal))

(defun yarn-read-json (file &optional json-type)
  "Read the JSON object in FILE, return object converted to JSON-TYPE.
JSON-TYPE must be one of `alist', `plist', or `hash-table'."
  (condition-case nil
      (let* ((json-object-type (or json-type 'plist))
             (cache (gethash (format "%s:%s" file json-object-type)
                             yarn-json-hash))
             (cache-tick (and cache (plist-get cache :tick)))
             (tick (file-attribute-modification-time (file-attributes
                                                      file
                                                      'string)))
             (content-json))
        (when (or (null cache)
                  (not (equal tick cache-tick)))
          (setq content-json
                (with-temp-buffer
                  (insert-file-contents file)
                  (json-read-from-string
                   (buffer-substring-no-properties
                    (point-min)
                    (point-max)))))
          (setq cache (list :tick tick
                            :json content-json))
          (puthash file cache yarn-json-hash))
        (plist-get cache :json))
    (error (message "Could't read %s as json."
                    file))))

(defvar yarn-history-dependencies nil)

;;;###autoload
(defun yarn-read-new-dependency (&optional initial-input)
  "Call the npm search shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (require 'ivy nil t)
  (require 'counsel nil t)
  (if (and (fboundp 'ivy-more-chars)
           (fboundp 'counsel--async-command)
           (fboundp 'ivy-read)
           (fboundp 'counsel-delete-process))
      (let ((result)
            (dependencies))
        (setq result (ivy-read
                      "Repo:\s"
                      (lambda (str)
                        (or
                         (ivy-more-chars)
                         (progn
                           (counsel--async-command (concat
                                                    "npm search --parseable "
                                                    str))
                           '("" "working..."))))
                      :initial-input initial-input
                      :dynamic-collection t
                      :history 'yarn-history-dependencies
                      :multi-action
                      (lambda (marked)
                        (setq dependencies
                              (append marked dependencies)))
                      :action (lambda (d) d)
                      :unwind #'counsel-delete-process
                      :caller 'yarn-read-new-dependency))
        (if dependencies
            (string-join (mapcar (lambda (d)
                                   (car (split-string d)))
                                 dependencies)
                         "\s")
          (car (split-string result nil t))))
    (read-string "Dependency: ")))

(defun yarn-get-nvm-node-version ()
  "Lookup and read .nvmrc file."
  (when-let ((nvmrc (locate-dominating-file default-directory ".nvmrc")))
    (with-temp-buffer (insert-file-contents (expand-file-name ".nvmrc" nvmrc))
                      (string-trim (buffer-substring-no-properties (point-min)
                                                                   (point-max))))))


(defun yarn-exec-with-args (command &rest args)
  "Run a shell COMMAND with ARGS."
  (let ((cmdline (mapconcat (lambda (it)
                              (if (string-match-p "\s\t\n" it)
                                  (shell-quote-argument it)
                                it))
                            (append (list command)
                                    (flatten-list args))
                            "\s")))
    (with-temp-buffer
      (shell-command cmdline (current-buffer))
      (let ((output (string-trim (buffer-substring-no-properties (point-min)
                                                                 (point-max)))))
        (unless (string-empty-p output)
          output)))))

(defun yarn-nvm-command (command &rest args)
  "Run nvm COMMAND with ARGS if nvm directory exists."
  (when-let ((nvm-path (yarn-nvm-path)))
    (yarn-exec-with-args "source" nvm-path
                             "&&" "nvm" command args)))

(defun yarn-nvm-installed-node-versions ()
  "Return list of installed with nvm node versions."
  (let ((versions (split-string (or (yarn-nvm-command "ls") "")
                                "[\r\f\n]" t)))
    versions))

(defun yarn-ensure-nvm-node-installed (&optional force)
  "Install node version specified in nvmrc file of PROJECT.
If FORCE is non nil, install it even if it is installed."
  (when-let ((nvm-node-version (yarn-get-nvm-node-version)))
    (let ((regex (regexp-quote nvm-node-version))
          (versions (yarn-nvm-installed-node-versions)))
      (when (and
             (or force
                 (not (seq-find (apply-partially #'string-match-p regex)
                                versions)))
             (yes-or-no-p
              (format
               "This project requires node %s, which is not installed. Install?"
               nvm-node-version)))
        (yarn-nvm-command "install"
                          nvm-node-version
                          "--reinstall-packages-from=current")))))

(defun yarn-ensure-nvm-use (command)
  "If PROJECT can use nvm, prepend to COMMAND nvm use."
  (if-let ((nvm-path (and
                      yarn-use-nvm
                      (yarn-get-nvm-node-version)
                      (yarn-nvm-path))))
      (progn (yarn-ensure-nvm-node-installed)
             (concat "source " nvm-path " && nvm use && " command))
    command))

(defun yarn-expand-when-exists (filename &optional directory)
  "Expand FILENAME to DIRECTORY and return result if exists."
  (when-let ((file (expand-file-name filename directory)))
    (when (file-exists-p file)
      file)))

(defun yarn-nvm-path ()
  "Return path to NVM_DIR if exists."
  (when-let* ((yarn-nvm-dir (or (getenv "NVM_DIR")
                           (when (file-exists-p "~/.nvm/")
                             "~/.nvm/")))
              (file (expand-file-name "nvm.sh" yarn-nvm-dir)))
    (when (file-exists-p file)
      file)))

(defun yarn-run-in-vterm (project command)
  "Run COMMAND in PROJECT in vterm."
  (let ((buffer (format "*%s*" (concat "vterm-yarn-"
                                       (replace-regexp-in-string
                                        "~/"
                                        ""
                                        (abbreviate-file-name project))))))
    (when (buffer-live-p (get-buffer buffer))
      (switch-to-buffer (get-buffer buffer))
      (vterm--invalidate)
      (kill-buffer (get-buffer buffer)))
    (let ((default-directory project))
      (vterm buffer)
      (run-at-time
       0.5 nil 'vterm-send-string command)
      (run-at-time 1 nil 'vterm-send-return))))

(defun yarn-run-async-shell-command (project command)
  "Run COMMAND in PROJECT with `async-shell-command'."
  (let ((buffer (format "*%s*" (concat "yarn-"
                                       (abbreviate-file-name project))))
        (default-directory project))
    (when (buffer-live-p (get-buffer buffer))
      (kill-buffer buffer))
    (async-shell-command command buffer buffer)))

(defun yarn-get-package-versions (package)
  "Exec \"yarn info\" for PACKAGE and return list of available versions."
  (let ((str (string-trim
              (shell-command-to-string (string-join
                                        `("yarn info" ,package)
                                        "\s"))))
        (versions))
    (with-temp-buffer
      (erase-buffer)
      (insert str)
      (goto-char (point-min))
      (re-search-forward "\\_<\\(versions\\)\\_>" nil t 1)
      (skip-chars-forward ":\s\t\f\n")
      (when (looking-at "\\[")
        (let ((beg (point))
              (end))
          (forward-sexp 1)
          (setq end (point))
          (setq versions
                (mapcar
                 (lambda (it) (string-join (split-string it "[,'\"]" t) ""))
                 (split-string
                  (buffer-substring-no-properties
                   (1+ beg)
                   (1- end))
                  nil t))))))
    (reverse versions)))

(defun yarn-list-symlinked-dirs-recursively (directory)
  "Return list of absolute symlinked directories in DIRECTORY."
  (setq directory (file-name-as-directory directory))
  (let ((result nil)
        (tramp-mode (and tramp-mode (file-remote-p
                                     (expand-file-name directory)))))
    (dolist (file (delete ".." (delete "." (directory-files directory))))
      (when-let ((full-file (unless (member file '(".git" ".github"))
                              (concat directory file))))
        (when (file-directory-p full-file)
          (if (file-symlink-p full-file)
              (push full-file result)
            (when-let ((dirs (yarn-list-symlinked-dirs-recursively
                              full-file)))
              (setq result (nconc result dirs)))))))
    result))

(defun yarn-links-in-dir (directory)
  "Return non-absolute symlinked packages in DIRECTORY."
  (let ((len (1+ (length directory))))
    (mapcar (lambda (it)
              (substring it len))
            (yarn-list-symlinked-dirs-recursively directory))))

(defun yarn-project-linked-dependencies ()
  "Return list of linked packages in the current project.
Only those packages includes that listed in package.json."
  (let ((node-modules (yarn-get-node-modules-path))
        (dependencies (mapcar #'car (yarn-get-dependencies-alist)))
        (links))
    (dolist (package-name dependencies)
      (when-let ((dir (yarn-expand-when-exists (format "%s" package-name)
                                                   node-modules)))
        (when (file-symlink-p dir)
          (push (format "%s" package-name) links))))
    links))

(defun yarn-find-global-links ()
  "Return linked packages in `yarn-global-config-directory' directory."
  (when-let ((links-dir
              (when yarn-global-config-directory
                (yarn-expand-when-exists
                 "link"
                 yarn-global-config-directory))))
    (yarn-links-in-dir links-dir)))

(defun yarn-get-node-modules-path ()
  "Look up directory hierarchy for directory containing node_modules.
Return absolute path to node_modules or nil."
  (when-let ((project-root (locate-dominating-file
                            default-directory
                            "node_modules")))
    (expand-file-name "node_modules" project-root)))

(defun yarn-get-project-root ()
  "Look up directory hierarchy for directory containing package.json.
Return full path of containing directory or nil."
  (locate-dominating-file
   default-directory
   "package.json"))

(defun yarn-get-package-json-path ()
  "Look up directory hierarchy for directory containing package.json.
Return absolute path to package.json or nil."
  (when-let ((project-root (yarn-get-project-root)))
    (expand-file-name "package.json"
                      project-root)))

(defun yarn-get-node-modules-paths ()
  "Look up directory hierarchy for directory containing package.json.
Return absolute path to package.json or nil."
  (when-let ((project-root (locate-dominating-file
                            default-directory
                            "node_modules")))
    (expand-file-name "node_modules"
                      project-root)))

(defun yarn-read-from-package-json (key)
  "Return value of KEY from package.json."
  (let* ((package-json-path (yarn-get-package-json-path))
         (package-json (yarn-read-json package-json-path 'alist)))
    (alist-get key package-json)))

(defun yarn-jest-installed-p ()
  "Return t if jest can be run in the current project."
  (when-let* ((node-modules (yarn-get-node-modules-path))
              (package-json-path (yarn-get-package-json-path))
              (package-json (yarn-read-json package-json-path 'alist))
              (scripts (alist-get 'scripts package-json)))
    (and (alist-get 'test scripts)
         (not (null (member "jest" (directory-files node-modules)))))))

(defun yarn-jest-current-file-cmd ()
  "Return string with jest command for testing current file."
  (when-let ((file (or buffer-file-name default-directory))
             (project-root (yarn-get-project-root)))
    (let ((path-pattern (shell-quote-argument
                         (replace-regexp-in-string
                          (regexp-quote project-root)
                          ""
                          (abbreviate-file-name file)))))
      (yarn-add-props
       (string-join (list
                     "test"
                     "--testPathPattern"
                     path-pattern)
                    "\s")
       :description "Run jest on current file"))))

(defun yarn-get-current-scripts ()
  "Get current project scripts from package.json.
Return list of strings, propertized with :description."
  (when-let* ((package-json-path (yarn-get-package-json-path))
              (package-json (yarn-read-json package-json-path 'alist))
              (scripts (alist-get 'scripts package-json)))
    (mapcar (lambda (it)
              (yarn-add-props
               (format "%s" (car it))
               :description (format "%s:\s%s" (car it)
                                    (cdr it))))
            scripts)))

(defun yarn-read-package-json-script ()
  "Get current project scripts from package.json.
Return list of strings, propertized with :description."
  (when-let* ((package-json-path (yarn-get-package-json-path))
              (package-json (yarn-read-json package-json-path 'alist))
              (scripts (alist-get 'scripts package-json)))
    (let ((annotf
           (lambda (it)
             (concat ": "
                     (cdr
                      (assoc
                       (intern
                        it)
                       scripts))))))
      (completing-read "Script: "
                       (lambda (str pred
                               action)
                         (if
                             (eq action
                                 'metadata)
                             `(metadata
                               (annotation-function
                                . ,annotf))
                           (complete-with-action
                            action scripts
                            str
                            pred)))))))

(defun yarn-stringify (x)
  "Convert X to string effeciently.
X can be any object."
  (cond
   ((stringp x)
    x)
   ((symbolp x)
    (symbol-name x))
   ((integerp x)
    (number-to-string x))
   ((floatp x)
    (number-to-string x))
   (t (format "%s" x))))

(defun yarn-add-props (string &rest properties)
  "Propertize STRING with PROPERTIES."
  (setq string (yarn-stringify string))
  (let* ((result (list 'head))
         (last result))
    (while properties
      (let* ((key (pop properties))
             (val (pop properties))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (apply #'propertize string (cdr result))))

(defun yarn-confirm-package-version (package &optional prompt)
  "Confirm PACKAGE version with PROMPT."
  (let ((version))
    (unless (string-match-p "[a-z-]+@" package)
      (when-let ((versions (seq-reduce
                            (lambda (acc it)
                              (let ((prefixes '("^" ">="))
                                    (l `(,it)))
                                (dolist (prefix prefixes)
                                  (push (format "%s%s" prefix it) l))
                                (setq acc (append acc l))))
                            (yarn-get-package-versions package)
                            '())))
        (setq version
              (completing-read
               (or prompt (format "%s " package))
               (push "latest" versions)))))
    (if version
        (format "%s@%s" package version)
      package)))


(defun yarn-get-dependencies-alist ()
  "Return alist of all dependencies from package.json."
  (let* ((package-json-file  (yarn-get-package-json-path))
         (package-json (yarn-read-json package-json-file
                                           'alist))
         (dependencies (seq-copy
                        (alist-get 'dependencies package-json)))
         (devDependencies (seq-copy (alist-get 'devDependencies
                                               package-json)))
         (peerDependencies (seq-copy (alist-get
                                      'peerDependencies
                                      package-json)))
         (optionalDependencies (seq-copy (alist-get
                                          'optionalDependencies
                                          package-json))))
    (append dependencies devDependencies
            peerDependencies
            optionalDependencies)))

(defun yarn-installed-completion-table ()
  "Completion table with current package.json dependencies."
  (lambda
    (str pred action)
    (let* ((package-json-file  (yarn-get-package-json-path))
           (package-json (yarn-read-json package-json-file
                                             'alist))
           (dependencies (seq-copy
                          (alist-get 'dependencies package-json)))
           (devDependencies (seq-copy (alist-get 'devDependencies
                                                 package-json)))
           (peerDependencies (seq-copy (alist-get
                                        'peerDependencies
                                        package-json)))
           (optionalDependencies (seq-copy (alist-get
                                            'optionalDependencies
                                            package-json)))
           (alist (append dependencies devDependencies
                          peerDependencies
                          optionalDependencies))
           (annotf (lambda (str)
                     (let* ((symb (intern str))
                            (vers (concat "@" (or (cdr
                                                   (assoc symb
                                                          alist))
                                                  "")))
                            (suffix (or (cond ((assoc symb
                                                      devDependencies)
                                               "dev")
                                              ((assoc symb
                                                      peerDependencies)
                                               "peer")
                                              ((assoc symb
                                                      optionalDependencies)
                                               "optional"))
                                        "")))
                       (concat vers " " suffix))))
           (cycle-sort-fn (lambda (it) it))
           (display-sort-fn (lambda (it)
                              (seq-sort-by #'length '< it))))
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,annotf)
            (cycle-sort-function . ,cycle-sort-fn)
            (display-sort-function . ,display-sort-fn))
        (complete-with-action action alist str pred)))))

(defun yarn-common-create-unique-buffer-name (root npm-command)
  "Create buffer name unique to ROOT and NPM-COMMAND."
  (concat "*" npm-command " in " root "*"))

(defun yarn-common--generate-buffer-name-function (root npm-command)
  "Generate function which return buffer name to pass `compilation-start'.
ROOT is project root directory.  NPM-COMMAND is npm command string.
ARGS is list of arguments passed to npm command.

This function uses `yarn-common-buffer-name-function'."
  (if (stringp yarn-common-buffer-name-function)
      yarn-common-buffer-name-function
    (funcall yarn-common-buffer-name-function
             root npm-command)))

(defun yarn-compile-get-new-env (version)
  "Return alist of new environment for node VERSION."
  (when-let* ((version-path (cdr (yarn-nvm-find-exact-version-for version))))
    (let* ((env-flags
            (mapcar
             (lambda (it)
               (cons
                (car it)
                (concat version-path "/" (cdr it))))
             '(("NVM_BIN" . "bin")
               ("NVM_PATH" . "lib/node")
               ("NVM_INC" . "include/node"))))
           (path-re (concat "^"
                            (concat (or (getenv "NVM_DIR")
                                        (expand-file-name "~/.nvm"))
                                    "/\\(?:versions/node/\\|versions/io.js/\\)?")
                            "v[0-9]+\.[0-9]+\.[0-9]+" "/bin/?$"))
           (new-bin-path (expand-file-name  "bin/" version-path))
           (paths
            (cons
             new-bin-path
             (seq-remove
              (lambda (path)
                (if path (string-match-p path-re path) t))
              (parse-colon-path (getenv "PATH")))))
           (new-path (cons "PATH" (string-join paths path-separator)))
           (flags (append env-flags (list new-path)))
           (regexp (mapconcat 'identity (mapcar 'car flags) "\\|")))
      (append (mapcar (lambda (it)
                        (concat (car it) "=" (cdr it)))
                      flags)
              (seq-remove (apply-partially 'string-match-p regexp)
                          process-environment)))))

(defun yarn-compile (npm-command &rest args)
  "Generic compile command for NPM-COMMAND with ARGS functionality."
  (when args
    (setq npm-command (concat npm-command
                              " "
                              (mapconcat (lambda (i)
                                           (unless (stringp i)
                                             (setq i (format "%s" i)))
                                           (string-trim i))
                                         (delq nil (flatten-list args))))))
  (let ((compenv (let* ((nvm-version (yarn-get-nvm-node-version))
                        (env (when nvm-version (yarn-compile-get-new-env
                                                nvm-version))))
                   (or env
                       (when (and (not env) nvm-version)
                         (prog1 process-environment
                           (minibuffer-message
                            "Warning: Mismatched node version")))))))
    (let* ((command npm-command)
           (compilation-read-command nil)
           (compilation-environment compenv)
           (compile-command command)
           (compilation-buffer-name-function
            (lambda (_mode)
              (format "*yarn:%s - %s*"
                      (yarn-read-from-package-json 'name)
                      npm-command))))
      (compilation-start (concat "node -v && " command) t))))

;;;###autoload
(defun yarn-unlink ()
  "Unlink and reinstall linked package in current project."
  (interactive)
  (let ((current-project-name (yarn-read-from-package-json 'name))
        (global-links (yarn-find-global-links))
        (linked-packages (yarn-project-linked-dependencies))
        (choice))
    (setq choice (completing-read "Unlink:\s"
                                  (if (member current-project-name global-links)
                                      (nconc (list "self") linked-packages)
                                    linked-packages)))
    (yarn-compile "yarn unlink"
                  (pcase choice
                    ("self" "")
                    ((pred stringp)
                     (concat choice " && yarn install --force"))
                    (_ " && yarn install --force")))))

;;;###autoload
(defun yarn-add-read-dependency (&optional initial-input)
  "Read dependency to install.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (let ((result (if initial-input
                    (read-string "yarn add\s" (or initial-input ""))
                  (yarn-read-new-dependency)))
        (parts))
    (setq result
          (string-join (seq-drop-while
                        (lambda (it) (member it '("yarn" "add")))
                        (split-string result "[\s\f\t\n\r\v\"]+" t))
                       "\s"))
    (setq result (split-string result nil t))
    (if (yes-or-no-p "Check versions?")
        (dolist (it result)
          (unless (string-match-p "[a-z-]+@" it)
            (when-let* ((versions (yarn-get-package-versions it))
                        (version (completing-read
                                  (format "%s@^" it) versions)))
              (setq it (if (and version
                                (string-match-p "[0-9]" version))
                           (format "%s@^%s" it version)
                         it)))
            (push it parts)))
      (setq parts result))
    (string-join (reverse parts)  "\s")))


;;;###autoload (autoload 'yarn-dispatch-all-options "yarn" nil t)
(transient-define-prefix yarn-dispatch-all-options ()
  "Options dispatcher."
  :incompatible '(("--enable-pnp" "--disable-pnp")
                  ("--verbose" "--silent"))
  [:description
   (lambda ()
     (concat "Yarn " (shell-command-to-string "yarn -v") "Options"))
   [("-fl" "Flat" "--flat")
    ("-F" "Force" "--force")
    ("-P" "Prod" "--prod="
     :class transient-option
     :choices ("true" "false"))
    ("-ld" "link duplicates" "--link-duplicates")
    ("-pp" "prepend node-path" "--scripts-prepend-node-path "
     :class transient-option
     :choices ("true" "false"))
    ("-fs" "Focus on a workspace" "--focus")]
   ["Lockfile"
    ("-pl" "Pure" "--pure-lockfile")
    ("-fr" "Frozen" "--frozen-lockfile")
    ("-nl" "No" "--no-lockfile")
    ("-nv" "--no-node-version-check" "--no-node-version-check")]]
  ["Configure"
   [("-ch" "Cache"
     "--cache-folder "
     :class transient-option
     :reader transient-read-directory
     :prompt "Path: ")
    ("-cw" "Cwd" "--cwd "
     :class transient-option
     :reader transient-read-directory
     :prompt "Working directory: ")
    ("-mf" "Modules" "--modules-folder "
     :class transient-option
     :reader transient-read-directory
     :prompt "Directory: ")]
   [("-lf" "Links" "--link-folder"
     :class transient-option
     :reader transient-read-directory)
    ("-cf" "Cache" "--preferred-cache-folder "
     :class transient-option
     :reader transient-read-directory)
    ("-gf" "Global packages" "--global-folder "
     :class transient-option
     :reader transient-read-directory
     :prompt "Working directory: ")]]
  [["Network"
    ("-r" "Registry" "--registry "
     :class transient-option
     :prompt "Url: ")
    ("-hr" "Save HAR" "--har")
    ("-nc" "concurrency" "--network-concurrency "
     :class transient-option
     :reader transient-read-number-N0)
    ("-of" "Offline" "--offline")
    ("-po" "Prefer-offline"
     "--prefer-offline")
    ("-nt" "TCP timeout" "--network-timeout "
     :class transient-option
     :prompt "Milliseconds: "
     :reader transient-read-number-N0)]
   ["Proxy"
    ("-hp" "Https proxy" "--https-proxy "
     :class transient-option
     :prompt "Host: ")
    ("-pr" "Proxy" "--proxy "
     :class transient-option
     :prompt "Host: ")]]
  ["Pnp"
   ("-dp"  "disable" "--disable-pnp")
   ("-ep" "enable" "--enable-pnp")]
  [["Other"
    ("-ni" "non-interactive" "--non-interactive")
    ("-np" "no progress" "--no-progress")
    ("-yc" "A yarnrc file" "--use-yarnrc "
     :class transient-option
     :prompt "File (.yarnrc only, not .npmrc): "
     :reader transient-read-file)
    ("-ct" "Verify file tree" "--check-files")
    ("-St" "strict-semver" "--strict-semver")
    ("-ic" "skip-integrity-check" "--skip-integrity-check")
    ("-js" "json format" "--json")
    ("-ot" "one-time password" "--otp " :class
     transient-option)
    ("-uc" "update checksums" "--update-checksums")
    ("-em" "enable emoji" "--emoji "
     :class transient-option
     :choices ("true" "false"))]
   ["Toggle"
    ("-ie" "ignore engines check" "--ignore-engines")
    ("-io" "ignore optional dependencies" "--ignore-optional")
    ("-ip" "ignore platform" "--ignore-platform")
    ("-is" "ignore lifecycle scripts" "--ignore-scripts")
    ("-ss" "silent" "--silent")
    ("-nb" "no bin links" "--no-bin-links")
    ("-yy" "no default rc" "--no-default-rc")]]
  ["Misc"
   ("-V" "verbose" "--verbose")])

;;;###autoload
(defun yarn-transient-show-args ()
  "Wave at the user."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (msg (format "transient-current-command %s arguments %s"
                      transient-current-command args)))
    (print args)
    (message (propertize msg 'face 'success))
    args))

(defun yarn-run-command (command &optional project)
  "Run COMMAND in PROJECT using vterm or asynchronously."
  (let ((default-directory (or project
                               (when-let ((package-json-file
                                           (yarn-get-package-json-path)))
                                 (file-name-directory package-json-file))
                               (vc-root-dir)
                               default-directory)))
    (yarn-compile command)))

;;;###autoload
(defun yarn-done ()
  "Execute yarn transient command."
  (interactive)
  (let* ((args
          (when transient-current-command
            (transient-args
             transient-current-command)))
         (cmd
          (pcase transient-current-command
            ('yarn-install "yarn install")
            ('yarn-audit "yarn audit")
            ('yarn-info (concat "yarn info "
                                (yarn-read-new-dependency)))
            ('yarn-add (concat "yarn add "
                               (yarn-add-read-dependency)))
            ('yarn-upgrade
             (concat "yarn upgrade "
                     (let* ((package (completing-read
                                      "Upgrade"
                                      (yarn-installed-completion-table))))
                       (yarn-confirm-package-version
                        package
                        (format "%s@%s" package
                                (alist-get (intern package)
                                           (yarn-get-dependencies-alist)))))))
            ('yarn-remove
             (string-join
              (append (list "yarn remove")
                      (list (completing-read "Remove: "
                                             (yarn-installed-completion-table))))
              "\s"))
            ('yarn-link
             (concat "yarn link "
                     (completing-read "Link: "
                                      (yarn-find-global-links))))
            ('yarn-upgrade-interactive "yarn upgrade-interactive")
            (_
             (when transient-current-command
               (concat "yarn "
                       (replace-regexp-in-string "yarn-" ""
                                                 (symbol-name
                                                  transient-current-command)))))))
         (formatted-args
          (mapconcat (lambda (it)
                       (let ((opt (string-trim it)))
                         (if (string-match "\s\t" opt)
                             (shell-quote-argument opt)
                           opt)))
                     (flatten-list args)
                     "\s")))
    (yarn-run-command (read-string "Run: "
                                   (string-join
                                    (list (string-trim cmd) formatted-args)
                                    " ")))))

(transient-define-argument yarn-level-options ()
  :class 'transient-switches
  :always-read t
  :argument-format "--level %s"
  :argument-regexp "info\\|low\\|moderate\\|high\\|critical"
  :choices '("info" "low" "moderate" "high" "critical"))

;;;###autoload (autoload 'yarn-audit "yarn-yarn.el" nil t)
(transient-define-prefix yarn-audit ()
  "Command dispatcher for yarn audit options."
  ["Options"
   [("l" "Level" yarn-level-options)
    ("g" "Groups " "--groups "
     :class transient-option
     :choices ("dependencies" "devDependencies" "peerDependencies")
     :multi-value rest)
    ("-j" "json format" "--json")
    ("-v" "verbose" "--verbose")]]
  [("RET" "Run" yarn-done)])

;;;###autoload (autoload 'yarn-add "yarn" nil t)
(transient-define-prefix yarn-add ()
  "Command dispatcher for yarn add options."
  ["Options"
   [("-d" "devDependencies " "--dev")
    ("-p" "peerDependencies" "--peer")
    ("-o" "optional" "--optional")
    ("-e" "exact" "--exact")
    ("-t" "tilde" "--tilde")]
   [("-v" "verbose" "--verbose")
    ("-a" "audit" "--audit")
    ("-w" "ignore workspace root" "--ignore-workspace-root-check")]]
  [("RET" "Run" yarn-done)])

;;;###autoload
(defun yarn-bin ()
  "Command dispatcher for yarn audit options."
  (interactive)
  (yarn-run-command (read-string "Run: "
                                 "yarn bin")))

;;;###autoload
(defun yarn-jump-to-cache-dir ()
  "Jump to yarn cache dir."
  (interactive)
  (let ((dir (car (last (split-string
                         (string-trim (shell-command-to-string
                                       "yarn cache dir"))
                         "\n"
                         t)))))
    (when (file-exists-p dir)
      (find-file dir))))

;;;###autoload (autoload 'yarn-install "yarn-yarn.el" nil t)
(transient-define-prefix yarn-install ()
  "Command dispatcher for yarn install options."
  :incompatible '(("--verbose" "--silent"))
  ["Options"
   [("-p" "Prod" "--prod")
    ("-f" "Force" "--force")
    ("-F"
     "Flat"
     "--flat")
    ("-O" "Focus on a workspace" "--focus")
    ("-m" "Modules" "--modules-folder "
     :class transient-option
     :reader transient-read-directory
     :prompt "Directory: ")]
   [("-nb" "no bin links" "--no-bin-links")
    ("-ni" "non-interactive" "--non-interactive")
    ("-ld" "link duplicates" "--link-duplicates")]]
  [["Ignore options"
    ("-io" "ignore optional dependencies" "--ignore-optional")
    ("-ip" "ignore platform" "--ignore-platform")
    ("-is" "ignore lifecycle scripts" "--ignore-scripts")
    ("-ie" "ignore engines check" "--ignore-engines")]
   ["Network"
    ("-hr" "Save HAR" "--har")
    ("-o" "Offline" "--offline")]]
  [["Lockfile"
    ("-lp" "Don’t generate a yarn.lock lockfile" "--pure-lockfile")
    ("-ln" "Don’t read or generate a yarn.lock lockfile." "--no-lockfile")
    ("-lf"
     "Don’t generate a yarn.lock lockfile and fail if an update is needed."
     "--frozen-lockfile")
    ("-lc" "update checksums" "--update-checksums")]
   ["Check"
    ("-a" "audit" "--audit")
    ("-v" "Verify file tree" "--check-files")
    ("-s" "silent" "--silent")
    ("-V" "verbose" "--verbose")]
   [("RET" "show args" yarn-done)]])

;;;###autoload (autoload 'yarn-cache "yarn-yarn.el" nil t)
(transient-define-prefix yarn-cache ()
  "Command dispatcher for yarn cache."
  :incompatible '(("list" "list --pattern "))
  ["Options"
   [("-l" "list all" "list")
    ("-p" "pattern" "list --pattern " :class transient-option)]]
  [("d"
    yarn-jump-to-cache-dir
    :description
    (lambda ()
      (shell-command-to-string
       "yarn cache dir")))
   ("RET" "Run" yarn-done)])

;;;###autoload
(defun yarn-config-set (&optional global)
  "Run yarn config set.
With argument GLOBAL is non nil, globally."
  (interactive (list (yes-or-no-p "Globally?")))
  (yarn-run-command (read-string "Run: "
                                     (yarn-ensure-nvm-use
                                      (concat "yarn config set "
                                              (yarn-config-read global))))))

;;;###autoload
(defun yarn-config-get (&optional global)
  "Run yarn config get.
With argument GLOBAL is non nil, globally."
  (interactive (list (yes-or-no-p "Globally?")))
  (let ((cmd (yarn-ensure-nvm-use
              (concat "yarn config get "
                      (yarn-config-read global)))))
    (message (shell-command-to-string cmd))))

;;;###autoload
(defun yarn-config-delete (&optional global)
  "Run yarn config delete.
With argument GLOBAL is non nil, globally."
  (interactive (list (yes-or-no-p "Globally?")))
  (yarn-run-command (read-string "Run: "
                                     (yarn-ensure-nvm-use
                                      (concat "yarn config delete "
                                              (yarn-config-read global))))))

(defun yarn-config-read (&optional globally)
  "Read yarn config property GLOBALLY."
  (let* ((jsons (ignore-errors nil
                               (mapcar #'json-read-from-string
                                       (split-string
                                        (string-trim
                                         (shell-command-to-string
                                          (if globally
                                              "yarn config list --json"
                                            "yarn config list --global --json")))
                                        "\n"
                                        t))))
         (alist
          (mapcar (lambda (it)
                    (cons (format "%s" (car it))
                          (cdr it)))
                  (seq-find #'listp
                            (mapcar
                             (apply-partially
                              #'alist-get
                              'data)
                             jsons))))
         (annotf (lambda (str)
                   (format " (%s)" (cdr (assoc str alist))))))
    (if alist
        (completing-read "Candidates: "
                         (lambda (str pred action)
                           (if (eq action 'metadata)
                               `(metadata
                                 (annotation-function . ,annotf))
                             (complete-with-action action alist str pred))))
      (read-string "Yarn config property: "))))

;;;###autoload (autoload 'yarn-config "yarn-yarn.el" nil t)
(transient-define-prefix yarn-config ()
  "Command dispatcher for yarn config."
  :incompatible '(("list" "list --global"))
  ["yarn config"
   [("l" "list" "list")
    ("L" "list global" "list --global")
    ("g" "get" yarn-config-get)
    ("d" "delete" yarn-config-delete)
    ("s" "set" yarn-config-set)]]
  [("RET" "Run" yarn-done)])

(transient-define-argument yarn-global-argument ()
  :class 'transient-switches
  :always-read t
  :init-value (lambda (ob)
                (when (not (slot-value ob 'value))
                  (setf (slot-value ob 'value)
                        "list")))
  :argument-format "%s"
  :argument-regexp "add\\|bin\\|list\\|remove\\|upgrade"
  :choices '("add" "bin" "list" "remove" "upgrade"))

;;;###autoload (autoload 'yarn-global "yarn-yarn.el" nil t)
(transient-define-prefix yarn-global ()
  "Command dispatcher for yarn global."
  ["Options"
   [("g" "global" yarn-global-argument)
    ("-p" "prefix" "--prefix "
     :class transient-option
     :reader transient-read-directory
     :prompt "Path: ")]]
  [("RET" "Run" yarn-done)])

;;;###autoload
(defun yarn-generate-lock-entry ()
  "Command dispatcher for yarn generate-lock-entry."
  (interactive)
  (yarn-run-command (read-string "Run: "
                                 "yarn generate-lock-entry")))

;;;###autoload
(defun yarn-help ()
  "Command dispatcher for yarn generate-lock-entry."
  (interactive)
  (yarn-run-command (read-string "Run: "
                                     "yarn help")))

;;;###autoload
(defun yarn-import ()
  "Command dispatcher for yarn import."
  (interactive)
  (yarn-run-command (read-string "Run: "
                                 "yarn import")))

;;;###autoload
(defun yarn-licences ()
  "Command dispatcher for yarn licences."
  (interactive)
  (yarn-run-command
   (yarn-ensure-nvm-use
    (concat
     "yarn licences "
     (completing-read
      "yarn licences "
      '("list"
        "generate-disclaimer"))))))

;;;###autoload
(defun yarn-versions ()
  "Command dispatcher for yarn versions."
  (interactive)
  (yarn-run-command (read-string "Run: "
                                 "yarn versions")))

;;;###autoload (autoload 'yarn-list "yarn-yarn.el" nil t)
(transient-define-prefix yarn-list ()
  "Command dispatcher for yarn list."
  ["Options"
   [("p" "pattern" "--pattern " :class transient-option)
    ("d" "depth" "--depth="
     :class transient-option
     :reader transient-read-number-N0)]]
  [("RET" "Run" yarn-done)])

;;;###autoload (autoload 'yarn-init "yarn-yarn.el" nil t)
(transient-define-prefix yarn-init ()
  "Command dispatcher for yarn init."
  ["Options"
   [("p" "private" "--private")
    ("y" "yes" "--yes")]]
  [("RET" "Run" yarn-done)])
;;;###autoload (autoload 'yarn-info "yarn-yarn.el" nil t)
(transient-define-prefix yarn-info ()
  "Command dispatcher for yarn info."
  ["Options"
   [("j" "json" "--json")]]
  [("RET" "Run" yarn-done)])
;;;###autoload (autoload 'yarn-upgrade "yarn-yarn.el" nil t)
(transient-define-prefix yarn-upgrade ()
  "Command dispatcher for yarn upgrade options."
  ["Options"
   [("-L" "latest" "--latest")
    ("-c" "caret" "--caret")
    ("-e" "exact" "--exact")
    ("-t" "tilde" "--tilde")
    ("-i" "ignore engines check" "--ignore-engines")
    ("-p" "pattern" "--pattern " :class transient-option)]]
  [("RET" "Run" yarn-done)])

;;;###autoload (autoload 'yarn-remove "yarn-yarn.el" nil t)
(transient-define-prefix yarn-remove ()
  "Command dispatcher for yarn remove options."
  ["Options"
   [("-d" "devDependencies " "--dev")
    ("-p" "peerDependencies" "--peer")
    ("-o" "optional" "--optional")]]
  [("RET" "Run" yarn-done)])
;;;###autoload (autoload 'yarn-link "yarn-yarn.el" nil t)
(transient-define-prefix yarn-link ()
  "Command dispatcher for yarn add options."
  ["Options"
   [("-l" "Link folder" "--link-folder "
     :class transient-option
     :reader transient-read-directory
     :prompt "Directory: ")]]
  [("RET" "Run" yarn-done)])
;;;###autoload (autoload 'yarn-upgrade-interactive "yarn-yarn.el" nil t)
(transient-define-prefix yarn-upgrade-interactive ()
  "Command dispatcher for yarn add options."
  ["Options"
   [("-l" "Latest" "--latest")]]
  [("RET" "Run" yarn-done)])

;;;###autoload
(defun yarn-owner ()
  "Command dispatcher for yarn owner."
  (interactive)
  (let* ((subcommand (completing-read "yarn owner " '("list" "add" "remove")))
         (package (unless (string= subcommand "list")
                    (read-string "Package: "))))
    (yarn-run-command (read-string "Run: "
                                       (yarn-ensure-nvm-use
                                        (string-join (delq nil (list
                                                                "yarn owner"
                                                                subcommand
                                                                package))
                                                     " "))))))
;;;###autoload
(defun yarn-run ()
  "Command dispatcher for yarn run."
  (interactive)
  (let ((script (yarn-read-package-json-script)))
    (yarn-run-command (read-string "Run: "
                                   (concat "yarn run " script)))))

(transient-define-argument yarn-version-type-options ()
  :class 'transient-switches
  :argument-format "%s"
  :argument-regexp
  "--prerelease\\|--prepatch\\|--preminor\\|--premajor\\|--patch\\|--minor\\|--major"
  :choices '("--prerelease"
             "--prepatch"
             "--preminor"
             "--premajor"
             "--patch"
             "--minor"
             "--major"))

;;;###autoload
(defun yarn-why ()
  "Command dispatcher for yarn why."
  (interactive)
  (let
      ((pacakge
        (completing-read ""
                         (when-let ((node-modules
                                     (yarn-get-node-modules-paths)))
                           (directory-files node-modules nil
                                            directory-files-no-dot-files-regexp)))))
    (yarn-run-command (read-string "Run: "
                                       (yarn-ensure-nvm-use
                                        (concat "yarn why " pacakge))))))

;;;###autoload (autoload 'yarn-version "yarn-yarn.el" nil t)
(transient-define-prefix yarn-version ()
  "Command dispatcher for yarn version options."
  ["Options"
   [("-c" "--no-commit-hooks" "--no-commit-hooks")
    ("t" "type" yarn-version-type-options)
    ("n" "new-version" "--new-version " :class transient-option)
    ("-g" "disable creating a git tag" "--no-git-tag-version")]]
  [("RET" "Run" yarn-done)])

;;;###autoload (autoload 'yarn-pack "yarn-yarn.el" nil t)
(transient-define-prefix yarn-pack ()
  "Command dispatcher for yarn pack."
  [("f" "output name " "--filename "
    :class transient-option
    :reader transient-read-file)]
  [("RET" "Run" yarn-done)])

;;;###autoload
(defun yarn-outdated ()
  "Command dispatcher for yarn outdated."
  (interactive)
  (yarn-run-command (read-string "Run: "
                                     (yarn-ensure-nvm-use
                                      "yarn outdated"))))

(transient-define-argument yarn-access-type-options ()
  :class 'transient-switches
  :argument-format "--access %s"
  :argument-regexp "public\\|restricted"
  :choices '("public" "restricted"))

(transient-define-prefix yarn-publish ()
  "Open yarn publish transient menu pop up."
  ["Arguments"
   ("-v"
    "Skips the prompt for new version by using the value of version instead"
    "--new-version " :class transient-option)
   ("a" "set package access for other users on the registry"
    yarn-access-type-options)
   ("-o" "one-time password" "--otp " :class transient-option)
   ("-t" "set tag" "--tag " :class transient-option)]
  [["Command"
    ("p" "Publish"       yarn-done)]])

;;;###autoload
(defun yarn-login ()
  "Run yarn login."
  (interactive)
  (yarn-run-command (read-string "Run: "
                                 (yarn-ensure-nvm-use
                                  "yarn login"))))

;;;###autoload
(defun yarn-logout ()
  "Run yarn logout."
  (interactive)
  (yarn-run-command (read-string "Run: "
                                 (yarn-ensure-nvm-use
                                  "yarn logout"))))

;;;###autoload (autoload 'yarn-menu "yarn" nil t)
(transient-define-prefix yarn-menu ()
  "Command dispatcher for yarn commands."
  [:description
   (lambda ()
     (concat "Yarn " (shell-command-to-string "yarn -v")))
   [("a" "add" yarn-add)
    ("A" "audit" yarn-audit)
    ("b" "bin" yarn-bin)
    ("c" "cache" yarn-cache)
    ("C" "config" yarn-config)
    ("ge" "generate-lock-entry" yarn-generate-lock-entry)
    ("gl" "global" yarn-global)
    ("h" "help" yarn-help)
    ("im" "import" yarn-import)
    ("if" "info" yarn-info)
    ("I" "init" yarn-init)
    ("in" "install" yarn-install)]
   [("ru" "run script" yarn-run)
    ("li" "link" yarn-link)
    ("ls" "list" yarn-list)
    ("lc" "licenses" yarn-licences)
    ("ln" "login" yarn-login)
    ("lo" "logout" yarn-logout)
    ("ou" "outdated" yarn-outdated)
    ("ow" "owner" yarn-owner)
    ("pa" "pack" yarn-pack)
    ("po" "policies" transient-inapt :inapt-if (lambda () t))
    ("pu" "publish" yarn-publish)
    ("re" "remove" yarn-remove)
    ("ta" "tag" transient-inapt :inapt-if (lambda () t))]
   [("te" "team" transient-inapt  :inapt-if (lambda () t))
    ("un" "unlink" yarn-unlink)
    ("up" "upgrade" yarn-upgrade)
    ("ui" "upgrade-interactive" yarn-upgrade-interactive)
    ("ug" "unplug" transient-inapt  :inapt-if (lambda () t))
    ("v" "version" yarn-version)
    ("V" "versions" yarn-versions)
    ("wh" "why" yarn-why)
    ("wr" "workspace" transient-inapt :inapt-if (lambda () t))
    ("ws" "workspaces" transient-inapt :inapt-if (lambda () t))]])

(provide 'yarn)
;;; yarn.el ends here