;;; adh_claude-code.el --- Major mode for Claude Code prompts -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 09 Mar 2026

;;; Commentary:
;;
;; A major mode for editing Claude Code prompts and plans.  Derived from
;; markdown-mode with soft line wrapping.  Provides org-capture-style
;; keybindings for finishing or aborting the edit.

;;; Code:

(require 'markdown-mode)
(require 'visual-fill-column)

(defun claude-code-prompt-finish ()
  "Save the buffer and close the frame.
Used to confirm the prompt or plan edit in Claude Code."
  (interactive)
  (save-buffer)
  (delete-frame))

(defun claude-code-prompt-abort ()
  "Kill the buffer without saving and close the frame.
Used to abort the prompt or plan edit in Claude Code."
  (interactive)
  (let ((buf (current-buffer)))
    (set-buffer-modified-p nil)
    (delete-frame)
    (kill-buffer buf)))

(defvar claude-code-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-code-prompt-finish)
    (define-key map (kbd "C-c C-k") #'claude-code-prompt-abort)
    map)
  "Keymap for `claude-code-prompt-mode'.")

;;;###autoload
(define-derived-mode claude-code-prompt-mode markdown-mode "Claude"
  "Major mode for editing Claude Code prompts and plans.
Derived from `markdown-mode' with soft line wrapping.

\\{claude-code-prompt-mode-map}"
  (setq header-line-format
        (substitute-command-keys
         "Claude Code prompt.  Finish \
'\\[claude-code-prompt-finish]', abort '\\[claude-code-prompt-abort]'.")))

(defun claude-code-prompt-mode-soft-wrap ()
  "Set up soft line wrapping for `claude-code-prompt-mode'.
Run from the mode hook so it overrides parent mode hooks."
  (set-fill-column 80)
  (auto-fill-mode 0)
  (visual-line-mode t)
  (toggle-word-wrap t)
  (visual-fill-column-mode t))

(add-hook 'claude-code-prompt-mode-hook #'claude-code-prompt-mode-soft-wrap)

;;;###autoload
(add-to-list 'auto-mode-alist '("claude-prompt-.*\\.md\\'" . claude-code-prompt-mode))

(provide 'adh_claude-code)
;;; adh_claude-code.el ends here
