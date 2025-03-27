;;; dot-command.el --- mimic vim dot command  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  sychen52

;; Author: <sychen52@gmail.com>
;; Keywords: lisp, vim dot command, repeat
;; Version: 0.0.1

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

;; This package try to mimic vim dot command through recent-keys.
;; Instead of adding a hook to *-command-hook, which gets executed
;; everytime you press a key. This will only run when you press C-. for
;; example. I will look up the last group of edit commands in recent-keys
;; and then execute them.

;;; Code:
;; we maintain a state machine while looping through the recent-keys.
;; possible state: before-recording, recording, completion, done
(defvar dot-command-neutral-cmds
  '()
  "These commands will not change status, i.e, if it is recording, these will be recorded. If it is not, these will be skipped/ignored.")
(defvar dot-command-record-cmds
  '(self-insert-command
    newline
    kill-word
    backward-kill-word
    delete-char
    python-indent-dedent-line-backspace
    backward-delete-char-untabify
    zap-to-char
    zap-up-to-char
    read-char-from-minibuffer-insert-char)
  "A consecutive series of these commands will be recorded.")
(defvar dot-command-completion-cmds
  '(corfu-next corfu-previous corfu-complete corfu-insert dabbrev-completion completion-at-point)
  "This is used to handle auto completion package such as corfu or company.
A consecutive series of these commands will be replaced with dot-command's completion."
  )
(defvar dot-command-macro nil "recorded kmacro")
(defvar dot-command-completion-ring-size 5 "The length of the recorded completion commands.")
(defvar dot-command--completion-ring
  (make-ring dot-command-completion-ring-size)
  "The ring to record completion commands (beg end newtext).
The elements are inserted in order, so index 0 is the last inserted, and the last completion command in time.")
(defvar dot-command--completion-start -1 "The index of the first completion commands to replay")
(defvar dot-command--completion-index -1 "The index of the current completion commands")

(defun dot-command-execute ()
  "Go through recent-keys in reverse order to find a dot-command to repeat.
This dot-command will also be saved into last-kbd-macro.
If no dot-command is find, it will play last-kbd-macro.
A dot-command is the last consecutive dot-command-record-cmds and/or dot-command-completion-cmds, similar to vim dot command."
  (interactive)
  (setq dot-command--completion-start -1)
  (setq dot-command-macro nil)
  (let* ((rk (recent-keys 'include-cmds)) ;; recent keys list. format: [int_or_symbol_is_key * n, (nil . cmd), ...]
         (idx (length rk))
         (status 'before-recording) ;; before-recording, recording, completion, done
         (item nil)  ;; item in the recent keys list. it could be a key or a cmd
         (keys nil)
         (cmd nil))
    ;; (pp rk)
    (while (and (> idx 0) (not (eq status 'done)))
      (setq idx (1- idx)) ;; loop in reverse order
      (setq item (aref rk idx))
      (if (or (integerp item) (symbolp item))  ;; is a key
          (push item keys) ;; this will revert the keys' order back
        ;; is a cmd.
        (when keys
          ;; process the previous stored key and cmd. This will skip the first cmd and cmd without a key,
          (setq status (dot-command--process keys cmd status))
          (setq keys nil))
        (setq cmd (cdr item)))) ;; wait for the next iteration
    (when (eq status 'done)
      (kmacro-display dot-command-macro)
      (setq last-kbd-macro (vconcat dot-command-macro []))
      (setq dot-command--completion-index dot-command--completion-start)))
  (kmacro-end-and-call-macro 1))

(keymap-global-set "C-." #'dot-command-execute)

(defun dot-command--process (keys cmd status)
  "Go through the cmd in reverse order and return the new status.
Possible status:
before-recording,
recording,
completion,
done,"
  (cond
   ((member cmd dot-command-neutral-cmds)  ;; neutral
    (unless (eq status 'before-recording)  ;; record if it is already in recording/completion status
      (setq dot-command-macro (append keys dot-command-macro)))
    status)
   ((member cmd dot-command-record-cmds)  ;; record
    (when (eq cmd #'self-insert-command)
      (setq keys (list (car keys))))  ;; in python model for example, keys will repeat multiple times for some reason.
    (setq dot-command-macro (append keys dot-command-macro))
    'recording)
   ((member cmd dot-command-completion-cmds)  ;; completion
    (unless (eq status 'completion)  ;; a series of completion-cmds will only insert one dot-command--play-completion.
      (setq dot-command--completion-start (1+ dot-command--completion-start))
      (when (>= dot-command--completion-start dot-command-completion-ring-size)
        (message "you need a larger dot-command--completion-ring"))
      (setq dot-command-macro (append '(3 63) dot-command-macro)))
    (if (member cmd '(completion-at-point dabbrev-completion))  ;; In case you trigger two completions in a row, these cmds indicate it is a new start of a series of completion in a row.
        'recording
      'completion))
   (t
    (if (eq status 'recording)
        'done  ;; interupt recording
      'before-recording  ;; not started
      ))))

(defun dot-command--record-completion (beg end newtext)
  (let* ((p (point))
         (beg (- beg (point)))
         (end (- end (point))))
    (ring-insert dot-command--completion-ring `(,beg ,end ,newtext))))
(advice-add #'completion--replace :before #'dot-command--record-completion)
(fset 'completion--replace-org (advice--cd*r (symbol-function #'completion--replace)))

(defun dot-command--replay-completion ()
  "A consecutive series of dot-command-completion-cmds will be replaced with a dot-command--play-completion.
This will replay the completion command."
  (interactive)
  (let* ((arg (ring-ref dot-command--completion-ring dot-command--completion-index))
         (p (point))
         (beg (+ (car arg) p))
         (end (+ (cadr arg) p))
         (newtext (caddr arg)))
    (completion--replace-org beg end newtext)
    (setq dot-command--completion-index (1- dot-command--completion-index))))

;; dot-command--replay-completion needs to be bound to a key so that it can be added to the kmacro
(keymap-global-set "C-c ?" #'dot-command--replay-completion)  ;; (3 63)

(provide 'dot-command)
;;; dot-command.el ends here
