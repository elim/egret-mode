;; 2012 Takeru Naito
;; MIT License.

(require 'popwin)
(require 'time-date)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define mode
;;

(define-derived-mode egret-mode nil "egret" "*nodoc*"
  (define-key egret-mode-map (kbd "C-c C-c") 'egret-el-create-note-from-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define constant
;;

(defconst egret-el-apllescript-command-format
  "\
tell application \"Evernote\"
  create note from file \"%s\" notebook \"%s\"
end tell")

(defconst egret-el-task-format
  "\
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE en-export SYSTEM \"http://xml.evernote.com/pub/evernote-export2.dtd\">
<en-export export-date=\"%s\" application=\"Evernote\" version=\"\">
<note><title>%s</title><content><![CDATA[<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!DOCTYPE en-note SYSTEM \"http://xml.evernote.com/pub/enml2.dtd\">
<en-note style=\"word-wrap: break-word; -webkit-nbsp-mode: space; -webkit-line-break: after-white-space;\">
<en-todo/>%s<br/>
<table style=\"background-color:#f7f7f7;border-width:1px;max-width:450\" width=\"450\" border=\"0\" summary=\"egretlist-note\">
<tr>
<td>
%s
</td>
</tr>
</table>
</en-note>
]]></content>
<created>%s</created>
<updated>%s</updated>
<note-attributes>
<author>egret.el</author>
</note-attributes>
</note>
</en-export>
"
  )

(defconst egret-el-detail-format
	"\
<div style=\"
border-bottom: 1px solid #d3d5fe;
min-height: 17px;
max-width: 450;
font-size: 14px;
font-family: Helvetica\">
%s
</div>
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define variable
;;

(defvar egret-el-timestamp "")
(defvar egret-el-task-name "")
(defvar egret-el-task-detail "")
(defvar egret-el-temporary-file
  (expand-file-name "egret-el.enex" temporary-file-directory))
(defvar egret-el-default-notebook "...inbox")
(defvar egret-mode-hook nil)
(defvar egret-cooperate-ddskk (featurep 'skk))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define function
;;

(defun egret-el-create-input-buffer ()
  (interactive)
  (let*
      ((buffer  (get-buffer-create "*egret*"))
       (windows (popwin:create-popup-window))
       (master-win (car windows))
       (popup-win (cadr windows))
       (skk-j-state (and (boundp skk-j-mode) skk-j-mode)))

    (select-window popup-win)
    (switch-to-buffer buffer)
    (erase-buffer)
    (egret-mode)
    (when (and egret-cooperate-ddskk skk-j-state) (skk-j-mode-on))
    (run-hooks egret-mode-hook)))


(defun egret-el-create-note-from-buffer ()
  (interactive)
  (let*
      ((content (buffer-string))
       (task-name (car (split-string content "\n")))
       (task-detail (cdr (split-string content "\n")))
       (task-detail (mapconcat #'identity task-detail "\n"))
       (task-detail (replace-regexp-in-string "^\\s-+\\|\\s-+$" "" task-detail)))

    (egret-el-internal-set-timestamp)
    (egret-el-internal-set-task task-name task-detail)
    (egret-el-internal-create-temporary-note-file)
    (egret-el-internal-post)
    (delete-window)))


(defun egret-el-create-note-from-interactive (task-name task-detail)
  "*nodoc*"
  (interactive "sTask Name: \nsDetail: ")

  (egret-el-internal-set-timestamp)
  (egret-el-internal-set-task task-name task-detail)
  (egret-el-internal-create-temporary-note-file)

  (egret-el-internal-post))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define function (internal)
;;

(defun egret-el-internal-post ()
  (funcall
   (cond
    ((eq system-type 'darwin)
     #'egret-el-internal-post-mac)
    ((eq system-type 'windows-nt)
     #'egret-el-internal-post-win))))


(defun egret-el-internal-post-mac ()
  (do-applescript
   (format egret-el-apllescript-command-format
           egret-el-temporary-file
           egret-el-default-notebook)))


(defun egret-el-internal-post-win ()
  (call-process
   "C:/Program Files/Evernote/Evernote/ENScript"
           nil nil nil
           "importNotes"
           "/s" egret-el-temporary-file
           "/n" egret-el-default-notebook))


(defun egret-el-internal-format-detail ()
  "*nodoc*"
  (let
      ((details (split-string task-detail "\n")))
    (mapconcat (lambda (x)
                 (format egret-el-detail-format x))
               details "")))


(defun egret-el-internal-set-timestamp ()
  "*nodoc*"
  (interactive)
  (let*
      ((now (current-time))
       (time-difference (seconds-to-time (car (current-time-zone))))
       (utc (time-subtract now time-difference))
       (timestamp (format-time-string "%Y%m%dT%H%M%SZ" utc)))

    (setq egret-el-timestamp timestamp)))


(defun egret-el-internal-format-note ()
  "*nodoc*"
  (let
      ((note (format egret-el-task-format
                     egret-el-timestamp   ; export time
                     egret-el-task-name   ; note title
                     egret-el-task-name   ; todo title
                     egret-el-task-detail ; detail
                     egret-el-timestamp   ; create time
                     egret-el-timestamp   ; update time
                     )))
    (replace-regexp-in-string "\n" "" note)))


(defun egret-el-internal-set-task (task-name task-detail)
  (egret-el-internal-set-task-name task-name)
  (egret-el-internal-set-task-detail task-detail))


(defun egret-el-internal-set-task-name (task-name)
  (setq egret-el-task-name task-name))


(defun egret-el-internal-set-task-detail (task-detail)
  (setq egret-el-task-detail
        (egret-el-internal-format-detail)))


(defun egret-el-internal-create-temporary-note-file ()
  (let
      ((buffer (find-file-noselect egret-el-temporary-file)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (egret-el-internal-format-note))
      (save-buffer))))


(provide 'egret-mode)
