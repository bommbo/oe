# oe
Maybe more powerful than oil.nvim
```
▼ oe
  ├─ ▶ .git
  ├─ 📄 .gitignore
  ├─ 📄 LICENSE
  ├─ 📝 README.md
  ├─ 📜 oe-session.el
  ├─ 📜 oe-trash.el
  └─ 📜 oe.el
```
# Installation

```
(add-to-list 'load-path "xxx/oe")
(require 'oe)
(require 'oe-trash)
(require 'oe-session)
(setq oe-history-limit 200)
(setq oe-history-deduplicate 'adjacent)
(setq oe-default-tree-mode t)

(defun find-file--handle-directory (orig-fun &rest args)
  "If the file is a directory, open it in oe instead of Dired."
  (let ((filename (car args)))
	(if (and (stringp filename)
			 (file-directory-p (file-name-as-directory filename)))
		(oe-open (file-name-as-directory filename))
	  (apply orig-fun args))))

(advice-add 'find-file :around #'find-file--handle-directory)
```
# Function

| Command                 | Key     |
|-------------------------|---------|
| edit mode <-> tree mode | C-c C-t |
| oe <-> dired            | C-c o   |

## edit mode

| Command | Key     |
|---------|---------|
| oe-save | C-c C-c |

## tree mode

| Command                  | Key   |
|--------------------------|-------|
| oe-enter                 | RET   |
| oe-up                    | ^     |
| oe-toggle-fold           | tab   |
| oe-toggle-selection      | space |
| oe-selecte-all           | *     |
| oe-toggle-all-selections | t     |
| oe-copy                  | C     |
| oe-cut                   | X     |
| oe-paste                 | P     |
| oe-oe-delete-selected    | D     |
| oe-mark                  | m     |
| oe-unmark                | u     |
| oe-unmark-all            | U     |
| oe-preview               | p     |

## more

- `oe-session-save`
- `oe-session-restore`
- `oe-session-delete`
- `oe-session-recent`
...

---
- `oe-delete-selected-with-trash`

# Similar projects

https://github.com/mwac-dev/grease.el

https://github.com/yibie/Oil.el/
