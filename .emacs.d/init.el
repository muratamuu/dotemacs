(require 'package)

;; ~/.emacs.d/elisp をロードパスに追加する
;; add-to-load-path関数を作成したので不要
;; (add-to-list 'load-path "~/.emacs.d/elisp")

;; Emacs 23より前のバージョンを利用している場合
;; user-emacs-directory変数が未定義のため次の設定を追加
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;; MELPAを追加
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Marmaladeを追加
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; 初期化
(package-initialize)

;; 行番号表示
;;(global-linum-mode t)

;; ツールバー非表示
;;(tool-bar-mode -1)

;; メニューバー非表示
(menu-bar-mode -1)

;; シェルのパス設定
;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))

;; 英語フォント
;;(set-face-attribute 'default nil
;;  :family "Menlo" ;; font
;;  :height 100)    ;; font size

;; 日本語フォント
;;(set-fontset-font
;;  nil 'japanese-jisx0208
;;  (font-spec :family "Hiragino Kaku Gothic ProN"))

;;(setq face-font-rescale-alist
;;  '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)))

;; Metaキーにcommandを割り当てる
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; 背景を透過
;;(set-frame-parameter nil 'alpha 85)

;; メニューバーにファイルパスを表示する
;;(setq frame-title-format
;;  (format "%%f - Emacs@%s" (system-name)))

;; バックアップファイルを作らない
;; (setq make-backup-files nil)
;; (setq auto-save-default nil)
;; (setq delete-auto-save-files t)

;; バックアップとオートセーブファイルを~/.emacs.d/backupsへ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; タブをスペースに変換する
(setq-default tab-width 4 indent-tabs-mode nil)

;; 末尾のホワイトスペースを目立たせる
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))

;; "C-t"でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; 現在行のハイライト
(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景色がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my-face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; paren-mode:対応する括弧を強調して表示する
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化
;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; カラム番号もモードラインに表示する
(column-number-mode t)

;; ファイルサイズをモードラインに表示する
(size-indication-mode t)

;; egg読み込み
(when (executable-find "git")
  (require 'egg nil t))
