(require 'package)

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
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq delete-auto-save-files t)

;; タブをスペースに変換する
(setq-default tab-width 4 indent-tabs-mode nil)

;; 末尾のホワイトスペースを目立たせる
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))

;; "C-t"でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
