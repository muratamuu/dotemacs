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

;; package.elの設定
(when (require 'package nil t)
  ;; パッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA", "http://tromey.com/elpa/"))
  ;; 初期化
  (package-initialize))

;; auto-installの設定
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; (setq url-proxy-services '(("http", . "localhost:8339")))
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

;; auto-installでインストールしたelispのurl
;; M-x install-elisp RET or 以下のS式を*scratch*で評価(C-j)
;; (install-elisp "http://www.emacswiki.org/emacs/download/redo+.el")

;; redo+の設定
(when (require 'redo+ nil t)
  ;; C-'にリドゥを割り当てる
  ;; (global-set-key (kbd "C-'") 'redo)
  ;; 日本語キーボードのばあいC-.の方がいいかも
  (global-set-key (kbd "C-.") 'redo)
  )

;; anythingの設定
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描画するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root権限でアクションを実行するときのコマンド
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install))

  (define-key global-map (kbd "M-y") 'anything-show-kill-ring))

;; 要color-moccur.el (現在未インストール)
;; http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el
(when (require 'anything-c-moccur nil t)
  (setq
   ;; anything-c-moccur用 `anything-idle-delay'
   anything-c-moccur-anything-idle-delay 0.1
   ;; バッファの情報をハイライトする
   anything-c-moccur-higligt-info-line-flag t
   ;; 現在選択中の候補の位置を他のwindowに表示する
   anything-c-moccur-enable-auto-look-flag t
   ;; 起動時にポイントの位置の単語を初期パターンにする
   anything-c-moccur-enable-initial-pattern t)
  ;; C-M-oにanything-c-moccur-occur-by-moccurを割り当てる
  (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur))

;; auto-completeの設定
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; color-moccurの設定
;; M-x auto-install-from-emacswiki RET color-moccur.el RET
(when (require 'color-moccur nil t)
  ;; M-oにoccur-by-moccurを割り当て
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のとき除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  ;; Migemoを利用できる環境であればMigemoを使う
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (setq moccur-use-migemo t))
  ;; moccur-edit.el
  (require 'moccur-edit nil t))

;; wgrepの設定 ELPA *grep* でC-c C-pで編集可能 C-c C-kで破棄 C-c C-cで反映
(require 'wgrep nil t)

;; undohistの設定
;; M-x install-elisp RET http://cx4a.org/pub/undohist.el
(when (require 'undohist nil t)
  (undohist-initialize))

;; undotreeの設定
;; M-x package-install RET undo-tree RET
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; point-undoの設定
;; M-x auto-install-from-emacswiki RET point-undo.el RET
(when (require 'point-undo nil t)
  (define-key global-map (kbd "M-[") 'point-undo)
  (define-key global-map (kbd "M-]") 'point-redo))

;; Subversionフロントエンドpsvnの設定
;; M-x install-elisp RET http://www.xsteve.at/prg/emacs/psvn.el
(when (executable-find "svn")
  (setq svn-status-verbose nil)
  (autoload 'svn-status "psvn" "Run `svn status'." t))

;; Gitフロントエンドeggの設定
;; M-x install-elisp RET https://raw.github.com/byplayer/egg/master/egg.el
;; M-x install-elisp RET https://raw.github.com/byplayer/egg/master/egg-grep.el
(when (executable-find "git")
  (require 'egg nil t))

;; multi-termの設定
;; M-x package-install RET multi-term RET
(when (require 'multi-term nil t)
  ;; 使用するシェルを設定
  ;;(setq multi-term-program "/usr/local/bin/zsh"))
  (setq multi-term-program "/bin/bash"))

;; womanキャッシュを作成
(setq woman-cache-filename "~/.emacs.d/.wmncach.el")

;; manパスを設定
(setq woman-manpath '("/usr/share/man"
                      "/usr/local/share/man"
                      "/usr/local/share/man/ja"))

;; anything-for-document用のソースを定義
(setq anything-for-document-sources
      (list anything-c-source-man-pages
            anything-c-source-info-cl
            anything-c-source-info-pages
            anything-c-source-info-elisp
            anything-c-source-apropos-emacs-commands
            anything-c-source-apropos-emacs-functions
            anything-c-source-apropos-emacs-variables))

;; anything-for-documentコマンドを生成
(defun anything-for-document ()
  "Preconfigured `anything' for anything-for-document."
  (interactive)
  (anything anything-for-document-sources
            (thing-at-point 'symbol) nil nil nil
            "*anything for document*"))

;; Command+dにanything-for-documentを割り当て
(define-key global-map (kbd "C-c d") 'anything-for-document)

;; anything-for-filesを割り当て
(define-key global-map (kbd "C-c f") 'anything-for-files)

;; 行番号表示
;;(global-linum-mode t)

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバー非表示
(menu-bar-mode -1)

;; シェルのパス設定
;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))

;; 文字コードを指定する
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Mac OS Xの場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; Windowsの場合のファイル名の設定
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locate-coding-system 'cp932))

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

;; "C-m"にnewline-and-indentを割り当てる
;; global-set-key = define-key global-map
(global-set-key (kbd "C-m") 'newline-and-indent)

;; 入力されるシーケンスを置き換える
;; ?\C-?はDELのシーケンス
(keyboard-translate ?\C-h ?\C-?)

;; 別のキーバインドにヘルプを割り当てる
(define-key global-map (kbd "C-x ?") 'help-command)

;; "C-t"でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; コンパイルコマンド
(define-key global-map (kbd "C-c c") 'compile)

;; リージョンの背景色を変更
;;(set-face-background 'region "darkgreen")

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
;;(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; カラム番号もモードラインに表示する
(column-number-mode t)

;; ファイルサイズをモードラインに表示する
(size-indication-mode t)

;; cua-modeの設定
(cua-mode t)
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効にする
(define-key global-map (kbd "M-RET") 'cua-set-rectangle-mark)

;; egg読み込み
(when (executable-find "git")
  (require 'egg nil t))

;; C/C++のスタイル
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "gnu")))

;; Makefileの種類を定義
(defvar flymake-makefile-filenames
  '("Makefile" "makefile" "GNUmakefile")
  "File names for make.")

;; Makefileがなければコマンドを直接利用するコマンドラインを生成
(defun flymake-get-make-gcc-cmdline (source base-dir)
  (let (found)
    (dolist (makefile flymake-makefile-filenames)
      (if (file-readable-p (concat base-dir "/" makefile))
          (setq found t)))
    (if found
        (list "make"
              (list "-s"
                    "-C"
                    base-dir
                    (concat "CHK_SOURCES=" source)
                    "SYNTAX_CHECK_MODE=1"
                    "check-syntax"))
      (list (if (string= (file-name-extension source) "c") "gcc" "g++")
            (list "-o"
                  "/dev/null"
                  "-fsyntax-only"
                  "-Wall"
                  source)))))

;; Flymake初期化関数の生成
(defun flymake-simple-make-gcc-init-impl
    (create-temp-f use-relative-base-dir
                   use-relative-source build-file-name get-cmdline-f)
  "Create syntax check command line for a directly checked source file. Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
         (source-file-name buffer-file-name)
         (buildfile-dir (file-name-directory source-file-name)))
    (if buildfile-dir
        (let* ((temp-source-file-name
                (flymake-init-create-temp-buffer-copy create-temp-f)))
          (setq args
                (flymake-get-syntax-check-program-args
                 temp-source-file-name
                 buildfile-dir
                 use-relative-base-dir
                 use-relative-source
                 get-cmdline-f))))
    args))

;; 初期化関数を定義
(defun flymake-simple-make-gcc-init ()
  (message "%s" (flymake-simple-make-gcc-init-impl
                 'flymake-create-temp-inplace t t "Makefile"
                 'flymake-get-make-gcc-cmdline))
  (flymake-simple-make-gcc-init-impl
   'flymake-create-temp-inplace t t "Makefile"
   'flymake-get-make-gcc-cmdline))

;; 拡張子 .c, .cpp, c++などのときに上記関数を利用する 要warning対応
;; (add-to-list 'flymake-allowed-file-name-masks
;;              ;;'("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'"
;;              '("\\.\\(cc\\|cpp\\|cxx\\|h\\|hpp\\)\\'"
;;                flymake-simple-make-gcc-init))

;; ファイルが#!から始まる場合、+xをつけて保存する
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; emacs-lisp-mode用の関数を定義
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))

;; emacs-lisp-modeのフックをセット
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; scala mode 2
;; M-x package-install RET scala-mode2
(require 'scala-mode2)
