# ubuntuの場合
- Metaキーが使えない場合、以下をインストールする  
```
$ sudo apt-get install gnome-panel
```

- ショートカット(ターミナル)  
```
$ emacsclient -t
```

- ショートカット(GUI)  
```
$ emacsclient -c
```

# .emacs.d ディレクトリ構成
```
.emacs.d/
  |- init.el
  |- backups/
  |- conf/
  |- elisp/
  |- elpa/
  ¥- public_repos/
```

# emacs環境構築
- ディレクトリ作成  
```
$ mkdir backups conf elisp elpa public_repos
```

- auto-installを導入  
```
$ cd ~/.emacs.d/elisp
$ curl -O https://www.emacswiki.org/emacs/download/auto-install.el
$ emacs --batch -Q -f batch-byte-compile auto-install.el
// M-x byte-compile-file RET ~/.emacs.d/elisp/auto-install.el RET
```

- anythingを導入  
```
M-x auto-install-batch RET anything RET
```

- auto-completeを導入  
```
M-x package-install RET auto-complete RET
```

- color-moccurを導入  
```
M-x auto-install-from-emacswiki RET color-moccur.el RET
```

- moccur-editを導入  
```
M-x auto-install-from-emacswiki RET moccur-edit.el RET
```

- wgrepを導入  
```
M-x package-install RET wgrep RET
```

- undohistの導入  
```
M-x install-elisp RET http://cx4a.org/pub/undohist.el
```

- undo-treeの導入  
```
M-x package-install RET undo-tree RET
```

- point-undoの導入  
```
M-x auto-install-from-emacswiki RET point-undo.el RET
```
- multi-termの導入  
```
M-x package-install RET multi-term RET
```
- psvnの導入  
```
M-x install-elisp RET http://www.xsteve.at/prg/emacs/psvn.el
```
- eggの導入  
```
$ cd ~/.emacs.d/public_repos
$ git clone git://github.com/byplayer/egg.git
```

# emacs コマンド
- S式の末尾でS式を評価する  
`C−x C-e`

### ヘルプ C-h -> C-x ? に読み替える
- 説明書を読む  
`M-x info`

- ヘルプコマンドについて調べる  
`C-h C-h`

- 入力した文字列が含まれているコマンドのリストを表示する(auto-install)  
`C-h a auto-install`
- 関数`lambda`についてのヘルプを参照する  
`C-h f lambda`

- 変数`load-path`についてのヘルプを参照する  
`C-h v load-path`

- キーバインドを確認する  
`C-h b`

- manマニュアルを見る  
`M-x man`

### ディレクトリ操作
- Diredを立ち上げる  
`C-x d`

- 次の行へ, 前の行へ  
`n` `p`

- マークする, マークをはずす, 削除対象としてマークする, 削除する  
`m` `u` `d` `x`

- 操作を一つ戻す  
`C-_`

- 指定のファイルを 削除、リネーム、コピー  
`D` `R` `C`

- 閉じる
`q`

### キーボードマクロ
- マクロの記録を開始する  
`C-x (`

- マクロの記録を終了する  
`C-x )`

- マクロを呼び出す  
`C-x e`

- マクロを10回呼び出す  
`C-u 10 C-x e`

### 検索
- grep検索  
`M-x grep`

- インクリメンタル検索(前方向、後方向)  
`C-s` `C-r`

- 正規表現版インクリメンタル検索(前方向、後方向)  
`C-M-s` `C-M-r`

### 置換
- 対話型の置換, 対話側の正規表現置換  
`M-%` `C-M-%`

- 一括置換, 一括正規表現置換  
`M-x replace-string` `M-x replace-regexp`

### アンドゥ
- アンドゥ  
`C-x u` `C-/`

### コメント
- コメントアウト, 解除 (要リージョン選択)  
`M-;`

### コピペ
- コピー (要リージョン選択)  
`M-w`

- カット (要リージョン選択)  
`C-w`

- ペースト  
`C-y`, `M-y`

### 保存
- バッファを保存  
`C-x C-s`

- 全てのバッファを保存  
`C-x s`

- 別名で保存  
`C-x C-w`

### 移動
- カーソルの位置を起点にウィンドウ表示をリフレッシュ(中央, 上, 下)  
`C-x l`

- 別ウィンドウの画面スクロール  
`C-M-v` `C-M-S-v`

- 行入力  
`M-g M-g`

### 文字サイズ
- 変更  
`C-x C-+` `C-x C--` `C-x C-0`

- 一気に変更  
`C-u 5 M-x text-scale-adjust`

### anything
- バッファ、ファイル切り替え  
`M-x anything-for-files`

- コンテキストメニュー表示 (anything表示中)  
`C-i` `TAB`

- kill-ring表示  
`M-y`

### auto-complete
- auto-complete呼び出し  
`M-TAB`

### color-moccur, moccur-edit
- マルチバッファを検索する  
`M-x moccur RET`

- カレントバッファを検索する  
`M-o`

- moccur上で編集開始、編集破棄、編集保存  
`r` `C-c C-u` `C-c C-c`

### wgrep
- grep上で編集開始、編集破棄、編集保存  
`C−c C-p` `C-c C-u` `C-c C-c`

### point-undo
- カーソルの一を戻す、やり直す  
`M-[` `M-]`

### multi-term
- multi-termを起動する  
`M-x multi-term`

### psvn
- psvnで状態を見る  
`M-x svn-status`

### egg
- git statusに相当するコマンド  
`C-x v s`

- ログを見るコマンド  
`C-x v l`

### その他
- バイトコンパイル  
`M-x byte-compile-file RET`

- ELPAのパッケージ一覧  
`M-x list-packages`

- 矩形モードに移行  
`M-RET`
