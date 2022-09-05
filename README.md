# 概要

VSCode で Rmd から `bookdown::render()` を使ってファイル出力するとき用のテンプレ.
scr ディレクトリで書いてビルドタスクを実行すると, 出力ファイルが docs ディレクトリに保存される.
Serve and Open Book タスクを実行すると, 結果をプレビューしながらソースファイルの編集ができる.

## Tasks.json

tools フォルダ内のスクリプトを実行しているだけ. 各タスクの具体的な中身はその名前に対応するスクリプトや json 本体を参照. ここでは概要のみ記す.

- Render Book : ビルドタスク. scr 内の先頭の Rmd ファイルを見つけて `bookdown::render_book()` を実行する.
- Open Book in Chrome : ビルドされた book_filename.html を見つけて Chrome で開く.
- Serve and Open Book: `bookdown::serve_book()` を実行して Chrome で開く.
- Clear Rendered Results : 一時オブジェクトおよび docs ディレクトリ内のファイルを破棄し, メモリを掃除する.

## dir_config.yml

`bookdown::render_book()` に関わるディレクトリやファイルの情報を管理する.
ほとんど全てのスクリプトがこのファイルの情報を直接的または間接的に参照している. ディレクトリ名/構成を変えたらこの中身もそれに応じて変えること. 構成を変えた場合は, tools.R 内のディレクトリを発見する関数群に対しても変更の必要がたぶん生じる. `get_output_dir()` とか `get_book_filename()` とか.

## tools

Tasks.json で実行される R スクリプト達.

- tools.R: `bookdown::render_book()` 等が使う各種ディレクトリやファイルのパスを見つける関数が入っている.
- render_book.R: 同名のタスクの具体的な実行内容. dir_config.yml から取ってきたディレクトリやファイル情報を取得して `bookdown::render_book()` に流し込む.
  - `input =` dir_config.yml の `rmd_basename`.
  - `output_dir =` project-root 直下のフォルダで, dir_config.yml の `output_dir_basename` に記載された名前と等しいフォルダ. このフォルダが存在しなければ自動的に作成される.
- serve_book.R: render_book.R と同様.
- open_in_chrome.R: ビルドされた book_filename.html を見つけて Chrome で開く. book_filename は _bookdown.yml を読み込んで自動的に取得される. `get_rendered_book()` によって最初に発見されたものが開かれる.
- clear_all_output.R: dir_config.yml の記載に従って検索された出力フォルダをクリアする. 意図しないフォルダをクリアしないために, プロジェクト外のフォルダを参照したり, 出力フォルダ階層下に Rmd ファイルが含まれているとエラーで止まる.

project-root は, `rprojroot::is_git_root` と定義している. 必要に応じて変更して使うこと.

## 参考

- [Authoring Books with R Markdown](https://bookdown.org/yihui/bookdown/configuration.html)
- [R Markdown: The Definitive Guide](https://bookdown.org/yihui/rmarkdown/)
- [rprojroot](https://rprojroot.r-lib.org/index.html)
