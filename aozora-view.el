;;; aozora-view.el --- Aozora Bunko text Emacs viewer.  -*- lexical-binding: t -*-

;; Filename: aozora-view.el
;; Description: Aozora Bunko text Emacs viewer.
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2012-01-01
;; Version: 1.140310
;; Keywords: text
;; Human-Keywords: aozora bunko
;; URL: https://github.com/kawabata/aozora-view

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; This support file provides displaying capability to the "青空文庫"
;; text file.

;;; Code:

;; Customization

(require 'view)
(defvar aozora-fill-column 0.8)

(defvar aozora-view-cache-directory
  (concat temporary-file-directory "/aozora-view/"))
(defvar aozora-view-cache-ext    ".cache.gz")
(defvar aozora-view-save-cache   'prompt) ;; t, nil, 'prompt

;; Variables

(defvar aozora-view-text-buffer nil
  "現在の青空ビューバッファに対応するテキストバッファ。")
(make-variable-buffer-local 'aozora-view-text-buffer)

(defvar aozora-view-text-file nil
  "現在の青空ビューバッファに対応するテキストファイル。")
(make-variable-buffer-local 'aozora-view-text-file)

(defvar aozora-view-buffer nil
  "現在のテキストバッファに対応する青空ビューバッファ。")
(make-variable-buffer-local 'aozora-view-buffer)

(defvar aozora-view-bookmarks nil)
;; If you are using session.el, include the above variable for saving among sessions.
;; e.g. 
;; (add-to-list 'session-globals-include 'aozora-view-bookmarks)

(defvar aozora-kenten-alist
  '(("傍点" .       ?﹅)
    ("白ゴマ傍点" . ?﹆)
    ("丸傍点" .     ?●)
    ("白丸傍点" .   ?○)
    ("傍点（白丸）" . ?○) ;; 特殊例
    ("黒三角傍点" . ?▲)
    ("白三角傍点" . ?△)
    ("二重丸傍点" . ?◎)
    ("蛇の目傍点" . ?◉)))

(defvar aozora-kenten-regexp
  (concat "［＃「\\([^」]+?\\)」に"
          (regexp-opt (mapcar 'car aozora-kenten-alist) t)
          "］\n?"))

(define-translation-table 'aozora-accent-table
  (make-translation-table-from-alist
   '(([?! ?@] . ?¡) ([?? ?@] . ?¿)
     ([?A ?E ?&] . ?Æ) ([?a ?e ?&] . ?æ)
     ([?O ?E ?&] . ?Œ) ([?o ?e ?&] . ?œ)
     ([?a ?&] . ?å) ([?A ?&] . ?Å) ([?S ?&] . ?ß)
     ([?O ?/] . ?Ø) ([?o ?/] . ?ø)
     (?` . ?\x300)
     (?' . ?\x301)
     (?^ . ?\x302)
     (?~ . ?\x303)
     (?: . ?\x308)
     (?& . ?\x30a)
     (?, . ?\x327)
     (?_ . ?\x304))))

;; Keymap

(defvar aozora-view-mode-map nil
  "*Keymap for Aozora View mode.")

(unless aozora-view-mode-map
  (setq aozora-view-mode-map (make-sparse-keymap))
  (set-keymap-parent aozora-view-mode-map view-mode-map)
  (define-key aozora-view-mode-map "b" 'aozora-view-bookmark)
  (define-key aozora-view-mode-map "," 'aozora-view-restore-bookmark)
  (define-key aozora-view-mode-map "q" 'aozora-view-suspend)
  (define-key aozora-view-mode-map "t" 'aozora-view-traditional)
  (define-key aozora-view-mode-map "l" 'aozora-view-redraw))

;; Mode Definition

(define-derived-mode aozora-view-mode text-mode "青空文庫"
  "青空文庫 View Mode.
Do not call this directly.  Execute `aozora-view' instead."
  (buffer-disable-undo)
  (toggle-read-only 1)
  (view-mode)
  (use-local-map aozora-view-mode-map)
  (setq line-spacing 0))

;;; Main Program

(defun aozora-view-arrange-replace ()
  "青空文庫方式に従い、外字を置換し、ルビを設定する。"
  (set-text-properties (point-min) (point-max) nil)
  (goto-char (point-min))
  (while (re-search-forward "※［[^］]+\\([12]\\)-\\([0-9]+\\)-\\([0-9]+\\)\\(、.+?\\)?］" nil t)
    (let ((plane  (string-to-number (match-string 1)))
          (row    (+ 32 (string-to-number (match-string 2))))
          (column (+ 32 (string-to-number (match-string 3)))))
      (replace-match
       (char-to-string (make-char (if (= plane 1) 'japanese-jisx0213.2004-1 'japanese-jisx0213-2)
                                  row column)))))
  (goto-char (point-min))
  (while (re-search-forward "※［＃[^、］]+、UCS-\\([0-9A-F]+\\)、[^］]+］" nil t)
    (replace-match (char-to-string (string-to-number (match-string 1) 16))))
  (goto-char (point-min))
  (while (re-search-forward "※［＃\\([^、］]+\\)、[^］]+］" nil t)
    (let ((char (gethash (match-string 1) aozora-view-gaiji-table)))
      (if char (replace-match char))))
  (goto-char (point-min))
  (while (search-forward "／＼" nil t) (replace-match "〳〵"))
  (goto-char (point-min))
  (while (search-forward "／″＼" nil t) (replace-match "〴〵"))
  ;; 圏点（﹅◉﹆﹅◦•）
  ;; TODO: ルビとの重ね表示への対応
  ;; TODO: 長文圏点への対応（改行されないのを防ぐ）
  (goto-char (point-min))
  (while (re-search-forward aozora-kenten-regexp nil t)
    (let* ((text (match-string 1))
           (size (length text))
           (dot (cdr (assoc (match-string 2) aozora-kenten-alist ))))
      (replace-match "")
      (if (search-backward text nil t)
          (replace-match (concat "｜" text "《"
                                 (apply 'string (make-list size dot)) "》"))
        (message "圏点指示の対応テキストが見つかりません！")
        )))
  ;; 漢文
  (goto-char (point-min))
  (while (re-search-forward "［＃\\([レ一二三四上中下甲乙丙丁天地人]+\\)］\n?" nil t)
    (replace-match (propertize (match-string 1)
                               'display '((height 0.5)))))
  ;; 漢字の小書
  (goto-char (point-min))
  (while (re-search-forward "［＃（\\(.+?\\)）］\n?" nil t)
    (replace-match (propertize (match-string 1)
                               'display '((height 0.5) (raise 1))
                               )))
  ;; 行右小書き
  (goto-char (point-min))
  (while (re-search-forward "［＃「\\([^」]+\\)」は行右小書き］\n?" nil t)
    (let* ((text (match-string 1)))
      (replace-match "")
      (if (search-backward text nil t)
          (replace-match (propertize text
                                     'display '((height 0.5))))
        (error "行右小書き指示の対応テキストが見つかりません！"))))
  ;; 下付き注記
  (goto-char (point-min))
  (while (re-search-forward "［＃「\\(.?\\)」は下付き小文字］\n?" nil t)
    (let* ((text (match-string 1)))
      (replace-match "")
      (if (search-backward text nil t)
          (replace-match (propertize text
                                     'display '((height 0.5))))
        (error "下付き指示の対応テキストが見つかりません！"))))
  ;; 上付き注記
  (goto-char (point-min))
  (while (re-search-forward "［＃「\\(.?\\)」は上付き小文字］\n?" nil t)
    (let* ((text (match-string 1)))
      (replace-match "")
      (if (search-backward text nil t)
          (replace-match (propertize text
                                     'display '((height 0.5) (raise 1))))
        (error "上付き注記指示の対応テキストが見つかりません！"))))
  ;; ママ注記
  ;; e.g. ［＃「喋」に「ママ」の注記］
  ;; TODO: ルビとの重ね表示への対応
  (goto-char (point-min))
  (while (re-search-forward "［＃「\\(.+?\\)」に「ママ」の注記］" nil t)
    (let* ((text (match-string 1)))
      (replace-match "")
      (if (search-backward text nil t)
          (replace-match (concat "｜" text "《ママ》"))
        (error "ママ注記指示の対応テキストが見つかりません！")
        )))
  ;; 傍線処理
  (goto-char (point-min))
  (while (re-search-forward "［＃「\\([^」]+\\)」に傍線］\n?" nil t)
    (let* ((text (match-string 1)))
      (replace-match "")
      (if (search-backward text nil t)
          (put-text-property (match-beginning 0) (match-end 0)
                             'face 'underline)
        nil ;;(error "傍線指示の対応テキストが見つかりません！")
        )))
  ;; アクセント処理
  ;; 〔〕：アクセント分解された欧文をかこむ
  ;; http://www.aozora.gr.jp/accent_separation.html
  (goto-char (point-min))
  (when (re-search-forward "〔〕.*アクセント" nil t)
    ;; アクセント注記がある作品。（なお〔〕の先頭文字はアクセントとみなさない）
    (while (re-search-forward "〔.\\(.+?\\)〕" nil t)
      (let ((accented-string (match-string 1)))
        (replace-match
         (save-match-data
           (with-temp-buffer
             (insert accented-string)
             (translate-region (point-min) (point-max)
                               'aozora-accent-table)
             (ucs-normalize-NFC-region (point-min) (point-max))
             (buffer-string)))))))
  ;; 割注処理
  (goto-char (point-min))
  (while (search-forward "［＃ここから割り注］" nil t)
    (let (start end)
      (replace-match "")
      (setq start (point))
      (if (re-search-forward "［＃\\(ここで\\)?割り注終わり］" nil t)
          (progn
            (replace-match "")
            (put-text-property start (point)
                               'display '((height 0.5) (raise 0.5))))
        (error "割注終了指示が見付かりません。"))))

  ;; 字下げ処理
  (goto-char (point-min))
  (while (re-search-forward "［＃ここから\\([0-9０-９]+\\)字下げ］\n?" nil t)
    (let ((start (match-beginning 0))
          (margin (string-to-number (save-match-data (japanese-hankaku (match-string 1))))))
      (replace-match "")
      (if (re-search-forward "［＃ここで字下げ終わり］\n?" nil t)
          (progn
            (put-text-property start (match-beginning 0) 'left-margin (* 2 margin))
            (replace-match ""))
        (error "[字下げ] instruction does not match!"))))
  ;; その他の指示は別途実装予定：
  ;; ［＃天からn字下げ］
  ;; ［＃地からn字上げ］
  ;; ［＃ここから２字下げ］
  ;; ［＃ここから改行天付き、折り返してn字下げ］
  ;; ［＃ここからn字下げ、折り返してn字下げ］
  ;; ［＃地付き］
  ;; ［＃改ページ］
  ;; ［＃ここから横組み］～［＃ここで横組み終わり］
  (goto-char (point-min))
  (while (re-search-forward "^." nil t)
    (put-text-property (match-beginning 0) (match-end 0)
                       'line-number (line-number-at-pos (match-beginning 0))))
  ;; ルビをつけて、改行禁止にする。
  (goto-char (point-min))
  (while (re-search-forward
          "\\(\\(?:｜.+?\\)\\|\\(?:[a-z`※㐀-鿿󠄀-󠇿]+々*〻?※?\\)\\)《\\(.+?\\)》" nil t)
    (let ((string (match-string 1))
          (ruby (match-string-no-properties 2)))
      (save-match-data (if (= ?｜ (string-to-char string)) (setq string (substring string 1))))
      (put-text-property 0 (length string) 'ruby (cons (length string) ruby) string)
      ;;(put-text-property 0 (1- (length string)) 'read-only t string)
      (put-text-property 1 (length string) 'read-only t string)
      (replace-match string)))
  )

(defun aozora-view-buffer-width (start end)
  "display テキストプロパティによる文字幅半減を考慮した、指定領域の文字幅を計算する。"
  (let ((width (string-width (buffer-substring start end)))
        (pos start)
        (half-width-txt ""))
    (while (setq pos (text-property-not-all pos end 'display nil))
      (if (member '(height 0.5) (get-text-property pos 'display))
          (setq half-width-txt (concat half-width-txt (buffer-substring pos (1+ pos)))))
      (setq pos (1+ pos)))
    (- width (/ (string-width half-width-txt) 2))))

(defun aozora-view-arrange-fill-lines ()
  "テキストをfillし、ルビを表示する."
  (goto-char (point-min))
  (let ((adaptive-fill-regexp nil)
        (view-file aozora-view-text-file)
        (view-buffer aozora-view-text-buffer)) ;; save buffer-local-variable
    (while (not (eobp))
      (aozora-arrange-fill-lines nil))
    (setq aozora-view-text-file view-file
          aozora-view-text-buffer view-buffer) ;; restore buffer-local-variable
    )
  (goto-char (point-min))
  ;; Rubyを挿入する。
  (let (start end main main-len
              ruby-start ruby-end ruby-offset ruby ruby-str
              ruby-start-width ruby-end-width glue
              ruby-spc)
    (while (not (eobp))
      ;; 各行の処理
      (setq start    (point-at-bol)
            ruby-end start
            end      (point-at-eol)
            ruby-str "")
      (while (or ;; 現在位置にルビがある場合
              (and (get-text-property ruby-end 'ruby)
                   (setq ruby-start ruby-end))
              ;; 現在位置にルビがない場合
              (and (setq ruby-start (next-single-char-property-change
                                     ruby-end 'ruby nil end))
                   (not (equal ruby-start end)))) ;; 行末ではない
        (message "start=%d, ruby-start=%d" start ruby-start) ;; debug
        (setq ruby       (get-text-property ruby-start 'ruby)
              main-len   (car ruby)
              main       (buffer-substring ruby-start (+ ruby-start main-len))
              ruby       (cdr ruby)
              ruby-spc   (- (* 2.0 (string-width main)) (string-width ruby))
              ruby-end-width   (aozora-view-buffer-width start ruby-end)
              ruby-end         (+ ruby-start main-len) ;; new ruby-end
              ruby-start-width (aozora-view-buffer-width start ruby-start)
              )
        (if (< 0 ruby-spc)
            ;; ルビ文字列が、本文文字列より短い
            (setq ruby-offset 0
                  ruby-spc (/ ruby-spc (length ruby)))
          ;;  ルビ文字列が、本文文字列より長い
          (setq ruby-offset (/ (- ruby-spc) 2)
                ruby-spc 0))
        (setq glue (- (* 2 ruby-start-width) (string-width ruby-str) ruby-offset))
        ;; ruby文字列の作成
        (setq ruby-str
              (concat ruby-str
                      (if (< 0 glue)
                          (concat (make-string (/ (round glue) 2) ?　)
                                  (make-string (% (round glue) 2) ? )))
                      (if (< 0 ruby-spc)
                          (concat (make-string (round (/ ruby-spc 2)) ? )
                                  (mapconcat 'char-to-string
                                             (string-to-list ruby)
                                             (make-string (round ruby-spc) ? )))
                        ruby))))
      (goto-char start)
      (insert ruby-str "\n")
      (put-text-property start (point) 'display '((height 0.5)))
      (forward-line)))
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max)
                            '(ruby read-only left-margin)))
  )

(defun aozora-view-cache-file (file-name)
  "Cache file name for FILE-NAME."
  (concat aozora-view-cache-directory
          file-name
          aozora-view-cache-ext))

(defun aozora-view-load-cache (file-name)
  (let ((cache (aozora-view-cache-file file-name)))
    (when (file-exists-p cache)
      (insert
       (with-temp-buffer
         (let ((coding-system-for-read 'utf-8-emacs))
           (with-auto-compression-mode
             (insert-file-contents cache)))
         (read (current-buffer))))
      t)))

(defun aozora-view-save-cache (file-name)
  "現在のバッファ内容を、FILE-NAME のファイルに対応するキャッシュファイルとして保存する。"
  (let* ((cache-file (aozora-view-cache-file file-name))
         (cache-dir (file-name-directory cache-file)))
    (when (if (equal aozora-view-save-cache 'prompt)
               (y-or-n-p "Do you want to save cache file? ")
             aozora-view-save-cache)
      (with-auto-compression-mode
        (make-directory (file-name-directory cache-dir) t)
        (let ((coding-system-for-write 'utf-8-emacs))
          (write-region
           (prin1-to-string (buffer-string)) nil cache-file)) t))))

(defun aozora-view-bookmark (arg)
  (interactive "P")
  (when (eq major-mode 'aozora-view-mode)
    (setq aozora-view-bookmarks
          (let ((prev-change-point
                 (previous-single-char-property-change
                  (point) 'line-number)))
            (lax-plist-put
             aozora-view-bookmarks (expand-file-name aozora-view-text-file)
             (or (get-text-property prev-change-point 'line-number)
                 (get-text-property (1- prev-change-point) 'line-number)))))
    (message "Bookmarked!")))

(defun aozora-view-restore-bookmark ()
  (interactive)
  (when (eq major-mode 'aozora-view-mode)
    (let* ((line-number
            (lax-plist-get aozora-view-bookmarks
                           (expand-file-name aozora-view-text-file)))
           (pos
            (text-property-any
             (point-min) (point-max)
             'line-number line-number)))
      (if pos (goto-char pos) (goto-char (point-min))))))

(defun aozora-view-suspend ()
  (interactive)
  (if (> (count-windows) 1)
      (delete-window (get-buffer-window (current-buffer)))
    (switch-to-buffer (other-buffer)))
  (bury-buffer (current-buffer)))

(defun aozora-view-traditional ()
  (interactive)
  (if (and (eq major-mode 'aozora-view-mode)
           (require 'ivs-aj1 nil t))
      (let ((inhibit-read-only t))
        (ivs-aj1-trad-region (point-min) (point-max))
        (set-buffer-modified-p nil))
    (message "Not Aozora-View mode!")))

(defun aozora-view-redraw ()
  (interactive)
  (if (eq major-mode 'aozora-view-mode)
      (let ((file aozora-view-text-file)
            (buffer aozora-view-text-buffer))
        (aozora-view-draw aozora-view-text-buffer aozora-view-text-file)
        (set-buffer-modified-p nil)
        (aozora-view-mode)
        (setq aozora-view-text-file file)
        (aozora-view-restore-bookmark))
    (message "Not Aozora-View mode!")))

(defun aozora-view-draw (text-buffer text-file-name)
  "現バッファに、TEXT-BUFFER または TEXT-FILE-NAME にあるテキスト
ファイルを描画する。"
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (if (buffer-live-p text-buffer)
        (insert (with-current-buffer text-buffer
                  (buffer-string)))
      (if (and text-file-name
               (file-exists-p text-file-name))
          (with-coding-priority '(cp932 utf-8)
            (insert-file-contents text-file-name))
        (error "元のバッファまたはテキストファイルが見付かりません！")))
    (goto-char (point-min))
    (while (search-forward "" nil t) (replace-match ""))
    (aozora-view-arrange-replace)
    (aozora-view-arrange-fill-lines)
    (set-buffer-modified-p nil)
    (aozora-view-save-cache text-file-name)))

;;;###autoload
(defun aozora-view ()
  (interactive)
  "現バッファの内容を、青空文庫フォーマットとみなし、別バッファに整形して表示する。"
  (if (and (equal major-mode 'text-mode)
           (string-match "\\.txt" (buffer-file-name)))
      (let ((text-buffer    (current-buffer))
            (text-file-name (buffer-file-name)))
        (when (or (null aozora-view-buffer)
                  (not (buffer-live-p aozora-view-buffer)))
          ;; 青空ビューバッファを作成する。
          (with-current-buffer
              (setq aozora-view-buffer
                    (generate-new-buffer
                     (file-name-sans-extension
                      (file-name-nondirectory text-file-name))))
            (or (aozora-view-load-cache text-file-name)
                (aozora-view-draw text-buffer text-file-name))))
        (switch-to-buffer aozora-view-buffer)
        (aozora-view-mode)
        (setq aozora-view-text-buffer text-buffer
              aozora-view-text-file text-file-name)
        (aozora-view-restore-bookmark))
    (message "Buffer is not `*.txt' text-mode.")))

(defun aozora-arrange-fill-lines (_entry)
  "Fill lines except `read-only' property region."
  (text-mode)
  (let ((fill-column (if (integerp aozora-fill-column)
                         aozora-fill-column
                       (round (* (window-width) aozora-fill-column))))
        start end read-only-start)
    (while (not (eobp))
      (setq start (point)
            end   (point-at-eol)
            read-only-start (text-property-any start end 'read-only t))
      (if read-only-start
          (progn
            (goto-char read-only-start)
            (if (> (current-column) fill-column)
                (save-excursion
                  (fill-region start (point))))
            (goto-char (text-property-not-all read-only-start (point-max) 'read-only t))
            (if (and (> (current-column) fill-column)
                     (not (eolp))) (insert "\n")))
        (goto-char end)
        (if (> (current-column) fill-column)
            (save-excursion
              (fill-region start (point))))
        (forward-line)))))

;; gaiji section
;; This part is taken from `http://mirror.aozora.gr.jp/gaiji_chuki/'.
(defvar aozora-view-gaiji-table
  (eval-when-compile
    (let* ((directory (file-name-directory (or byte-compile-current-file
                                               load-file-name
                                               buffer-file-name)))
           (gaiji-file (expand-file-name "aozora_gaiji_chuki.txt" directory))
           (table (make-hash-table :test 'equal)))
      (unless (file-exists-p gaiji-file)
        (error "Gaiji data file not found!"))
      (with-temp-buffer
        (insert-file-contents gaiji-file)
        (while (re-search-forward
                "^.+?	.+?	.*?	\\(.+?\\)	※［＃\\(.+?\\)[］、]"
                nil t)
          (puthash (match-string 2) (match-string 1) table)))
      table)))

(provide 'aozora-view)

;;; aozora-view.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:
