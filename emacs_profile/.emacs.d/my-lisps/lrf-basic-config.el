;;外观设置
;;去掉工具栏
;(tool-bar-mode nil)
; make whitespace-mode use just basic coloring
(setq whitespace-style (quote
			( spaces tabs newline space-mark tab-mark newline-mark)))

;;默认使用ibuffer
(defalias 'list-buffers 'ibuffer)

;;备份设置
;;emacs还有一个自动保存功能，默认在~/.emacs.d/auto-save-list里，这个非常有用，我这里没有改动，具体可以参见Sams teach yourself emacs in 24hours(我简称为sams24)
;;启用版本控制，即可以备份多次
(setq version-control t)
;;备份最原始的版本两次，记第一次编辑前的文档，和第二次编辑前的文档
(setq kept-old-versions 2)
;;备份最新的版本五次，理解同上
(setq kept-new-versions 5)
;;删掉不属于以上7中版本的版本
(setq delete-old-versions t)
;;设置备份文件的路径
(setq backup-directory-alist '(("." . "~/.emacs.tmp")))
;;备份设置方法，直接拷贝
(setq backup-by-copying t)

;;时间戳设置(time-stamp)，设定文档上次保存的信息
;;只要里在你得文档里有Time-stamp:的设置，就会自动保存时间戳
;;启用time-stamp
(setq time-stamp-active t)
;;去掉time-stamp的警告？
(setq time-stamp-warn-inactive t)
;;设置time-stamp的格式，我如下的格式所得的一个例子：<Kevin181489 2009-01-03 12:00:00>
(setq time-stamp-format "Kevin181489 %04y-%02m-%02d %02H:%02M:%02S")
;;将修改时间戳添加到保存文件的动作里。
(add-hook 'write-file-hooks 'time-stamp)
;;默认使用Vi-Mode，这样就不会误修改文件了
(setq viper-mode t)
(require 'viper)
;;时间显示设置
;;启用时间显示设置，在minibuffer上面的那个杠上（忘了叫什么来着）
(display-time-mode 1)
;;时间使用24小时制
(setq display-time-24hr-format t)
;;时间显示包括日期和具体时间
(setq display-time-day-and-date t)
;;时间栏旁边启用邮件设置
(setq display-time-use-mail-icon t)
;;时间的变化频率，单位多少来着？
(setq display-time-interval 10)

;;启用minibuffer，好像是默认设置吧
(minibuffer-electric-default-mode 1)
;;启用部分补全功能，如输入M-x q r r相当于M-x query-replace-regexp
;; (partial-completion-mode 1)
;;在minibuffer里启用自动补全函数和变量
(icomplete-mode 1)
;;所有的问题用y/n方式，不用yes/no方式。有点懒，只想输入一个字母
(fset 'yes-or-no-p 'y-or-n-p)
;;允许minibuffer自由变化其大小（指宽度）
(setq resize-mini-windows t)
;;当寻找一个同名的文件，自动关联上那个文件？
(setq uniquify-buffer-name-style 'forward)
;;在emacs读man文档时，使用当前buffer
(setq Man-notify-method 'pushy)
;;鼠标自动避开指针，如当你输入的时候，指针到了鼠标的位置，鼠标有点挡住视线了
(mouse-avoidance-mode 'animate)
;;允许自动打开图片，如wiki里面
(auto-image-file-mode)
;;可以操作压缩文档
(auto-compression-mode 1)
;;在minibuffer上面可以显示列号
(column-number-mode t)
;;显示默认的文档的宽度，看起来比较舒服？
(setq default-fill-column 120)
;;设置tab为4个空格的宽度
(setq default-tab-width 4)
;;指针不要闪，我得眼睛花了
(blink-cursor-mode -1)
(transient-mark-mode 1)
;;当指针到一个括号时，自动显示所匹配的另一个括号
(show-paren-mode 1)
;; 语法高亮
(global-font-lock-mode t)

;是用滚轴鼠标
;;(mouse-wheel-mode t)
;;去掉烦人的警告铃声
(setq visible-bell nil)
;;滚动页面时比较舒服，不要整页的滚动
(setq scroll-step 1
	  scroll-margin 3
	  scroll-conservatively 10000)
;;去掉Emacs和gnus启动时的引导界面
(setq inhibit-startup-message t)
;; (setq gnus-inhibit-startup-message t)
;;当指针移到另一行，不要新增这一行？d
(setq next-line-add-newlines nil)

;;在文档最后自动插入空白一行，好像某些系统配置文件是需要这样的
(setq require-final-newline t)
(setq track-eol t)
;;使用C-k删掉指针到改行末的所有东西
(setq-default kill-whole-line t)
;;设定删除保存记录为200，可以方便以后无限恢复
(setq kill-ring-max 200)
;;增大使用查找函数和变量的寻找范围
(setq apropos-do-all t)
;;是用aspell程序作为Emacs的拼写检查成学
(setq-default ispell-program-name "aspell")
;;使用narrow功能时的一个设置
(put 'narrow-to-region 'disabled nil)
;;启动Emacs自动设置为两个窗口(上下各一个)
;;(split-window-vertically)
;;改变emacs标题栏的标题,%f显示文件全民,%m显示当前的mode
(setq frame-title-format "%f@%m")
;;允许emacs和外部其他程序的粘贴
(setq x-select-enable-clipboard t)
;;在buffer的前面显示文件的行号
(global-linum-mode 1)
;;设置linenum默认的格式
(setq linum-format "%4d ")

;;启动cua模式，可以使用win32默认的cut，copy，paste
(cua-mode 1)
;;不将C-z绑定
(setq cua-remap-control-z nil)
;;标准win32行为，copy后不删除原来的内容
(setq cua-keep-region-after-copy t)
(setq-default line-spacing 2)       ;; Add 1 pixel between lines
;;增加一个最近打开文件的菜单
(recentf-mode)

(defvar mswin (equal window-system 'w32) "Non-nil means windows system.")
;; 滚动条放到右边来
(if mswin
    ;; TODO: windows下如果没有滚动栏的话describe系列命令
    ;; 运行的时候有点问题
    (customize-set-variable 'scroll-bar-mode 'right)
  (customize-set-variable 'scroll-bar-mode nil))

;; Emacs找不到合适的模式时，缺省使用text-mode
(setq default-major-mode 'text-mode)
(setq-default indent-tabs-mode nil)

(require 'pc-select)
(require 'lrf-font)
(require 'lrf-ido-config)

(provide 'lrf-basic-config)
