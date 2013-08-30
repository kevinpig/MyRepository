;; -*- Emacs-Lisp -*-

;; Time-stamp: <10/19/2009 18:11:04 星期一 by ahei>

(require 'cn-weather)

;; 设置我所在的城市
(setq cn-weather-city "上海")
(defalias 'weather 'cn-weather-today)
(defalias 'weather-tomorrow 'cn-weather-forecast)
