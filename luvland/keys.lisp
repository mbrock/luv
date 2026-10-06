;;;; Physical keys, from luv's key names to the Linux evdev codes that
;;;; wl_keyboard.key carries.  Clients interpret them through the XKB keymap
;;;; the server sends, so a key is named by its place, not its character.

(in-package #:luvland)

(defparameter +evdev-keys+
  (let ((table (make-hash-table)))
    (loop for (name code)
            on '(:escape 1
                 :1 2 :2 3 :3 4 :4 5 :5 6 :6 7 :7 8 :8 9 :9 10 :0 11
                 :- 12 := 13 :backspace 14 :tab 15
                 :q 16 :w 17 :e 18 :r 19 :t 20 :y 21 :u 22 :i 23 :o 24 :p 25
                 :[ 26 :] 27 :return 28 :control-left 29
                 :a 30 :s 31 :d 32 :f 33 :g 34 :h 35 :j 36 :k 37 :l 38
                 :|;| 39 :|'| 40 :|`| 41 :shift-left 42 :|\\| 43
                 :z 44 :x 45 :c 46 :v 47 :b 48 :n 49 :m 50
                 :|,| 51 :|.| 52 :/ 53 :shift-right 54
                 :alt-left 56 :space 57 :caps-lock 58
                 :f1 59 :f2 60 :f3 61 :f4 62 :f5 63 :f6 64 :f7 65 :f8 66
                 :f9 67 :f10 68 :num-lock 69 :scroll-lock 70
                 :f11 87 :f12 88
                 :control-right 97 :print-screen 99 :alt-right 100
                 :home 102 :up 103 :page-up 104 :left 105 :right 106
                 :end 107 :down 108 :page-down 109 :insert 110 :delete 111
                 :super-left 125 :super-right 126 :menu 127)
          by #'cddr
          do (setf (gethash name table) code))
    table)
  "evdev codes by luv key name; see linux/input-event-codes.h.")

(defun key-evdev-code (key-name)
  (gethash key-name +evdev-keys+))

(defun button-evdev-code (button)
  (case button
    (:left #x110) (:right #x111) (:middle #x112) (:x1 #x113) (:x2 #x114)))
