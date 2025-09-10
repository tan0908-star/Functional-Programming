(print "hello from vscode")
(print "使用 hello.lisp 脚本")

(defun mydouble (x)
	(* x 2))
(defun cs ()
	"清除终端屏幕"
	(sb-ext:run-program "/bin/sh" '("-c" "clear") :input t :output t)
	nil)

(defun loadhellolisp ()
	"为 sbcl 添加 hello.lisp 中的函数"
	(load "hello.lisp"))

(defun good-reverse (lst)
	(labels ((rev (lst acc)
            (if (null lst)
                acc
                (rev (cdr lst) (cons (car lst) acc)))))
		(rev lst nil)))  ; 调用局部函数 rev 启动反转


; Lisp 运行时的内置交互环境
(defun readlist (&rest args)
	(values (read-from-string 
				(concatenate 'string "("
									(apply #'read-line args)
									")" ))))

(defun prompt (&rest args)
	(apply #'format *QUERY-IO* args)
	(finish-output *QUERY-IO*) 
	(read *QUERY-IO*))

; ? 这些代码有些使我疑惑的地方，就是为什么不直接使用函数调用，反而一直要用 apply 呢？ 
(defun break-loop (fn quit &rest args)
	(format *QUERY-IO* "Entering break-loop. ~%")
	(loop
		(let ((in (apply #'prompt args)))
			(if (funcall quit in)
				(return)
				(format *QUERY-IO* "~A~%" (funcall fn in))))))

; 符号和字符串部分
(defun mkstr (&rest args)
	(with-output-to-string (s)
		(dolist (a args) (princ a s))))

; 函数作为返回值的部分

; 宏系统
(defmacro nil! (var)
	(list 'setq var nil))

;; 反引用
(defmacro nif (expr pos zero neg)
	`(case (truncate (signum ,expr))
		(1 ,pos)
		(0 ,zero)
		(-1 ,neg)))

;; 宏展开
(defmacro while (test &body body)
	`(do ()
		((not ,test))
		,@body))


; CD 工程
;; 创建基本数据结构
(defun make-cd (title artist rating ripped)
	"创建一个 cd 的信息"
	(list :title title :artist artist :rating rating :ripped ripped))	;创建属性列表

;; 创建全局变量
(defvar *db* nil)

;; 写入全局变量
(defun add-cd (cd)
	(push cd *db*))

;; 读取全局变量
(defun dump-db ()
	(format t ""))

