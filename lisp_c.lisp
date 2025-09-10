;; 完整的C风格语法子集实现（在Lisp中）
;; 特性：花括号代码块、分号分隔语句、int/char类型、=赋值、if/while、printf、main入口
(defpackage :c-subset
  (:use :cl :split-sequence)
  (:export :enable-c-syntax))

(in-package :c-subset)

;; ---------------------------
;; 1. 读取宏：语法扩展核心
;; ---------------------------

(defvar *c-syntax-enabled* nil)  ; 语法启用标记

(defun enable-c-syntax ()
    "启用完整C风格语法支持"
    (unless *c-syntax-enabled*
        ;; 注册花括号读取器：{ ... } → 代码块
        (set-macro-character #\{ #'read-c-block)
        (set-macro-character #\} (lambda (s c) (error "Unmatched } in C syntax")))
        ;; 注册分号读取器：; 作为语句分隔符
        (set-macro-character #\; #'read-semicolon)
        ;; 注册字符常量读取器：#'c' → ASCII码（模拟C的'c'）
        (set-dispatch-macro-character #\# #\' #'read-c-char)
        (setf *c-syntax-enabled* t)
        (format t "C-style syntax enabled~%"))
    t)

(defun read-semicolon (stream char)
    "处理分号：作为语句分隔符，不产生实际值"
    (declare (ignore char))
    (values))  ; 分号仅用于分隔语句

(defun read-c-char (stream subchar arg)
    "处理字符常量：#'A' → 65（ASCII码）"
    (declare (ignore subchar arg))
    (let ((char (read-char stream)))  ; 读取单引号后的字符
        (read-char stream)  ; 跳过 closing '
        (char-code char)))  ; 返回ASCII码（C中char本质是整数）

(defun read-c-block (stream char)
    "处理花括号块：{ stmt1; stmt2; ... } → (progn (stmt1) (stmt2) ...)"
    (declare (ignore char))
    (let ((tokens (read-delimited-list #\} stream t))  ; 读取花括号内所有内容
          (current-stmt nil)
          (statements nil))
        ;; 按分号分割语句
        (dolist (token tokens)
            (if (eq token '|;|)  ; 遇到分号，结束当前语句
                (progn
                    (when current-stmt
                        (push (reverse current-stmt) statements)
                        (setf current-stmt nil)))
                (push token current-stmt)))  ; 收集语句 tokens
        ;; 处理最后一个没有分号的语句
        (when current-stmt
            (push (reverse current-stmt) statements))
        ;; 转换为Lisp可执行代码
        `(progn ,@(mapcar #'parse-statement (reverse statements)))))

;; ---------------------------
;; 2. 语句解析：表达式与赋值
;; ---------------------------

(defun parse-statement (tokens)
    "解析单条语句（处理赋值和普通表达式）"
    (cond
        ;; 处理变量声明：int a; 或 char c = #'A';
        ((member (first tokens) '(int char))
            (let* ((type (first tokens))
                   (var (second tokens))
                   (init (if (member '= tokens)
                             (infix->prefix (subseq tokens (+ 2 (position '= tokens))))
                             0)))
                (case type
                    (int `(defparameter ,var (make-c-int :value ,init)))
                    (char `(defparameter ,var (make-c-char :value ,init))))))
        ;; 处理普通语句（赋值或表达式）
        (t (infix->prefix tokens))))

(defun infix->prefix (expr)
    "中缀表达式转前缀表达式（支持运算符优先级）
     优先级：() > * / > + - > > < == != > ="
    (cond
        ((not (listp expr)) expr)  ; 原子值直接返回
        ((null expr) nil)
        ((null (cdr expr)) (car expr))  ; 单元素列表
        (t
            ;; 处理赋值（最低优先级）
            (let ((assign-pos (position '= expr)))
                (if assign-pos
                    (list 'setf
                          (infix->prefix (subseq expr 0 assign-pos))
                          (infix->prefix (subseq expr (1+ assign-pos))))
                    ;; 处理比较运算符（== != > < >= <=）
                    (let ((cmp-pos (position-if (lambda (x) (member x '(== != > < >= <=))) 
                                               expr :from-end t)))
                        (if cmp-pos
                            (list (nth cmp-pos expr)
                                  (infix->prefix (subseq expr 0 cmp-pos))
                                  (infix->prefix (subseq expr (1+ cmp-pos))))
                            ;; 处理加减（+ -）
                            (let ((add-pos (position-if (lambda (x) (member x '(+ -)))
                                                       expr :from-end t)))
                                (if add-pos
                                    (list (nth add-pos expr)
                                          (infix->prefix (subseq expr 0 add-pos))
                                          (infix->prefix (subseq expr (1+ add-pos))))
                                    ;; 处理乘除（* /）
                                    (let ((mul-pos (position-if (lambda (x) (member x '(* /)))
                                                               expr :from-end t)))
                                        (if mul-pos
                                            (list (nth mul-pos expr)
                                                  (infix->prefix (subseq expr 0 mul-pos))
                                                  (infix->prefix (subseq expr (1+ mul-pos))))
                                            expr))))))))))  ; 无法解析的表达式

;; ---------------------------
;; 3. 类型系统：int 和 char
;; ---------------------------

;; 类型结构体（模拟C的类型区分）
(defstruct (c-int (:type vector)) value)    ; 整型包装
(defstruct (c-char (:type vector)) value)   ; 字符型包装（存储ASCII码）

;; 类型安全的运算函数（自动处理int/char混合运算）
(defun c-op (op a b)
    "通用运算处理函数（支持int和char）"
    (let ((v1 (if (c-int-p a) (c-int-value a) (c-char-value a)))
          (v2 (if (c-int-p b) (c-int-value b) (c-char-value b))))
        (make-c-int :value (case op
                             (+ (+ v1 v2))
                             (- (- v1 v2))
                             (* (* v1 v2))
                             (/ (truncate v1 v2))  ; C风格整数除法
                             (> (if (> v1 v2) 1 0))
                             (< (if (< v1 v2) 1 0))
                             (== (if (= v1 v2) 1 0))
                             (!= (if (/= v1 v2) 1 0))
                             (>= (if (>= v1 v2) 1 0))
                             (<= (if (<= v1 v2) 1 0))
                             (t (error "Unknown operator: ~a" op))))))

;; 为运算符定义通用处理（通过宏自动生成函数）
(macrolet ((define-c-ops (&rest ops)
             `(progn
                ,@(loop for op in ops
                        collect `(defun ,op (a b) (c-op ',op a b))))))
    (define-c-ops + - * / > < == != >= <=))

;; ---------------------------
;; 4. 控制流：if 和 while
;; ---------------------------

(defmacro if (condition &rest body)
    "C风格if语句：if (cond) { ... } [else { ... }]
     注意：C中0为假，非0为真"
    (let* ((parsed-cond (infix->prefix condition))
           (then-block (first body))
           (else-block (when (second body) (second body))))
        `(if (> (c-int-value ,parsed-cond) 0)  ; 转换为Lisp的布尔判断
             ,then-block
             ,else-block)))

(defmacro while (condition &rest body)
    "C风格while语句：while (cond) { ... }"
    (let ((parsed-cond (infix->prefix condition)))
        `(loop while (> (c-int-value ,parsed-cond) 0)
               do ,@body)))

;; ---------------------------
;; 5. 输出函数：printf
;; ---------------------------

(defun printf (fmt &rest args)
    "C风格printf：支持%d（整数）和%c（字符）格式符"
    (let ((parts (split-sequence #\% fmt))  ; 分割格式字符串
          (arg-idx 0))
        (dolist (part parts)
            (unless (string= part "")
                (if (zerop arg-idx)
                    (princ part)  ; 格式字符串前缀
                    (let ((type-char (char part 0))  ; 格式符类型（d/c）
                          (rest-str (subseq part 1)))  ; 格式符后的字符串
                        (case type-char
                            (#\d  ; 输出整数
                             (princ (c-int-value (nth (1- arg-idx) args))))
                            (#\c  ; 输出字符（ASCII码转字符）
                             (princ (code-char (c-char-value (nth (1- arg-idx) args)))))
                        (princ rest-str))))  ; 输出格式符后的内容
            (incf arg-idx))))  ; 移动到下一个参数

;; ---------------------------
;; 6. 程序入口：main函数
;; ---------------------------

(defmacro main ()
    "C风格main函数：程序入口点，自动执行"
    `(progn
        (defun main ()
            ,(read-c-block *standard-input* #\{))  ; 读取main函数体
        (main)))  ; 定义后立即执行


;; ---------------------------
;; 使用示例（完全C风格代码）
;; ---------------------------
;; (enable-c-syntax)  ; 启用C风格语法
;; 
;; // 变量声明
;; int a;
;; int b;
;; char c;
;; 
;; // 主函数（程序入口）
;; main() {
;;     a = 10;
;;     b = 20;
;;     c = #'A';  // 字符常量（对应C的'A'）
;;     
;;     printf("a = %d, b = %d, c = %c\n", a, b, c);
;;     
;;     // if语句
;;     if (a + b > 25) {
;;         printf("a + b > 25\n");
;;     } else {
;;         printf("a + b <= 25\n");
;;     }
;;     
;;     // while循环
;;     int i = 0;
;;     while (i < 3) {
;;         c = c + 1;  // 字符自增（ASCII码加1）
;;         printf("i = %d, c = %c\n", i, c);
;;         i = i + 1;
;;     }
;; }
;; 
;; /* 预期输出：
;; C-style syntax enabled
;; a = 10, b = 20, c = A
;; a + b > 25
;; i = 0, c = B
;; i = 1, c = C
;; i = 2, c = D
;; */
