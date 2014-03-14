#! /usr/local/bin/gosh

(use dbi)
(use gauche.collection)

(define-class <mysql> ()
							((user		:init-keyword	:user
												:init-value		""
												:accessor			user-of)
							 (pass		:init-keyword	:pass
												:init-value		""
												:accessor			pass-of)
							 (host		:init-keyword	:host
												:init-value		""
												:accessor			host-of)
							 (db			:init-keyword	:db
												:init-value		""
												:accessor			db-of)
							 (table		:init-keyword	:table
												:init-value		""
												:accessor			table-of)
							 (column	:init-keyword	:column
												:init-value		()
												:accessor			column-of)))

(define-method select-db ((mysql <mysql>) dbname sql . args)
	(guard (e ((<dbi-error> e) (raise e)))
				 (let-optionals* args ((lst '()))
					 (let* ((conn (dbi-connect (string-append "dbi:mysql:db=" dbname "; host=" (host-of mysql)) :username (user-of mysql) :password (pass-of mysql)))
									(result (apply dbi-execute (dbi-prepare conn sql) lst))
									(getter (relation-accessor result)))
						 (let ((dat (map (lambda (row)
															 (map (lambda (x) (getter row x))
																		(relation-column-names result)))
														 result)))
							 (dbi-close conn)
							 dat)))))

(define-method insert-db ((mysql <mysql>) dbname sql . args)
	(guard (e ((<dbi-error> e) (raise e)))
				 (let-optionals* args ((lst '()))
					 (let* ((conn (dbi-connect (string-append "dbi:mysql:db=" dbname "; host=" (host-of mysql)) :username (user-of mysql) :password (pass-of mysql)))
									(result (apply dbi-execute (dbi-prepare conn sql) lst)))
							 (dbi-close conn)
							 result))))

(define-method select-all ((mysql <mysql>))
	(let* ((table (table-of mysql))
				 (db    (db-of mysql)))
		(select-db mysql db (string-append "select * from " table))))

(define-method select-where ((mysql <mysql>) . args)
	(let-optionals* args ((where ""))
		(let* ((table (table-of mysql))
					 (db    (db-of mysql)))
			(select-db mysql db (string-append "select * from " table " where " where)))))

(define-method select-n ((mysql <mysql>) limit . args)
	(let-optionals* args ((offset 0))
		(let* ((table  (table-of mysql))
					 (db     (db-of mysql)))
			(select-db mysql db (string-append "select * from " table " limit " (x->string offset) ", " (x->string limit))))))

(define-method select-where-n ((mysql <mysql>) limit . args)
	(let-optionals* args ((offset 0)
												(where  ""))
		(let* ((table (table-of mysql))
					 (db    (db-of mysql)))
			(select-db mysql db (string-append "select * from " table " where " where " limit " (x->string offset) ", " (x->string limit))))))

(define-method insert-list ((mysql <mysql>) argv . args)
	(let-optionals* args ((column (column-of mysql)))
		(let* ((table (table-of mysql))
					 (db    (db-of mysql)))
			(insert-db mysql db (string-append "insert into " table " (" (column-gen column) ") values (" (values-gen column) ")") argv))))

(define-method update-list ((mysql <mysql>) where argv . args)
	(let-optionals* args ((column (column-of mysql)))
		(let* ((table (table-of mysql))
					 (db    (db-of mysql)))
			(insert-db mysql db (string-append "update " table " set " (update-query-gen column) " where " where) argv))))


(define (update-query-gen lst)
	(let ((rev (reverse lst)))
		(string-join
			(reverse (cons (string-append (car rev) "=?")
				(map
					(lambda (x) (string-append x "=?, "))
					(cdr rev)))) "")))

(define (values-gen lst)
	(string-join
		(cons "?"
			(map
				(lambda (_) ", ?")
				(cdr lst)))
		""))

(define (column-gen lst)
	(string-join lst ", "))


(define (input-line put)
	(display put)(flush)
	(read-line))

(define (input-values lst)
	(map (lambda (x)
		(let ((input (input-line (string-append x " >> "))))
			(if (eof-object? input)
				(begin (newline)(display "exit")(newline)(exit))
				input))) lst))

(define (option? args-list args n)
	(begin (display args)(newline)(flush))
	(if (null? args-list)
		#f
		(if (string=? (car args-list) (car (drop args n)))
			#t
			(option? (cdr args-list) args n)
			)))

(define (list-n-exists? args n)
	(define (loop argv i)
		(cond ((null? (cdr argv))
					 (= i 0))
					((= i 0)
					 #t)
					(else
					 (loop (cdr argv) (- i 1)))))
	(loop args n))

(define (main args)
	(let ((c (make <mysql>
								:user "username"
								:pass "password"
								:host "hostname"
								:db "dbname"
								:table "tablename"
								:column '("column1" "column2"))))
		(begin (display (select-all c))(newline)(flush))
		(insert-list c '("11" "aaa"))
		(update-list c "column1 = 11" '("123" "abcd"))
		(begin (display (select-all c))(newline)(flush))
		(display c)
		(begin (display (select-all c))(newline)(flush))
		(begin (display (select-n c 5))(newline)(flush))
		(begin (display (select-n c 5 3))(newline)(flush))
		(begin (display (select-where c "column1 > 10"))(newline)(flush))
		(begin (display (select-where-n c 4 3 "column1 > 10"))(newline)(flush))
		))
