;Додаткова функція для поділу рядка на масив значень за заданим роздільником.
(defun split-string (string delimiter)
  "Розбиває рядок на масив значень за заданим роздільником DELIMITER."
  (let ((result '())         ; Ініціалізуємо порожній результат.
        (start 0))           ; Ініціалізуємо початковий індекс.
    (loop for pos = (position delimiter string :start start) ; Шукаємо позиції роздільника.
          while pos
          do (push (subseq string start pos) result)     ; Додаємо підрядок до результату.
             (setf start (1+ pos))                        ; Оновлюємо початковий індекс.
          finally (push (subseq string start) result))  ; Додаємо залишок рядка до результату.
    (nreverse result)))                                   ; Повертаємо масив у зворотньому порядку.

;Функція для створення хеш-таблиці з заголовків і значень.
(defun create-record (headers values)
  "Створює хеш-таблицю з заголовків HEADER і відповідних значень VALUES."
  (let ((record (make-hash-table :test 'equal)))   ; Створюємо хеш-таблицю для запису.
    (loop for header in headers                     ; Для кожного заголовка.
          for value in values                       ; Для кожного значення.
          do (setf (gethash header record) value)) ; Призначаємо значення заголовку в хеш-таблиці.
    record))

;Функція для читання CSV-файлу та повернення масиву хеш-таблиць.
(defun read-csv (file-path)
  "Читання CSV-файлу з шляху FILE-PATH та повернення масиву хеш-таблиць."
  (with-open-file (stream file-path :direction :input)  ; Відкриваємо файл для читання.
    (let ((headers (split-string (read-line stream) #\,))) ; Розбиваємо перший рядок на заголовки.
      (loop for line = (read-line stream nil)              ; Для кожного рядка в файлі.
            while line
            collect (create-record headers (split-string line #\,))))))) ; Створюємо хеш-таблицю для запису.

;Функція для вибору записів із хеш-таблиць за заданими фільтрами.
(defun select (records &rest filters)
  "Вибирає записи з RECORDS за заданими фільтрами FILTERS."
  (let* ((filter-keys (loop for (key value) on filters by #'cddr   ; Створюємо асоціативний масив фільтрів.
                            collect (cons key value))))
    (remove-if-not
     (lambda (record)                                    ; Перевіряємо кожен запис.
       (every (lambda (filter)                            ; Для кожного фільтра.
                (let ((key (car filter))                   ; Отримуємо ключ і значення.
                      (value (cdr filter)))
                  (equal (gethash key record) value)))  ; Порівнюємо значення.
              filter-keys))
     records)))                                           ; Повертам записи, що відповідають фільтрам.

;Функція для запису хеш-таблиць у CSV-файл.
(defun write-csv (file-path data)
  "Записує DATA у CSV-файл за шляхом FILE-PATH."
  (with-open-file (stream file-path :direction :output :if-exists :overwrite)  ; Відкриваємо файл для запису.
    (loop for record in data do
          (format stream "~{~A~^,~}~%" record))))  ; Форматуємо запис і записуємо в файл.

;Функція для перетворення хеш-таблиці в асоціативний масив.
(defun hash-table-to-alist (hash)
  "Перетворює HASH-таблицю в асоціативний масив."
  (loop for key being the hash-keys of hash              ; Для кожного ключа в хеш-таблиці.
        collect (cons key (gethash key hash))))         ; Створюємо пари ключ-значення.

;Функція для перетворення асоціативного масиву в хеш-таблицю.
(defun alist-to-hash-table (alist)
  "Перетворює асоціативний масив ALIST у хеш-таблицю."
  (let ((hash (make-hash-table :test 'equal)))         ; Створюємо хеш-таблицю.
    (dolist (pair alist hash)                          ; Для кожної пари ключ-значення в масиві.
      (setf (gethash (car pair) hash) (cdr pair))))   ; Призначаємо значення ключу в хеш-таблиці.

;Функція для створення хеш-таблиці з структури.
(defun struct-to-hash-table (struct)
  "Створює хеш-таблицю з STRUCT."
  (let ((hash (make-hash-table :test 'equal)))        ; Створюємо хеш-таблицю.
    (loop for slot in (mapcar #'car (class-slots (class-of struct))) ; Для кожного слоту структури.
          do (setf (gethash slot hash) (slot-value struct slot))) ; Призначаємо значення слотам.
    hash))

;Функція для виведення запису у вигляді рядка.
(defun print-record (record)
  "Виводить RECORD у вигляді рядка."
  (format t "~{~A~^, ~}" (hash-table-to-alist record)))  ; Конвертуємо хеш-таблицю в асоціативний масив і виводимо.

;Функція для виведення всіх записів.
(defun print-records (records)
  "Виводить ВСІ записи."
  (dolist (record records)                              ; Для кожного запису.
    (print-record record)                               ; Виводимо запис.
    (format t "--------~%")))                           ; Виводимо розділову лінію.

; ----------------------------------------------------------------------------------------------------
; ТЕСТИ
; ----------------------------------------------------------------------------------------------------

;Функція для перевірки рівності двох хеш-таблиць.
(defun hash-table-equal-p (ht1 ht2)
  "Перевіряє рівність двох хеш-таблиць."
  (equalp (hash-table-to-alist ht1) (hash-table-to-alist ht2))) ; Порівнює асоціативні масиви.

;Функція для перевірки успішності тесту.
(defun assert-equal (expected actual test-name)
  "Перевіряє, чи очікуваний результат дорівнює фактичному для тесту TEST-NAME."
  (if (equalp expected actual)                          ; Якщо результати рівні.
      (format t "PASS: ~a~%" test-name)                ; Тест пройдено.
      (format t "FAIL: ~a~%  Очікувано: ~a~%  Отримано: ~a~%" test-name expected actual))) ; Тест не пройдено.

;Тест для читання CSV.
(defun test-read-csv ()
  "Тест для функції read-csv."
  (let* ((test-csv-path "test.csv")                     ; Шлях до тестового CSV-файлу.
         (expected-data                                ; Ожидаємий масив даних.
          (mapcar #'alist-to-hash-table
                  '((("id" . "1") ("name" . "DJI") ("country" . "China"))
                    (("id" . "2") ("name" . "Parrot") ("country" . "France"))
                    (("id" . "3") ("name" . "Skydio") ("country" . "USA"))))))
    (with-open-file (stream test-csv-path :direction :output :if-exists :supersede)
      (format stream "id,name,country~%")
      (format stream "1,DJI,China~%")
      (format stream "2,Parrot,France~%")
      (format stream "3,Skydio,USA~%"))
    (let ((actual-data (read-csv test-csv-path)))      ; Отримуємо фактичні дані.
      (assert-equal
       (mapcar #'hash-table-to-alist expected-data)   ; Порівнюємо очікувані і фактичні дані.
       (mapcar #'hash-table-to-alist actual-data)
       "read-csv test"))))

;Тест для вибору записів.
(defun test-select ()
  "Тест для функції select."
  (let* ((records                                       ; Масив записів.
          (mapcar #'alist-to-hash-table
                  '((("id" . "1") ("name" . "Mavic 3") ("type" . "Quadcopter") ("price" . "1500") ("manufacturer_id" . "1"))
                    (("id" . "2") ("name" . "Anafi") ("type" . "Quadcopter") ("price" . "1200") ("manufacturer_id" . "2"))
                    (("id" . "3") ("name" . "X2") ("type" . "Fixed-Wing") ("price" . "3000") ("manufacturer_id" . "3"))))))
    (let ((expected (subseq records 0 2)))             ; Ожидаємо результати вибору за типом "Quadcopter".
      (assert-equal
       (mapcar #'hash-table-to-alist expected)
       (mapcar #'hash-table-to-alist
               (select records "type" "Quadcopter"))
       "select test - filtering by :type"))
    (let ((expected (list (elt records 2))))            ; Ожидаємо результати вибору за ціною "3000".
      (assert-equal
       (mapcar #'hash-table-to-alist expected)
       (mapcar #'hash-table-to-alist
               (select records "price" "3000"))
       "select test - filtering by :price"))))

;Тест для запису в CSV.
(defun test-write-csv ()
  "Тест для функції write-csv."
  (let* ((test-output-path "output.csv")               ; Шлях до тестового CSV-файлу для виведення.
         (test-records '((:id "1" :name "Mavic 3")     ; Дані для запису.
                         (:id "2" :name "Anafi")))
         (expected-content "id,1,name,Mavic id,2,name,Anafi")) ; Ожидаємий контент файлу.
    (write-csv test-output-path test-records)           ; Записуємо дані.
    (with-open-file (stream test-output-path :direction :input)
      (let ((content (with-output-to-string (out)
                       (loop for line = (read-line stream nil)
                             while line do (write-line line out))))) ; Читаємо і порівнюємо з очікуваним контентом.
        (assert-equal expected-content content "write-to-csv test")))))

;Тест для перетворення хеш-таблиці в асоціативний масив.
(defun test-hash-table-to-alist ()
  "Тест для функції hash-table-to-alist."
  (let* ((hash (make-hash-table :test #'equal))       ; Створюємо хеш-таблицю.
         (alist '((:name . "Mavic 3") (:id . "1"))))  ; Ожидаємий асоціативний масив.
    (setf (gethash :id hash) "1"
          (gethash :name hash) "Mavic 3")
    (assert-equal alist (hash-table-to-alist hash) "hash-table-to-alist test")))

;Тест для виведення запису.
(defun test-print-record ()
  "Тест для функції print-record."
  (let* ((record (make-hash-table :test #'equal))     ; Створюємо хеш-таблицю.
         (records (list (make-hash-table :test #'equal) ; Створюємо кілька хеш-таблиць.
                        (make-hash-table :test #'equal))))
    (setf (gethash :id (first records)) "1"            ; Призначаємо значення в першу хеш-таблицю.
          (gethash :name (first records)) "Mavic 3")
    (setf (gethash :id (second records)) "2"            ; Призначаємо значення у другу хеш-таблицю.
          (gethash :name (second records)) "Anafi")
    (assert-equal
     "ID: 1, Name: Mavic 3\n"                         ; Ожидаємо певний формат виведення.
     (with-output-to-string (out)
       (print-record (first records)))
     "print-record test")))

;Функція для запуску всіх тестів.
(defun run-tests ()
  "Запуск всіх тестів."
  (test-read-csv)
  (test-select)
  (test-write-csv)
  (test-hash-table-to-alist)
  (test-print-record))

(run-tests)  ; Виконуємо тести.
