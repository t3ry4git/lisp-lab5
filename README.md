<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Терентьєв Іван Дмитрович КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
* структури у геш-таблиці
* геш-таблиці у асоціативні списки
* асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.


## Варіант 10(22)
* База даних - Космічні апарати
* Тип записів - Структура
* Таблиці - Компанії, космічні апарати 
* Опис - База даних космічних апаратів для зв'язку, дослідження, тощо

## Лістинг реалізації завдання
```lisp
(defstruct company
  id             ;; Unique identifier for the company
  name           ;; Company name
  typeof         ;; Type of company (e.g., manufacturer, research, etc.)
  launch-year    ;; Year the company was launched
  description    ;; Company description
)

(defvar company-slots '(id name typeof launch-year description))

(defstruct spacecraft
  id             ;; Unique identifier for the spacecraft
  name           ;; Spacecraft name
  typeof         ;; Type of spacecraft (e.g., satellite, capsule, telescope)
  launch-year    ;; Year the spacecraft was launched
  manufacturer   ;; Equipment manufacturer
  description    ;; Spacecraft description
)

(defvar spacecraft-slots '(id name typeof launch-year manufacturer description))

(defun split-string (string separator)
  "Splits a STRING into substrings using SEPARATOR."
  (let ((start 0)
        (result nil))
    (loop for end = (position separator string :start start)
          while end
          do (push (subseq string start end) result)
             (setf start (1+ end)))
    (push (subseq string start) result)
    (nreverse result)))

(defun sanitize-header (header)
  "Replaces spaces in HEADER with underscores, trims unwanted characters, and returns a symbol."
  (intern (string-upcase (substitute #\- #\Space 
                                     (string-trim '(#\Space #\Tab #\Newline #\Return) header)))
          :keyword))

(defun read-csv-as-structures (filepath struct-type)
  "Reads a CSV file and returns a list of structures of type STRUCT-TYPE."
  (with-open-file (stream filepath :direction :input)
    (let* ((headers (mapcar #'sanitize-header
                            (split-string (read-line stream) #\,))) ;; Process headers
           (rows (loop for line = (read-line stream nil)
                       while line
                       collect (split-string line #\,))))       ;; Read rows
      ;; Transform rows into structures
      (mapcar (lambda (row)
                (apply struct-type
                       (mapcan (lambda (header value)
                                 (list header
                                       (string-trim '(#\Space #\Tab #\Newline #\Return #\") value))) ;; Clean values
                               headers row)))
              rows))))

(defun select (filepath key &rest filters)
  "Filters data from FILEPATH and creates structures based on KEY (:company or :spacecraft)."
  (let* ((struct-map '((:company . make-company)
                       (:spacecraft . make-spacecraft))) ;; Map keys to constructors
         (constructor (cdr (assoc key struct-map))))   ;; Get constructor by key
    (unless constructor
      (error "Unknown key: ~A. Expected :company or :spacecraft" key)) ;; Handle unknown key
    ;; Read data and create structures
    (let ((data (read-csv-as-structures filepath constructor)))
      ;; Filter data
      (if filters
          (let* ((filter-pairs (loop for (filter-key value) on filters by #'cddr
                                     collect (cons filter-key value))))
            (remove-if-not
             (lambda (item)
               (every (lambda (filter)
                        (let* ((field (slot-value item (intern (symbol-name (car filter)))))
                               (filter-value (cdr filter)))
                          ;; Convert both values to strings before comparison
                          (string= (write-to-string field)
                                   (write-to-string filter-value))))
                      filter-pairs))
             data))
          data)))) ;; If no filters, return all data

(defun write-structure-to-csv (filepath struct slots)
  "Writes STRUCT to a CSV file at FILEPATH. SLOTS is a list of structure fields."
  (with-open-file (stream filepath :direction :output :if-exists :append :if-does-not-exist :create)
    (let ((values (mapcar (lambda (slot)
                            (let* ((symbol (intern (string-upcase (symbol-name slot))
                                                   (find-package :common-lisp-user))) ;; Define symbol for slot
                                   (value (slot-value struct symbol))) ;; Get slot value
                              ;; Convert value to a string
                              (cond
                               ((null value) "") ;; If value is nil, return empty string
                               ((stringp value) value) ;; If value is a string, leave it as is
                               (t (princ-to-string value))))) ;; Use string representation for other types
                          slots))) ;; Iterate over slots
      (format stream "~{~a~^,~}~%" values)))) ;; Write row to file

(defun struct-to-hash-table (struct slots)
  "Converts STRUCT into a hash table.
SLOTS is a list of structure field names (e.g., (:id :name :typeof))."
  (let ((hash (make-hash-table :test 'equal))) ;; Create an empty hash table
    (dolist (slot slots)
      ;; Convert slot name to a symbol in the structure's package
      (let* ((slot-symbol (intern (string-upcase (symbol-name slot))
                                  (find-package :common-lisp-user)))
             (value (slot-value struct slot-symbol))) ;; Get slot value
        ;; Store value in hash table, key is the slot name in uppercase
        (setf (gethash (string-upcase (symbol-name slot)) hash) value)))
    hash))

(defun hash-table-to-alist (hash-table)
  "Converts a HASH-TABLE to an associative list."
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hash-table)
    (nreverse alist))) ;; Return reversed list to maintain order

(defun alist-to-hash-table (alist)
  "Converts an associative list ALIST into a hash table."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) hash) (cdr pair)))
    hash))

(defun print-table (structures slot-names)
  "Simple table output for a list of STRUCTURES with specified SLOT-NAMES."
  (let ((headers (mapcar (lambda (slot) (string-upcase (symbol-name slot))) slot-names))
        (rows (mapcar (lambda (struct)
                        (mapcar (lambda (slot)
                                  (let ((value (slot-value struct slot)))
                                    (if value (write-to-string value) "")))
                                slot-names))
                      structures)))
    ;; Print headers
    (format t "~{~a~^ | ~}~%" headers)
    ;; Separator
    (format t "~{~a~^---~}~%" (make-list (length headers) :initial-element "-------"))
    ;; Print rows
    (dolist (row rows)
      (format t "~{~a~^ | ~}~%" row))))

```
### Тестові набори та утиліти
```lisp
(defun test-alist-to-hash-table ()
  "Test for alist-to-hash-table function."
  (let* ((alist '(("ID" . 1)
                  ("NAME" . "Hubble Space Telescope")
                  ("TYPEOF" . "Telescope")
                  ("LAUNCH YEAR" . 1990)
                  ("MANUFACTURER" . "NASA")
                  ("DESCRIPTION" . "Large space-based observatory providing high-resolution imaging.")))
         (hash-table (alist-to-hash-table alist)))
    ;; Check values in the hash table
    (format t "Test: Converting alist to hash table:~%")
    (format t "Original alist: ~a~%" alist)
    (format t "Hash table:~%")
    (maphash (lambda (key value)
               (format t "~a => ~a~%" key value))
             hash-table)
    ;; Test data access
    (format t "~%Testing data access:~%")
    (dolist (key '("ID" "NAME" "TYPEOF" "LAUNCH YEAR" "MANUFACTURER" "DESCRIPTION"))
      (format t "~a => ~a~%" key (gethash key hash-table)))))

(defun test-write-structure-to-csv ()
  "Test the write-structure-to-csv function."
  (let* ((test-file "spacecraft.csv") ;; Output file for testing
         (test-data (make-spacecraft :id 5
                                     :name "Voyager 1"
                                     :typeof "Probe"
                                     :launch-year 1977
                                     :manufacturer "NASA"
                                     :description "First spacecraft to enter interstellar space.")))
    ;; Write test data to file
    (write-structure-to-csv test-file test-data '(id name typeof launch-year manufacturer description))
    ;; Read back the file to verify the data
    (with-open-file (stream test-file :direction :input)
      (format t "Contents of ~a:~%" test-file)
      (loop for line = (read-line stream nil)
            while line
            do (format t "~a~%" line)))))

(defun test-space-data ()
  "Function for testing data processing with spacecraft.csv and companies.csv."
  (let ((spacecrafts (select "spacecraft.csv" :spacecraft)) ;; Read spacecrafts
        (companies (select "companies.csv" :company)))     ;; Read companies
    ;; Test 1: Print all spacecrafts
    (format t "All spacecrafts:~%")
    (print-table spacecrafts spacecraft-slots)
    
    ;; Test 2: Filter spacecrafts by manufacturer
    (format t "~%Spacecrafts by NASA:~%")
    (print-table (select "spacecraft.csv" :spacecraft :manufacturer "NASA") spacecraft-slots)
      
    
    ;; Test 3: Print all companies
    (format t "~%All companies:~%")
    (print-table companies company-slots)
    
    ;; Test 4: Filter companies by type
    (format t "~%Manufacturers:~%")
    (print-table (select "companies.csv" :company :typeof "Manufacturer") company-slots)
      
    ;; Test 5: Convert structure to hash table and back
    (let ((craft (first spacecrafts)))
      (format t "~%Test: Converting structure to hash table and to alist:~%")
      (let ((hash (struct-to-hash-table craft '(id name typeof launch-year manufacturer description))))
        (format t "Hash table: ~a~%" (hash-table-to-alist hash))
        (test-alist-to-hash-table)
        (print nil)))))
```
### Вміст тестових файлів №1 (companies.csv)
```
id,name,typeof,launch year,description
1,SpaceX,Manufacturer,2002,American aerospace manufacturer and space transportation company
2,NASA,Research,1958,United States federal agency responsible for space exploration and research
3,Blue Origin,Manufacturer,2000,Private aerospace manufacturer and spaceflight services company
```
### Вміст тестових файлів №2 (spacecraft.csv)
```
id,name,typeof,launch year,manufacturer,description
1,Hubble Space Telescope,Telescope,1990,NASA,Large space-based observatory providing high-resolution imaging
2,Dragon,Capsule,2010,SpaceX,Reusable spacecraft for cargo and crew transportation
3,New Shepard,Suborbital Rocket,2015,Blue Origin,Suborbital space tourism and research spacecraft
```
### Тестування
```lisp
cl-user > (test-space-data)
All spacecrafts:
ID | NAME | TYPEOF | LAUNCH-YEAR | MANUFACTURER | DESCRIPTION
---------------------------------------------------------
"1" | "Hubble Space Telescope" | "Telescope" | "1990" | "NASA" | "Large space-based observatory providing high-resolution imaging"
"2" | "Dragon" | "Capsule" | "2010" | "SpaceX" | "Reusable spacecraft for cargo and crew transportation"
"3" | "New Shepard" | "Suborbital Rocket" | "2015" | "Blue Origin" | "Suborbital space tourism and research spacecraft"
Spacecrafts by NASA:
ID | NAME | TYPEOF | LAUNCH-YEAR | MANUFACTURER | DESCRIPTION
---------------------------------------------------------
"1" | "Hubble Space Telescope" | "Telescope" | "1990" | "NASA" | "Large space-based observatory providing high-resolution imaging"
All companies:
ID | NAME | TYPEOF | LAUNCH-YEAR | DESCRIPTION
-----------------------------------------------
"1" | "SpaceX" | "Manufacturer" | "2002" | "American aerospace manufacturer and space transportation company"
"2" | "NASA" | "Research" | "1958" | "United States federal agency responsible for space exploration and research"
"3" | "Blue Origin" | "Manufacturer" | "2000" | "Private aerospace manufacturer and spaceflight services company"
Manufacturers:
ID | NAME | TYPEOF | LAUNCH-YEAR | DESCRIPTION
-----------------------------------------------
"1" | "SpaceX" | "Manufacturer" | "2002" | "American aerospace manufacturer and space transportation company"
"3" | "Blue Origin" | "Manufacturer" | "2000" | "Private aerospace manufacturer and spaceflight services company"
Test: Converting structure to hash table and to alist:
Hash table: ((ID . 1) (NAME . Hubble Space Telescope) (TYPEOF . Telescope)
 (LAUNCH-YEAR . 1990) (MANUFACTURER . NASA)
 (DESCRIPTION
  . Large space-based observatory providing high-resolution imaging))
Test: Converting alist to hash table:
Original alist: ((ID . 1) (NAME . Hubble Space Telescope) (TYPEOF . Telescope)
 (LAUNCH YEAR . 1990) (MANUFACTURER . NASA)
 (DESCRIPTION
  . Large space-based observatory providing high-resolution imaging.))
Hash table:
ID => 1
NAME => Hubble Space Telescope
TYPEOF => Telescope
LAUNCH YEAR => 1990
MANUFACTURER => NASA
DESCRIPTION => Large space-based observatory providing high-resolution imaging.
Testing data access:
ID => 1
NAME => Hubble Space Telescope
TYPEOF => Telescope
LAUNCH YEAR => 1990
MANUFACTURER => NASA
DESCRIPTION => Large space-based observatory providing high-resolution imaging.
NIL 
NIL


cl-user > (test-write-structure-to-csv)
Contents of spacecraft.csv:
id,name,typeof,launch year,manufacturer,description
1,Hubble Space Telescope,Telescope,1990,NASA,Large space-based observatory providing high-resolution imaging
2,Dragon,Capsule,2010,SpaceX,Reusable spacecraft for cargo and crew transportation
3,New Shepard,Suborbital Rocket,2015,Blue Origin,Suborbital space tourism and research spacecraft
5,Voyager 1,Probe,1977,NASA,First spacecraft to enter interstellar space.
NIL
```
