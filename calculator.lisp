(defpackage :calculator
  (:use :cl :hunchentoot :cl-who)
  (:import-from :parse-number :parse-number)
  (:export :start-server :stop-server))

(in-package :calculator)

(defparameter *server* nil)

(defvar *html-template* "
<!DOCTYPE html>
<html lang='en'>
<head>
  <meta charset='UTF-8'>
  <meta name='viewport' content='width=device-width, initial-scale=1.0'>
  <title>CLISP Calculator</title>
  <style>
    body {
      font-family: 'Arial', sans-serif;
      background-color: #f0f4f8;
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh;
      margin: 0;
    }
    .calculator {
      background-color: #ffffff;
      border-radius: 8px;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      padding: 2rem;
      width: 300px;
    }
    h1 {
      color: #2c3e50;
      text-align: center;
      margin-bottom: 1.5rem;
    }
    form {
      display: flex;
      flex-direction: column;
    }
    input[type='text'], select {
      margin-bottom: 1rem;
      padding: 0.5rem;
      border: 1px solid #bdc3c7;
      border-radius: 4px;
      font-size: 1rem;
    }
    select {
      background-color: #ecf0f1;
    }
    input[type='submit'] {
      background-color: #3498db;
      color: white;
      border: none;
      padding: 0.7rem;
      border-radius: 4px;
      font-size: 1rem;
      cursor: pointer;
      transition: background-color 0.3s ease;
    }
    input[type='submit']:hover {
      background-color: #2980b9;
    }
    #result {
      margin-top: 1.5rem;
      padding: 1rem;
      background-color: #e8f6fe;
      border: 1px solid #3498db;
      border-radius: 4px;
      text-align: center;
      font-size: 1.2rem;
      color: #2c3e50;
    }
  </style>
  <script>
    function calculate(event) {
      event.preventDefault();
      const num1 = document.getElementsByName('num1')[0].value;
      const operation = document.getElementsByName('operation')[0].value;
      const num2 = document.getElementsByName('num2')[0].value;
      const xhr = new XMLHttpRequest();
      xhr.open('POST', '/calculate', true);
      xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
      xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
          document.getElementById('result').innerHTML = 'Result: ' + xhr.responseText;
        }
      };
      xhr.send('num1=' + encodeURIComponent(num1) + '&operation=' + encodeURIComponent(operation) + '&num2=' + encodeURIComponent(num2));
    }
  </script>
</head>
<body>
  <div class='calculator'>
    <h1> CLISP Calculator</h1>
    <form onsubmit='calculate(event)'>
      <input type='text' name='num1' placeholder='Enter first number' required />
      <select name='operation'>
        <option value='add'>Add (+)</option>
        <option value='subtract'>Subtract (-)</option>
        <option value='multiply'>Multiply (×)</option>
        <option value='divide'>Divide (÷)</option>
      </select>
      <input type='text' name='num2' placeholder='Enter second number' required />
      <input type='submit' value='Calculate' />
    </form>
    <div id='result'>Result will appear here</div>
  </div>
</body>
</html>
")
(defun calculate (num1 operation num2)
  (let ((n1 (parse-number num1))
        (n2 (parse-number num2)))
    (case (intern (string-upcase operation) :keyword)
      (:add (+ n1 n2))
      (:subtract (- n1 n2))
      (:multiply (* n1 n2))
      (:divide (if (/= n2 0)
                   (/ n1 n2)
                   "Error: Division by zero"))
      (otherwise "Unknown operation"))))

(defun handle-calculation ()
  (let ((num1 (parameter "num1"))
        (operation (parameter "operation"))
        (num2 (parameter "num2")))
    (format t "Parameters received: num1=~A, operation=~A, num2=~A~%" num1 operation num2)
    (if (and num1 operation num2)
        (format nil "~A" (calculate num1 operation num2))
        "Error: Missing parameters")))

(defun main-page-handler ()
  (setf (content-type*) "text/html")
  *html-template*)

(defun calculate-page-handler ()
  (setf (content-type*) "text/plain")
  (handle-calculation))

(defun start-server ()
  (setf *server* (make-instance 'easy-acceptor :port 8080))
  (define-easy-handler (main-page :uri "/") ()
    (main-page-handler))
  (define-easy-handler (calculate-page :uri "/calculate") ()
    (calculate-page-handler))
  (start *server*))

(defun stop-server ()
  (when (and *server* (hunchentoot:started-p *server*))
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "Server stopped successfully.~%"))
  (format t "No active server to stop.~%"))