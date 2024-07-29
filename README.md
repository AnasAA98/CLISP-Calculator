# CLISP Calculator

## Introduction

This project implements a simple web-based calculator using Common Lisp and the Hunchentoot web server.

## Package Definition

```lisp
(defpackage :calculator
  (:use :cl :hunchentoot :cl-who)
  (:import-from :parse-number :parse-number)
  (:export :start-server :stop-server))

(in-package :calculator)
```
## Explanation
## ```(:use :cl :hunchentoot :cl-who)```
+ ```cl:``` The Common Lisp package, which provides all standard Common Lisp functions and macros.  

+ ```:hunchentoot:``` A web server written in Common Lisp, used to create the web application.  

+ ```:cl-who:``` A DSL (Domain Specific Language) for generating HTML in a Lisp-like syntax.  

+ ```(:import-from :parse-number :parse-number):``` Imports the parse-number function from the parse-number package. This function is used to convert strings to numbers, useful for processing user input in the calculator.  

+ ```(in-package :calculator):``` Switches the current package to : calculator. All subsequent definitions and expressions in the file will be within this package.


## Server Management
### Global Variables
```(defparameter *server* nil)```
+ ```server```: A global variable that will hold the Hunchentoot server instance. Initially set to nil because the server hasn't been started yet. When the server is started (in the start-server function), this variable is assigned the actual server instance.
### HTML Template
```(defvar *html-template* "...")```  

+ The ```html-template``` declaration defines a global variable that contains the HTML, CSS, and JavaScript for the calculator's web interface. This approach stores the entire web page as a string in a Lisp variable, serving static content. When a user requests the calculator page, the Lisp web server sends this string as the HTTP response, eliminating the need for external files.  

## Main Page Handler

``` LISP
(defun main-page-handler () 
(setf (content-type*) "text/html")
*html-template*)
```

+ This function sets the content type to HTML and then returns the contents of *html-template*, which will be sent to the user's web browser.

## JavaScript Code

``` JS
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
```
  ### Explanation
  
+ ```function calculate(event):``` Defines a function named calculate that takes an event parameter. This function is called when the form is submitted.
  
+ ```event.preventDefault();:``` Prevents the default form submission behavior, which would cause the page to reload.

+ ```const num1 = document.getElementsByName('num1')[0].value;:``` Gets the value of the first input element named num1.

+ ```const operation = document.getElementsByName('operation')[0].value;:``` Gets the selected operation from the dropdown menu.

+ ```const num2 = document.getElementsByName('num2')[0].value;:``` Gets the value of the first input element named num2.

+ ```const xhr = new XMLHttpRequest();:``` Creates a new XMLHttpRequest object for making an AJAX request.

+ ```xhr.open('POST', '/calculate', true);:``` Initializes a POST request to the /calculate endpoint. The true parameter makes it asynchronous.

+ ```xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');:``` Sets the content type of the request to form data.

+ ```xhr.onreadystatechange = function () { ... };:``` Sets up a callback function that will be called whenever the request's state changes.

+ ```if (xhr.readyState === 4 && xhr.status === 200) { ... }:``` Checks if the request is complete (readyState 4) and successful (status 200).

+ ```document.getElementById('result').innerHTML = 'Result: ' + xhr.responseText;:``` Updates the result element with the server's response.

+ ```xhr.send('num1=' + encodeURIComponent(num1) + '&operation=' + encodeURIComponent(operation) + '&num2=' + encodeURIComponent(num2));:``` Sends the request with the form ```data. encodeURIComponent()``` is used to properly encode the values.

## Functions
  ### Handle Calculation
  +  ```handle-calculation:``` Retrieves parameters from the HTTP request, logs the received parameters, calls calculate if all parameters are present, otherwise returns an error message.

### Start Server

``` LISP
(defun start-server ()
  (setf *server* (make-instance 'easy-acceptor :port 8080))
  (setf (handler-bind '("/" :function main-page-handler))
        (handler-bind '("/calculate" :function calculate-handler)))
  (start *server*))
```
+ ```start-server:```
  - Creates a new Hunchentoot server instance on port 8080.
    
  - Defines two easy handlers for the main page and calculation endpoint.

  - Starts the server.

### Stop Server

``` LISP
(defun stop-server ()
  (when (and *server* (acceptor-running-p *server*))
    (stop *server*)
    (setf *server* nil)
    (format t "Server stopped successfully."))
  (format t "No active server to stop."))
```
+ ```stop-server:```
  - Checks if the server exists and is started.
    
  - If so, stops the server, sets *server* to nil, and prints a success message.

  - If not, prints a message that there's no active server to stop.
  
