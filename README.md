#README for Calculator Web Application
  #Overview
This project is a simple web-based calculator built using Common Lisp. The application uses the Hunchentoot web server to handle HTTP requests, and CL-WHO to generate HTML content. The calculator performs basic arithmetic operations (addition, subtraction, multiplication, and division) and communicates with the server asynchronously using JavaScript.
#Package Definition
(defpackage :calculator
  (:use :cl :hunchentoot :cl-who)
  (:import-from :parse-number :parse-number)
  (:export :start-server :stop-server))

(in-package :calculator)
#Explanation
defpackage :calculator: Defines a new package named :calculator.
(:use :cl :hunchentoot :cl-who): Uses the Common Lisp (:cl), Hunchentoot (:hunchentoot), and CL-WHO (:cl-who) packages.
cl provides all standard Common Lisp functions and macros.
hunchentoot is a web server written in Common Lisp, used to create the web application.
cl-who is a DSL (Domain Specific Language) for generating HTML in a Lisp-like syntax.
(:import-from :parse-number :parse-number): Imports the parse-number function from the :parse-number package to convert strings to numbers, useful for processing user input in the calculator.
in-package :calculator: Switches the current package to :calculator. All subsequent definitions and expressions in the file will be within this package.
#Global Variables
(defparameter *server* nil)

*server*: A global variable that will hold the Hunchentoot server instance. Initially set to nil because the server hasn't been started yet.
#HTML Template
(defvar *html-template* "...HTML content...")
*html-template*: Contains the HTML, CSS, and JavaScript for the calculator's web interface. This approach of storing the entire web page as a string in a Lisp variable is a simple way to serve static content. When a user requests the calculator page, the server sends this string as the HTTP response.
#Main Page Handler
(defun main-page-handler ()
  (setf (content-type*) "text/html")
  *html-template*)

This function sets the content type to HTML and then returns the contents of *html-template*, which will be sent to the user's web browser.


