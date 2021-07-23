(in-package :asdf)

(defsystem "cl-drawille"
  :description "cl-drawille: Drawing in terminal with Unicode Braille characters."
  :version "1.0.0"
  :author "Goheeca <goheeca@gmail.com>"
  :licence "MIT"
  :components ((:file "drawille"))
  :depends-on ("cffi" "osicat" "alexandria"))

(defsystem "cl-drawille/examples"
  :description "cl-drawille examples"
  :version "1.0.0"
  :author "Goheeca <goheeca@gmail.com>"
  :licence "MIT"
  :components ((:file "examples"))
  :depends-on ("cl-drawille"))

(defsystem "cl-drawille/examples-animations"
  :description "cl-drawille animated examples"
  :version "1.0.0"
  :author "Goheeca <goheeca@gmail.com>"
  :licence "MIT"
  :components ((:file "animations"))
  :depends-on ("cl-drawille" "cl-charms"))
