(defsystem "clir2mlw-test"
  :description "CLIR2MLW Test Suite"
  :version "0.2.3"
  :author "Santiago Saavedra <s.saavedra@fdi.ucm.es>"
  :licence "Apache 2"
  :depends-on ("clir2mlw"
               :prove
               :printv)
  :defsystem-depends-on (:prove-asdf)
  :components
  ((:test-file "t/formatter-test"))
  :perform (test-op :after (op c) (funcall (intern #.(string :run) :prove) c)))

