(defsystem "clir2mlw"
  :description "CAVI-ART IR to WhyML compiler"
  :version "0.2.5"
  :author "Santiago Saavedra <s.saavedra@fdi.ucm.es>"
  :licence "AGPL3"
  :depends-on (:printv
               :net.didierverna.clon
               :clir-theories)
  :serial t
  :components ((:file "formatter")
               (:file "user" :depends-on ("formatter"))))

