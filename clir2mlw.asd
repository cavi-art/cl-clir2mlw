(defsystem "clir2mlw"
  :description "CAVI-ART IR to WhyML compiler"
  :version "0.2.4"
  :author "Santiago Saavedra <s.saavedra@fdi.ucm.es>"
  :licence "AGPL3"
  :depends-on (:printv
               :net.didierverna.clon)
  :serial t
  :components ((:file "../vcgen/packages")
               (:file "../vcgen/theories/fetcher")
               (:file "../vcgen/theories/defaultdb/database")
               (:file "formatter")
               (:file "user" :depends-on ("formatter"))))

