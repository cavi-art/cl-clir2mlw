(defsystem "clir2mlw"
    :description "CAVI-ART IR to WhyML compiler"
    :version "0.0.1"
    :author "Santiago Saavedra <s.saavedra@fdi.ucm.es>"
    :licence "AGPL3"
    :depends-on (:printv)
    :components ((:file "../cl-reexport")
                 (:file "../utils" :depends-on ("../cl-reexport"))
                 (:file "formatter")))

