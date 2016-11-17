LISP ?= sbcl

clir2mlw: *.lisp quicklisp
	ros run -L $(LISP) -l binary.lisp


quicklisp: qlfile qlfile.lock
	./autogen-quicklisp.ros
	touch quicklisp


.PHONY: update-quicklisp
update-quicklisp: qlfile qlfile.lock
	./autogen-quicklisp.ros --update
