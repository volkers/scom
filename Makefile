scom: scom.lisp settings.lisp
	sbcl --load scom.lisp \
		--eval "(sb-ext:save-lisp-and-die #p\"scom\" :toplevel #'scom:main :executable t)"

install: scom
	mv scom ~/bin/scom

clean:
	rm -f scom
