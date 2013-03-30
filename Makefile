conf=~/.config/common-lisp/source-registry.conf.d/home-dir.conf

default: init

.PHONY: init

init:
	@echo '(:tree (:home "lisp"))' >${conf}
	@echo "###DONE###"
