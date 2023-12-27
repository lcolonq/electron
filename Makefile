.PHONY: run
run:
	emacs -q -l electron.el
electron.so: electron.c
	gcc -Wall -Werror -g -ggdb -fPIC -shared $^ -lraylib -o $@
