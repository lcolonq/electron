electron.so: electron.c
	gcc -Wall -Werror -g -ggdb -fPIC -shared $^ -lraylib -o $@
run: electron.so
	emacs --batch --load=electron.so --load=uwtrbtpimicasy.el
