electron.so: electron.c
	gcc -Wall -Werror -g -ggdb -fPIC -shared $^ -lraylib -o $@
