electron.so: electron.c
	gcc -Wall -Werror -g -ggdb -fPIC -shared -Ideps/raylib/include -Ldeps/raylib/lib $^ -lraylib -o $@
