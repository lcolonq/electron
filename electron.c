#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>

#include "emacs-module.h"
#include <raylib.h>

int plugin_is_GPL_compatible;

void electron_defvar(emacs_env *env, const char *nm, emacs_value x) {
	emacs_value symbol = env->intern(env, nm);
	emacs_value args[] = {symbol, x};
	env->funcall(env, env->intern(env, "set"), 2, args);
}

void electron_defun(emacs_env *env, const char *nm, const char *docs, ptrdiff_t arity, emacs_value (*func)(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *)) {
	emacs_value func_init_window = env->make_function(env, arity, arity, func, docs, NULL);
	emacs_value symbol = env->intern(env, nm);
	emacs_value args[] = {symbol, func_init_window};
	env->funcall(env, env->intern(env, "defalias"), 2, args);
}

void electron_finalize_color(void *ptr) {
	if (ptr) free(ptr);
}

emacs_value electron_make_color(emacs_env *env, Color c) {
	Color *ret = calloc(1, sizeof(Color));
	*ret = c;
	return env->make_user_ptr(env, electron_finalize_color, ret);
}

emacs_value electron__init_window(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	int64_t w = env->extract_integer(env, args[0]);
	int64_t h = env->extract_integer(env, args[1]);
	char title[256];
	ptrdiff_t len = sizeof(title);
	env->copy_string_contents(env, args[2], title, &len);
	InitWindow(w, h, title);
	return env->intern(env, "nil");
}

emacs_value electron__set_target_fps(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	int64_t fps = env->extract_integer(env, args[0]);
	SetTargetFPS(fps);
	return env->intern(env, "nil");
}

emacs_value electron__window_should_close(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	return env->intern(env, WindowShouldClose() ? "t" : "nil");
}

emacs_value electron__begin_drawing(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	BeginDrawing();
	return env->intern(env, "nil");
}

emacs_value electron__end_drawing(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	EndDrawing();
	return env->intern(env, "nil");
}

emacs_value electron__clear_background(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	Color color = *(Color *) env->get_user_ptr(env, args[0]);
	ClearBackground(color);
	return env->intern(env, "nil");
}

emacs_value electron__draw_text(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	char text[256];
	ptrdiff_t len = sizeof(text);
	env->copy_string_contents(env, args[0], text, &len);
	int64_t x = env->extract_integer(env, args[1]);
	int64_t y = env->extract_integer(env, args[2]);
	int64_t sz = env->extract_integer(env, args[3]);
	Color color = *(Color *) env->get_user_ptr(env, args[4]);
	DrawText(text, x, y, sz, color);
	return env->intern(env, "nil");
}

int emacs_module_init(struct emacs_runtime *runtime) {
	if (runtime->size < (long int) sizeof(*runtime)) return 1;
	emacs_env *env = runtime->get_environment(runtime);
	if (env->size < (long int) sizeof(*env)) return 2;
	electron_defvar(env, "electron/color-gray", electron_make_color(env, GRAY));
	electron_defvar(env, "electron/color-green", electron_make_color(env, GREEN));
	electron_defun(env, "electron//init-window", "initialize window", 3, electron__init_window);
	electron_defun(env, "electron//set-target-fps", "set target framerate", 1, electron__set_target_fps);
	electron_defun(env, "electron//window-should-close", "check if the window should close", 0, electron__window_should_close);
	electron_defun(env, "electron//begin-drawing", "begin drawing", 0, electron__begin_drawing);
	electron_defun(env, "electron//end-drawing", "end drawing", 0, electron__end_drawing);
	electron_defun(env, "electron//clear-background", "clear window background", 1, electron__clear_background);
	electron_defun(env, "electron//draw-text", "draw text to window", 5, electron__draw_text);
	return 0;
}
