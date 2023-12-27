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

void electron_finalize_texture(void *ptr) {
	if (ptr) {
		UnloadTexture(*(Texture2D *) ptr);
		free(ptr);
	}
}

emacs_value electron_make_texture(emacs_env *env, Texture2D t) {
	Texture *ret = calloc(1, sizeof(Texture));
	*ret = t;
	return env->make_user_ptr(env, electron_finalize_texture, ret);
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

emacs_value electron__make_color(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	int64_t r = env->extract_integer(env, args[0]);
	int64_t g = env->extract_integer(env, args[1]);
	int64_t b = env->extract_integer(env, args[2]);
	int64_t a = env->extract_integer(env, args[3]);
	return electron_make_color(env, (Color){r, g, b, a});
}

emacs_value electron__load_texture(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	char text[256];
	ptrdiff_t len = sizeof(text);
	env->copy_string_contents(env, args[0], text, &len);
	return electron_make_texture(env, LoadTexture(text));
}

emacs_value electron__draw_texture(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	Texture2D t = *(Texture2D *) env->get_user_ptr(env, args[0]);
	int64_t x = env->extract_integer(env, args[1]);
	int64_t y = env->extract_integer(env, args[2]);
	Color color = *(Color *) env->get_user_ptr(env, args[3]);
	DrawTexture(t, x, y, color);
	return env->intern(env, "nil");
}

emacs_value electron__is_key_pressed(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	int64_t k = env->extract_integer(env, args[0]);
	return env->intern(env, IsKeyPressed(k) ? "t" : "nil");
}

emacs_value electron__is_key_down(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	int64_t k = env->extract_integer(env, args[0]);
	return env->intern(env, IsKeyDown(k) ? "t" : "nil");
}

emacs_value electron__is_key_up(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	int64_t k = env->extract_integer(env, args[0]);
	return env->intern(env, IsKeyUp(k) ? "t" : "nil");
}

int emacs_module_init(struct emacs_runtime *runtime) {
	if (runtime->size < (long int) sizeof(*runtime)) return 1;
	emacs_env *env = runtime->get_environment(runtime);
	if (env->size < (long int) sizeof(*env)) return 2;
	electron_defvar(env, "electron/color-white", electron_make_color(env, WHITE));
	electron_defvar(env, "electron/color-black", electron_make_color(env, BLACK));
	electron_defvar(env, "electron/color-blank", electron_make_color(env, BLANK));
	electron_defvar(env, "electron/color-gray", electron_make_color(env, GRAY));
	electron_defvar(env, "electron/color-red", electron_make_color(env, RED));
	electron_defvar(env, "electron/color-green", electron_make_color(env, GREEN));
	electron_defvar(env, "electron/color-blue", electron_make_color(env, BLUE));
	electron_defun(env, "electron//init-window", "initialize window", 3, electron__init_window);
	electron_defun(env, "electron//set-target-fps", "set target framerate", 1, electron__set_target_fps);
	electron_defun(env, "electron//window-should-close", "check if the window should close", 0, electron__window_should_close);
	electron_defun(env, "electron//begin-drawing", "begin drawing", 0, electron__begin_drawing);
	electron_defun(env, "electron//end-drawing", "end drawing", 0, electron__end_drawing);
	electron_defun(env, "electron//clear-background", "clear window background", 1, electron__clear_background);
	electron_defun(env, "electron//draw-text", "draw text to window", 5, electron__draw_text);
	electron_defun(env, "electron//make-color", "build an RGBA color", 4, electron__make_color);
	electron_defun(env, "electron//load-texture", "load a texture :3", 1, electron__load_texture);
	electron_defun(env, "electron//draw-texture", "draw texture", 4, electron__draw_texture);
	electron_defun(env, "electron//is-key-pressed", "check if a key is pressed", 1, electron__is_key_pressed);
	electron_defun(env, "electron//is-key-down", "check if a key is down", 1, electron__is_key_down);
	electron_defun(env, "electron//is-key-up", "check if a key is up", 1, electron__is_key_up);
	return 0;
}
