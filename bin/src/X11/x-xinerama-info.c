#include <xcb/xcb.h>
#include <xcb/xinerama.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char **argv) {
	int just_first = 0;
	int print_offset = 1;
	int i;

	for (i = 1; i < argc; i++) {
		int hyphens = 0;
		for (; *argv[i] == '-'; (argv[i])++) hyphens++;

		int error = 0;
		if (!hyphens) {
			error = 1;
		} else if (!strcmp(argv[i], "first")) {
			just_first = 1;
		} else if (!strcmp(argv[i], "all")) {
			just_first = 0;
		} else if (!strcmp(argv[i], "dim")) {
			print_offset = 0;
		} else {
			error = 1;
		}

		if (error) {
			fprintf(stdout, "Usage: %s [ --first | --all ] [ --dim ]\n", argv[0]);
			return 1;
		}
	}

	int screen_default;
	xcb_connection_t *conn = xcb_connect(NULL, &screen_default);
	if (xcb_connection_has_error(conn)) {
		fprintf(stderr,"NOK\n");
		return 1;
	}

	xcb_generic_error_t *error;

	xcb_xinerama_is_active_cookie_t is_active_req = xcb_xinerama_is_active(conn);
	xcb_xinerama_is_active_reply_t *is_active = xcb_xinerama_is_active_reply(conn, is_active_req, &error);
	if (error) {
		fprintf(stderr, "Couldn't query Xinerama\n");
		return 1;
	}
	if (!is_active->state) {
		fprintf(stderr, "Xinerama inactive\n");
		return 1;
	}
	xcb_xinerama_query_screens_cookie_t cookie_screen;
	cookie_screen = xcb_xinerama_query_screens(conn);
	xcb_xinerama_query_screens_reply_t *query_screens;
	query_screens = xcb_xinerama_query_screens_reply(conn, cookie_screen, &error);
	if (error) {
		fprintf(stderr, "Error getting screen info\n");
		return 1;
	}
	/* iterator
	xcb_xinerama_screen_info_iterator_t screens;
	screens = xcb_xinerama_query_screens_screen_info_iterator(query_screens);
	for (; screens.rem; xcb_xinerama_screen_info_next(&screens)) {
		xcb_xinerama_screen_info_t info = *screens.data;
		fprintf(stdout, "%ux%u@%d,%d\n", info.width, info.height, info.x_org, info.y_org);
	}
	*/
	int len;
	xcb_xinerama_screen_info_t *screens = xcb_xinerama_query_screens_screen_info(query_screens);
	len = xcb_xinerama_query_screens_screen_info_length(query_screens);
	for (i = 0; i < len; i++) {
		xcb_xinerama_screen_info_t info = screens[i];
		fprintf(stdout, "%ux%u", info.width, info.height);
		if (print_offset) fprintf(stdout, "+%d+%d", info.x_org, info.y_org);
		fprintf(stdout, "\n");
		if (just_first) break;
	}
	xcb_disconnect(conn);
	return 0;
}
