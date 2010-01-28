#include <xcb/xcb.h>
#include <xcb/xcb_atom.h>
#include <stdio.h>
#include <string.h>

xcb_screen_t *screen_of_display(xcb_connection_t *c, int screen) {
	xcb_screen_iterator_t iter;
	iter = xcb_setup_roots_iterator (xcb_get_setup (c));
	for (; iter.rem; --screen, xcb_screen_next (&iter))
		if (screen == 0)
			return iter.data;
	return NULL;
}

int main(void) {
	char *_NAWS = "_NET_ACTIVE_WINDOW";
	int screen_default;
	xcb_screen_t *screen;
	xcb_connection_t *conn = xcb_connect(NULL, &screen_default);
	if (!conn) {
		fprintf(stderr,"NOK\n");
		return 1;
	}
	xcb_intern_atom_cookie_t _NAWC;
	_NAWC = xcb_intern_atom(conn, 0, strlen(_NAWS), _NAWS);
	xcb_intern_atom_reply_t *_NAWSR = xcb_intern_atom_reply(conn, _NAWC, NULL);
	xcb_window_t root = { 0 };
	screen = screen_of_display(conn, screen_default);
	if (screen)
		root = screen->root;

	xcb_get_property_cookie_t propc = xcb_get_property(
		conn, 0, root, _NAWSR->atom, WINDOW, 0L, sizeof(xcb_window_t*)
	);
	xcb_get_property_reply_t *prop = xcb_get_property_reply(conn, propc, NULL);
	xcb_window_t *active = xcb_get_property_value(prop);
	printf("0x%0*x\n", (int)(2*sizeof(*active)), *active);
	xcb_disconnect(conn);
	return 0;
}
