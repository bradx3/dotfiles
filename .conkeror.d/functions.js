//toggle adblock
interactive("adblock-toggle",
            "Toggles adblock enabled/disabled.",
            function(I) {
		var name = "extensions.adblockplus.enabled";
		var orig = get_pref(name);
		user_pref(name, !orig);

		I.window.minibuffer.message("Adblock " + (orig ? "disabled" : "enabled"));
	    });
define_key(content_buffer_normal_keymap, "a", "adblock-toggle");

// reload conkerorrc
interactive("reload-rc",
            "Reload the Conkerorrc.",
            function(I) { load_rc_file("/Users/brad/.conkerorrc"); });


// switch to buffers 1-10 using keys
function define_key_buffer_switch(key, buf_num) {
    define_key(content_buffer_normal_keymap, key, function (I) {
            switch_to_buffer(I.window, I.window.buffers.get_buffer(buf_num));
        });
}
for (let i = 0; i < 10; ++i)
    define_key_buffer_switch(i == 9 ? "0" : (i+1).toString(), i);


// Change tabs bars to vertical
function vertical_tab_bars(window) {
    var tabBar = window.tab_bar.element;
    var container = window.buffers.container;
    var target = container.parentNode;
    
    var hbox = create_XUL(window, "hbox");
    hbox.setAttribute("flex", "1");
    target.insertBefore(hbox, container);

    tabBar.parentNode.removeChild(tabBar);
    hbox.appendChild(tabBar);
    hbox.appendChild(container);
    window.tab_bar.element.setAttribute("orient", "vertical");
}
add_hook("window_initialize_hook", vertical_tab_bars);

