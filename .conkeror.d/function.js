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

