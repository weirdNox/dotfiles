alsa_monitor.rules[#alsa_monitor.rules + 1] = {
    matches = {
        {{ "node.name", "matches", "alsa_input.*"  }},
        {{ "node.name", "matches", "alsa_output.*" }},
    },
    apply_properties = {
        ["audio.rate"] = 48000,
    },
}
