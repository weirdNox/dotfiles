alsa_monitor.rules[#alsa_monitor.rules + 1] = {
    matches = {
        {{ "node.name", "matches", "alsa_*.usb-*" }},
    },
    apply_properties = {
        ["session.suspend-timeout-seconds"] = 0,
    },
}
