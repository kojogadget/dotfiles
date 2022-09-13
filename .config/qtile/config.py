import os
import re
import socket
import subprocess

from libqtile import qtile
from libqtile import bar, layout, widget, hook
from libqtile.config import (
    Click,
    Drag,
    Group,
    Key,
    Match,
    Screen,
    Rule,
    KeyChord,
    ScratchPad,
    DropDown,
)
from libqtile.command import lazy
from libqtile.lazy import lazy
from typing import List  # noqa: F401
from Xlib import display as xdisplay

# from libqtile.utils import guess_terminal

### Variables
sysBin = "/usr/bin/"
myBin = "/home/kg/.bin/"

mod = "mod4"
terminal = "kitty"
myBrowser = "firefox"
myEditor = "emacsclient -c -a emacs"

keys = [
    ### Essentials
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod], "t", lazy.spawn(sysBin + "alacritty"), desc="Launch terminal"),
    Key([mod, "shift"], "Return", lazy.spawn("rofi -show drun"), desc="Run Launcher"),
    Key([mod], "b", lazy.spawn(myBrowser), desc="Firefox"),
    Key([mod, "shift"], "e", lazy.spawn(myEditor), desc="Emacs"),
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod, "shift"], "c", lazy.window.kill(), desc="Kill active window"),
    Key([mod], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "shift"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    ### Switching monitors
    Key([mod], "period", lazy.next_screen(), desc="Move focus to next monitor"),
    Key([mod], "comma", lazy.prev_screen(), desc="Move focus to prev monitor"),
    ### Window controls
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key(
        [mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"
    ),
    Key(
        [mod, "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    ### Window size
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key(
        [mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"
    ),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    ### Layout mode
    Key(
        [mod],
        "m",
        lazy.layout.maximize(),
        desc="toggle window between minimum and maximum sizes",
    ),
    Key([mod, "shift"], "f", lazy.window.toggle_floating(), desc="toggle floating"),
    Key([mod], "f", lazy.window.toggle_fullscreen(), desc="toggle fullscreen"),
    ### Sound
    Key(
        [],
        "XF86AudioMute",
        lazy.spawn(myBin + "changevolume mute"),
        desc="Mute",
    ),
    Key(
        [],
        "XF86AudioLowerVolume",
        lazy.spawn(myBin + "changevolume down"),
        desc="Lower Volume",
    ),
    Key(
        [],
        "XF86AudioRaiseVolume",
        lazy.spawn(myBin + "changevolume up"),
        desc="Raise Volume",
    ),
    # Key(
    #     [],
    #     "XF86AudioMute",
    #     lazy.spawn(sysBin + "amixer set Master toggle"),
    #     desc="Mute",
    # ),
    # Key(
    #     [],
    #     "XF86AudioLowerVolume",
    #     lazy.spawn(sysBin + "amixer set Master 5%-"),
    #     desc="Lower Volume",
    # ),
    # Key(
    #     [],
    #     "XF86AudioRaiseVolume",
    #     lazy.spawn(sysBin + "amixer set Master 5%+"),
    #     desc="Raise Volume",
    # ),
    ### Brightness
    Key(
        [],
        "XF86MonBrightnessUp",
        lazy.spawn(myBin + "changebrightness up"),
        desc="Increase brightness",
    ),
    Key(
        [],
        "XF86MonBrightnessDown",
        lazy.spawn(myBin + "changebrightness down"),
        desc="Decrease brightness",
    ),
    ### Notification
    Key(
        [mod],
        "n",
        lazy.spawn(sysBin + "dunstctl close"),
        desc="Close Notification",
    ),
    Key(
        [mod, "control"],
        "n",
        lazy.spawn(sysBin + "dunstctl close-all"),
        desc="Close Notification",
    ),
    Key(
        [mod, "shift"],
        "n",
        lazy.spawn(sysBin + "dunstctl history-pop"),
        desc="Close Notification",
    ),
    ### Emacs
    KeyChord(
        [mod],
        "e",
        [
            Key(
                [],
                "e",
                lazy.spawn("emacsclient -c -a 'emacs'"),
                desc="Emacsclient Dashboard",
            ),
            # Key(
            #     [],
            #     "a",
            #     lazy.spawn(
            #         "emacsclient -c -a 'emacs' --eval '(emms)' --eval '(emms-play-directory-tree \"~/Music/\")'"
            #     ),
            #     desc="Emacsclient EMMS (music)",
            # ),
        ],
    ),
    ### Scripts
    KeyChord(
        [mod],
        "p",
        [
            Key(
                [],
                "b",
                lazy.spawn(myBin + "set-wallpaper"),
                desc="Set wallpaper",
            ),
            Key([], "l", lazy.spawn(myBin + "i3lock-black"), desc="Lock Screen"),
            Key([], "p", lazy.spawn(myBin + "powermenu"), desc="Power Menu"),
            Key([], "w", lazy.spawn(myBin + "wifimenu"), desc="Wifi Menu"),
        ],
    ),
]


def get_num_monitors():
    num_monitors = 0
    try:
        display = xdisplay.Display()
        screen = display.screen()
        resources = screen.root.xrandr_get_screen_resources()

        for output in resources.outputs:
            monitor = display.xrandr_get_output_info(output, resources.config_timestamp)
            preferred = False
            if hasattr(monitor, "preferred"):
                preferred = monitor.preferred
            elif hasattr(monitor, "num_preferred"):
                preferred = monitor.num_preferred
            if preferred:
                num_monitors += 1
    except Exception as e:
        # always setup at least one monitor
        return 1
    else:
        return num_monitors


num_monitors = get_num_monitors()

group_names = "dev www sys doc vms scl mus game div".split()
groups = [Group(name, layout="Bsp") for name in group_names]
for i, name in enumerate(group_names):
    indx = str(i + 1)
    keys += [
        Key([mod], indx, lazy.group[name].toscreen()),
        Key([mod, "shift"], indx, lazy.window.togroup(name)),
    ]

layout_theme = {
    "border_width": 2,
    "margin": 4,
    # "border_focus": "e1acff",
    "border_focus": "345F0Ca0",
    "border_normal": "1D2330",
}

layouts = [
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    layout.Bsp(**layout_theme),
    # layout.Matrix(**layout_theme),
    # layout.MonadWide(**layout_theme),
    # layout.RatioTile(**layout_theme),
    # layout.Tile(**layout_theme),
    # layout.TreeTab(**layout_theme),
    layout.VerticalTile(**layout_theme),
    # layout.Zoomy(**layout_theme),
    layout.MonadTall(**layout_theme),
    layout.Max(**layout_theme),
    layout.Floating(**layout_theme),
]

colors = [
    ["#282c34", "#282c34"],
    ["#1c1f24", "#1c1f24"],
    ["#dfdfdf", "#dfdfdf"],
    ["#ff6c6b", "#ff6c6b"],
    ["#98be65", "#98be65"],
    ["#da8548", "#da8548"],
    ["#51afef", "#51afef"],
    ["#c678dd", "#c678dd"],
    ["#46d9ff", "#46d9ff"],
    ["#a9a1e1", "#a9a1e1"],
]

widget_defaults = dict(
    # font="JetBrain Mono Nerd Font",
    font="Ubuntu",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.Image(
                    filename="/home/kg/.config/qtile/icons/arch-emoji.png",
                    scale="True",
                    margin_y=3,
                    margin_x=6,
                ),
                widget.GroupBox(
                    font="Ubuntu Bold",
                    fontsize=12,
                    margin_y=3,
                    margin_x=0,
                    padding_y=5,
                    padding_x=3,
                    borderwidth=3,
                    active=colors[2],
                    inactive=colors[9],
                    rounded=False,
                    highlight_color=colors[1],
                    highlight_method="line",
                    this_current_screen_border=colors[6],
                    this_screen_border=colors[4],
                    other_current_screen_border=colors[6],
                    other_screen_border=colors[4],
                    # foreground=colors[2],
                    # background=colors[0],
                ),
                widget.Prompt(),
                widget.CurrentLayoutIcon(
                    custom_icon_paths=[os.path.expanduser("~/.config/qtile/icons")],
                    # foreground=colors[6],
                    # background=colors[1],
                    padding=0,
                    scale=0.6,
                ),
                widget.Spacer(),
                # widget.Chord(
                #     chords_colors={
                #         "launch": ("#ff0000", "#ffffff"),
                #     },
                #     name_transform=lambda name: name.upper(),
                # ),
                widget.Systray(icon_size=20, padding=5),
                widget.Spacer(length=4),
                widget.Sep(),
                # widget.Spacer(length=2),
                # widget.PulseVolume(font="FontAwesome", fmt=" {}", fontsize=14),
                # widget.Spacer(length=2),
                # widget.Sep(),
                widget.Spacer(length=3),
                widget.CheckUpdates(
                    update_interval=1800,
                    font="FontAwesome",
                    fontsize=14,
                    distro="Arch_checkupdates",
                    display_format=" {updates} ",
                    no_update_string="",
                    # foreground=colors[1],
                    # colour_have_updates=colors[1],
                    # colour_no_updates=colors[1],
                    mouse_callbacks={
                        "Button1": lambda: qtile.cmd_spawn(
                            terminal + " -e sudo pacman -Syu"
                        )
                    },
                    padding=5,
                    # background=colors[5],
                ),
                widget.Spacer(length=2),
                widget.Sep(),
                widget.Spacer(length=2),
                widget.Battery(font="FontAwesome", format=" {percent:2.0%}"),
                # widget.BatteryIcon(scale=1),
                widget.Spacer(length=2),
                widget.Sep(),
                widget.Spacer(length=4),
                widget.Clock(format="%d.%b.%Y %R"),
                widget.Spacer(length=2),
                widget.Sep(),
                widget.Spacer(length=3),
                widget.QuickExit(
                    font="FontAwesome",
                    fontsize=14,
                    default_text="",
                    countdown_format="[{}]",
                    padding=5,
                ),
                widget.Spacer(length=8),
            ],
            26,
            background=colors[0],
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
    ),
]

if num_monitors > 1:
    for m in range(num_monitors - 1):
        screens.append(
            Screen(
                top=bar.Bar(
                    [
                        widget.Image(
                            filename="/home/kg/.config/qtile/icons/arch-emoji.png",
                            scale="True",
                            margin_y=3,
                            margin_x=6,
                        ),
                        widget.GroupBox(
                            font="Ubuntu Bold",
                            fontsize=12,
                            margin_y=3,
                            margin_x=0,
                            padding_y=5,
                            padding_x=3,
                            borderwidth=3,
                            active=colors[2],
                            inactive=colors[9],
                            rounded=False,
                            highlight_color=colors[1],
                            highlight_method="line",
                            this_current_screen_border=colors[6],
                            this_screen_border=colors[4],
                            other_current_screen_border=colors[6],
                            other_screen_border=colors[4],
                            # foreground=colors[2],
                            # background=colors[0],
                        ),
                        widget.Prompt(),
                        widget.CurrentLayoutIcon(
                            custom_icon_paths=[
                                os.path.expanduser("~/.config/qtile/icons")
                            ],
                            # foreground=colors[2],
                            # background=colors[0],
                            padding=0,
                            scale=0.6,
                        ),
                        widget.WindowName(),
                        widget.Clock(format="%d.%b.%Y %R"),
                    ],
                    26,
                    background=colors[0],
                    # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
                    # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
                ),
            ),
        )

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

groups.append(
    ScratchPad(
        "scratchpad",
        [
            DropDown("term", "alacritty", width=0.4, x=0.3, y=0.2),
            DropDown("calc", "galculator", width=0.18, x=0.78, y=0.03),
            DropDown(
                "mixer", "pavucontrol", width=0.4, height=0.6, x=0.3, y=0.1, opacity=1
            ),
            DropDown(
                "bitwarden",
                "bitwarden-desktop",
                width=0.4,
                height=0.6,
                x=0.3,
                y=0.1,
                opacity=1,
            ),
        ],
    )
)

keys.extend(
    [
        KeyChord(
            [mod],
            "d",
            [
                Key([], "t", lazy.group["scratchpad"].dropdown_toggle("term")),
                Key([], "s", lazy.group["scratchpad"].dropdown_toggle("mixer")),
                Key([], "c", lazy.group["scratchpad"].dropdown_toggle("calc")),
                Key([], "b", lazy.group["scratchpad"].dropdown_toggle("bitwarden")),
            ],
        ),
    ]
)

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None


@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser("~")
    subprocess.Popen([home + "/.config/qtile/autostart.sh"])


# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
