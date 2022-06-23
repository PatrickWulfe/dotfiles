import os
import re
import socket
import subprocess
from libqtile import layout, bar, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.command import lazy
from libqtile.utils import guess_terminal
from libqtile.widget import Spacer
from random import randint
#import arcobattery

# black dark/light
color0 =                          "#1C1E26"
color8 =                          "#6C6F93"

# red dark/light
color1 =                          "#E9436F"
color9 =                          "#E9436F"

# green dark/light
color2 =                          "#27D796"
color10 =                         "#09F7A0"

# yellow dark/light
color3 =                          "#FAB795"
color11 =                         "#FAB795"

# blue dark/light
color4 =                          "#25B2BC"
color12 =                         "#25B2BC"

# magenta dark/light
color5 =                          "#B877DB"
color13 =                         "#B877DB"

# cyan dark/light
color6 =                          "#21BFC2"
color14 =                         "#21BFC2"

# white dark/light
color7 =                          "#FDF0ED"
color15 =                         "#f8f8f8"

#mod4 or mod = super key
mod                     = "mod4"
mod1                    = "alt"
mod2                    = "control"
terminal                = guess_terminal()
home                    = os.path.expanduser('~')

@lazy.function
def window_to_prev_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i - 1].name)

@lazy.function
def window_to_next_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i + 1].name)

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),

# FLIP LAYOUT FOR MONADTALL/MONADWIDE
    Key([mod, "shift"], "f", lazy.layout.flip()),

# WINDOW SIZE FOR MONAD
    Key([mod], "i", lazy.layout.grow(), desc="Grow window size"),
    Key([mod, "shift"], "i", lazy.layout.shrink(), desc="Shrink window size"),
    Key([mod], "o", lazy.layout.maximize(), desc="Maximize windows"),

# FLIP LAYOUT FOR BSP
    Key([mod, "mod1"], "k", lazy.layout.flip_up()),
    Key([mod, "mod1"], "j", lazy.layout.flip_down()),
    Key([mod, "mod1"], "l", lazy.layout.flip_right()),
    Key([mod, "mod1"], "h", lazy.layout.flip_left()),

# MOVE WINDOWS UP OR DOWN MONADTALL/MONADWIDE LAYOUT
    Key([mod, "shift"], "Up", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "Down", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "Left", lazy.layout.swap_left()),
    Key([mod, "shift"], "Right", lazy.layout.swap_right()),

# TOGGLE FLOATING LAYOUT
    Key([mod, "shift"], "space", lazy.window.toggle_floating()),
    ]

groups = []

# FOR QWERTY KEYBOARDS
group_names = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0",]

# group_labels = ["1 ", "2 ", "3 ", "4 ", "5 ", "6 ", "7 ", "8 ", "9 ", "0",]
group_labels = ["", "", "", "", "", "", "", "", "", "",]
# group_labels = ["Web", "Edit/chat", "Image", "Gimp", "Meld", "Video", "Vb", "Files", "Mail", "Music",]

group_layouts = ["columns", "columns", "max", "columns", "columns", "columns", "columns", "columns", "columns", "columns",]
# group_layouts = ["monadtall", "matrix", "monadtall", "bsp", "monadtall", "matrix", "monadtall", "bsp", "monadtall", "monadtall",]

for i in range(len(group_names)):
    groups.append(
        Group(
            name=group_names[i],
            layout=group_layouts[i].lower(),
            label=group_labels[i],
        ))

for i in groups:
    keys.extend([

# CHANGE WORKSPACES
        Key([mod], i.name, lazy.group[i.name].toscreen()),
        Key(["mod1"], "Tab", lazy.screen.next_group()),
        Key(["mod1", "shift"], "Tab", lazy.screen.prev_group()),

# MOVE WINDOW TO SELECTED WORKSPACE 1-10 AND STAY ON WORKSPACE
        # Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
# MOVE WINDOW TO SELECTED WORKSPACE 1-10 AND FOLLOW MOVED WINDOW TO WORKSPACE
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name) , lazy.group[i.name].toscreen()),
    ])

border_width = 2
margin = 8
col_mar = margin/2
def init_layout_theme():
    return {"margin":margin,
            "border_width": border_width,
            "border_focus": color3,
            "border_normal": color8
            }

layout_theme = init_layout_theme()

layouts = [
    layout.Columns(**layout_theme),
    # layout.MonadThreeCol(**layout_theme, ratio=.4),
    # layout.MonadTall(**layout_theme),
    # layout.MonadWide(**layout_theme),
    # layout.Matrix(**layout_theme),
    # layout.Bsp(**layout_theme),
    # layout.Floating(**layout_theme),
    # layout.RatioTile(**layout_theme),
    # layout.Max(**layout_theme)
]

# WIDGETS FOR THE BAR
font_size = 16

def init_widgets_defaults():
    return dict(font="Product Sans",
                fontsize = font_size,
                padding = 2,
                background=color0)

widget_defaults = init_widgets_defaults()

def init_widgets_list():
    prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())
    bgcolors = [color1, color2, color3, color5, color4]
    arr_len = len(bgcolors)
    foreground = color0
    index = randint(0, arr_len - 1) # to make sure the bg color always alternates and the colors randomize
    divider_size = 51
    padding = 12
    widgets_list = [
               widget.Spacer(length=12, background = color0),
               widget.CurrentLayout(
                        font = "Product Sans",
                        foreground = bgcolors[index % arr_len],
                        background = color0
                        ),
               widget.Sep(
                        linewidth = 1,
                        padding = padding,
                        foreground = bgcolors[index % arr_len],
                        background = color0
                        ),
               widget.GroupBox(font="FiraCode Nerd Font",
                        margin_y = 3,
                        margin_x = 0,
                        padding_y = 6,
                        padding_x = 6,
                        borderwidth = 0,
                        disable_drag = True,
                        active = color6,
                        inactive = color8,
                        rounded = False,
                        highlight_method = "text",
                        this_current_screen_border = color7,
                        foreground = bgcolors[(index:=index+1) % arr_len],
                        background = color0
                        ),
               widget.Sep(
                        linewidth = 1,
                        padding = padding,
                        foreground = bgcolors[(index:=index+1) % arr_len],
                        background = color0
                        ),
               widget.WindowName(
                        font = "Product Sans",
                        foreground = bgcolors[index % arr_len],
                        background = color0,
                        ),
               widget.TextBox(text = ' ', background = color0, foreground = bgcolors[(index:=index + 1) % arr_len], margin_y = 20, padding = -1, fontsize = divider_size),
               widget.TextBox(
                        font="FiraCode Nerd Font",
                        text=" ﯱ",
                        foreground = foreground,
                        background = bgcolors[index % arr_len],
                        padding = 0,
                        ),
               widget.NetGraph(
                        font="Product Sans",
                        bandwidth="down",
                        interface="auto",
                        fill_color = color0,
                        foreground = foreground,
                        background = bgcolors[index % arr_len],
                        graph_color = color0,
                        border_color = color0,
                        padding = 0,
                        border_width = 0,
                        line_width = 1,
                        samples = 20,
                        ),
               widget.TextBox(text = ' ', background = bgcolors[index % arr_len], foreground = bgcolors[(index:=index + 1) % arr_len], margin_y = 20, padding = -1, fontsize = divider_size),
               widget.TextBox(
                        font="FiraCode Nerd Font",
                        text="  ",
                        foreground = foreground,
                        background = bgcolors[index % arr_len],
                        padding = 0,
                        ),
               widget.CPUGraph(
                        border_color = color0,
                        fill_color = color0,
                        graph_color = color0,
                        background = bgcolors[index % arr_len],
                        border_width = 0,
                        line_width = 1,
                        core = "all",
                        samples = 20,
                        type = "box"
                        ),
               widget.TextBox(text = ' ', background = bgcolors[index % arr_len], foreground = bgcolors[(index:=index + 1) % arr_len], margin_y = 20, padding = -1, fontsize = divider_size),
               widget.TextBox(
                        font="FiraCode Nerd Font",
                        text="  ",
                        foreground = foreground,
                        background = bgcolors[index % arr_len],
                        padding = 0,
                        ),
               widget.MemoryGraph(
                        border_color = color0,
                        fill_color = color0,
                        graph_color = color0,
                        background = bgcolors[index % arr_len],
                        border_width = 0,
                        line_width = 1,
                        core = "all",
                        samples = 20,
                        type = "box"
               ),
               widget.TextBox(text = ' ', background = bgcolors[index % arr_len], foreground = bgcolors[(index:=index + 1) % arr_len], margin_y = 20, padding = -1, fontsize = divider_size),
               widget.TextBox(
                        font="FiraCode Nerd Font",
                        text="  ",
                        foreground = foreground,
                        background = bgcolors[index % arr_len],
                        padding = 0,
                        ),
               widget.Clock(
                        font = "Product Sans",
                        foreground = foreground,
                        background = bgcolors[index % arr_len],
                        format="%Y-%m-%d %H:%M"
                        ),
               widget.TextBox(text = ' ', background = bgcolors[index % arr_len], foreground = color0, margin_y = 20, padding = -1, fontsize = divider_size),
               widget.Systray(
                        background = color0,
                        icon_size=24,
                        margin = 5,
                        padding = 4
                        ),
               widget.Spacer(length=12, background = color0)
              ]
    return widgets_list

widgets_list = init_widgets_list()

def init_widgets_screen1():
    widgets_screen1 = init_widgets_list()
    return widgets_screen1

def init_widgets_screen2():
    widgets_screen2 = init_widgets_list()
    return widgets_screen2

widgets_screen1 = init_widgets_screen1()
widgets_screen2 = init_widgets_screen2()

bar_size = 28

def init_screens():
    return [Screen(left=bar.Gap(margin), right=bar.Gap(margin), bottom=bar.Gap(margin),
        top=bar.Bar(widgets=init_widgets_screen1(), size=bar_size, opacity=0.95, margin=[0,0,margin,0])),
            Screen(left=bar.Gap(margin), right=bar.Gap(margin), bottom=bar.Gap(margin),
                top=bar.Bar(widgets=init_widgets_screen2(), size=bar_size, opacity=0.95, margin=[0,0,margin,0]))]

screens = init_screens()

# MOUSE CONFIGURATION
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size())
]

dgroups_key_binder = None
dgroups_app_rules = []

############################################################
## => FUNCTIONS
############################################################
main = None

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/scripts/autostart.sh'])
    execute_once('guake')

@hook.subscribe.client_new
def set_floating(window):
    if (window.window.get_wm_transient_for()
            or window.window.get_wm_type() in floating_types):
        window.floating = True

follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    *layout.Floating.default_float_rules,
    Match(wm_class='guake'),
    # {'wmclass': 'Guake'},
    # {'wmclass': 'confirm'},
    # {'wmclass': 'dialog'},
    # {'wmclass': 'download'},
    # {'wmclass': 'error'},
    # {'wmclass': 'file_progress'},
    # {'wmclass': 'notification'},
    # {'wmclass': 'splash'},
#     {'wmclass': 'toolbar'},
    # {'wmclass': 'confirmreset'},
#     {'wmclass': 'makebranch'},
#     {'wmclass': 'maketag'},
#     {'wmclass': 'Arandr'},
#     {'wmclass': 'feh'},
#     {'wmclass': 'Galculator'},
#     {'wmclass': 'arcolinux-logout'},
#     {'wmclass': 'xfce4-terminal'},
#     {'wname': 'branchdialog'},
    # {'wname': 'Open File'},
#     {'wname': 'pinentry'},
    # {'wmclass': 'ssh-askpass'},
],  fullscreen_border_width = 0, border_width = border_width)

auto_fullscreen = True

floating_types = ["notification", "toolbar", "splash",
                  "dialog"]

focus_on_window_activation = "smart" # or smart

wmname = "LG3D"
