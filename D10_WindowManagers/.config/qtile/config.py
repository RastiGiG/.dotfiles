# -*- coding: utf-8 -*-
# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

## Import python System Libraries
import os
import socket
import subprocess

## Regexp are always neat
import re

## Import Qtile Libraries
from libqtile.config import Drag, Key, Screen, Group, Drag, Click, Rule, Match
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile import layout, bar, widget, hook
from libqtile.widget import Spacer

## Additional Libraries
from typing import List  # noqa: F401

## Setup Variables
mod = "mod4"
mod1 = "alt"
mod2 = "control"
## terminal = guess_terminal()
terminal = "alacritty"
home = os.path.expanduser('~')


## Helpful function definitions
def window_to_prev_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i - 1].name)

def windqow_to_next_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i + 1].name)

def window_to_previous_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i != 0:
        group = qtile.screens[i - 1].group.name
        qtile.current_window.togroup(group)

def window_to_next_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i + 1 != len(qtile.screens):
        group = qtile.screens[i + 1].group.name
        qtile.current_window.togroup(group)

def switch_screens(qtile):
    i = qtile.screens.index(qtile.current_screen)
    group = qtile.screens[i - 1].group
    qtile.current_screen.set_group(group)

## Configure Keybindings
keys = [

    # Launch terminal, kill window, restart and exit Qtile

    Key([mod], "Return", lazy.spawn(myTerm),
        desc="Launch terminal"),
    Key([mod], "r", lazy.spawncmd(),
        desc="Spawn a command using a prompt widget"),
    Key([mod], "w", lazy.window.kill(),
        desc="Close focused window"),
    Key([mod], "q", lazy.window.kill(),
        desc="Close focused window"),
    Key([mod], "Escape", lazy.spawn('xkill'),
        desc="Kill current Window"),
    Key([mod, "shift"], "r", lazy.restart(),
        desc="Restart Qtile"),
    Key([mod, "shift"], "q", lazy.shutdown(),
        desc="Shutdown Qtile"),
    Key([mod], "x", lazy.spawn("powerspec")),
    # Dmenu, Rofi and Gmrun

    Key([mod, "mod1"], "d", lazy.spawn("dmenu_run")),
    Key([mod, "mod1"], "n", lazy.spawn("networkmanager_dmenu")),
    Key([mod, "mod1"], "r", lazy.spawn("dmenufm")),
    Key([mod, "mod1"], "space", lazy.spawn("rofi -modi drun -show drun -show-icons")),
    Key([mod, "mod1"], "c", lazy.spawn("rofi -show emoji -modi emoji")),
    Key([mod, "mod1"], "v", lazy.spawn("rofi-locate")),
    Key([mod, "mod1"], "z", lazy.spawn("gmrun")),

    # Custom key bindings

    Key([mod, "shift"], "w", lazy.spawn("brave")),
    Key([mod, "shift"], "x", lazy.spawn("firefox")),
    Key([mod, "shift"], "t", lazy.spawn("tor-browser")),
    Key([mod, "shift"], "m", lazy.spawn("thunderbird")),
    Key([mod, "shift"], "m", lazy.spawn("veracrypt")),
    Key([mod, "shift"], "m", lazy.spawn("keepass")),
    Key([mod, "shift"], "f", lazy.spawn("pcmanfm")),
    Key([mod, "shift"], "s", lazy.spawn("pamac-manager")),
    Key([mod, "shift"], "i", lazy.spawn("nitrogen")),
    Key([mod, "shift"], "p", lazy.spawn('pavucontrol')),

   # Toggle layouts

    Key([mod], "space", lazy.next_layout()),
    Key([mod, "shift"], "space", lazy.window.toggle_floating()),

    # Switch between windows - enable arrow keys and vim keys

    Key([mod], "Left", lazy.layout.left(),
        desc="Move focus to left"),
    Key([mod], "Right", lazy.layout.right(),
        desc="Move focus to right"),
    Key([mod], "Down", lazy.layout.down(),
        desc="Move focus down"),
    Key([mod], "Up", lazy.layout.up(),
        desc="Move focus up"),
    Key([mod], "h", lazy.layout.left(),
        desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(),
        desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(),
        desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(),
        desc="Move focus up"),
    # Move through stack
    Key([mod, "mod1"], "Left", lazy.layout.previous(),
        desc="Go to previous layout"), # Stack
    Key([mod, "mod1"], "h", lazy.layout.previous(),
        desc="Go to previous layout"), # Stack
    Key([mod, "mod1"], "Right", lazy.layout.next(),
        desc="Go to next layout"), # Stack
    Key([mod, "mod1"], "l", lazy.layout.next(),
        desc="Go to next layout"), # Stack

    # Resize layout
    # If current window is on the edge of screen 
    # and direction will be to screen edge - window
    # would shrink.

    Key([mod], "n", lazy.layout.normalize(),
        desc="Reset all window sizes"),
    Key([mod], "m", lazy.layout.toggle_maximize(),
        desc="Maximize Window"), # Stack
    Key([mod, "control"], "l",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        desc="Grow window to the right"
        ),
    Key([mod, "control"], "Right",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        desc="Grow window to the right"
        ),
    Key([mod, "control"], "h",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        desc="Grow window to the left"
        ),
    Key([mod, "control"], "Left",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        desc="Grow window to the left"
        ),
    Key([mod, "control"], "k",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        desc="Grow window up"
        ),
    Key([mod, "control"], "Up",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        desc="Grow window up"
        ),
    Key([mod, "control"], "j",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        desc="Grow window down"
        ),
    Key([mod, "control"], "Down",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        desc="Grow window down"
        ),

    # Move windows between left/right columns or move
    # up/down in current stack.
    # Moving out of range in Columns layout will create
    # new column.
    Key([mod, "shift"], "f", lazy.layout.flip()),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "Left",
        lazy.layout.swap_left(),
        lazy.layout.client_to_previous()), # Stack
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "Right",
        lazy.layout.swap_right(),
        lazy.layout.client_to_next()), # Stack
    Key([mod, "shift"], "Down", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "Up", lazy.layout.shuffle_up(),
        desc="Move window up"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(),
        desc="Move window up"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but 
    # still with multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),

    # Working with multiple Screens (Physical Monitors)

    # Switch focus to a physical monitor
    # (dual/triple set up)

    Key([mod], "period", lazy.next_screen(),
        desc="Go to next screen"),
    Key([mod], "comma", lazy.prev_screen(),
        desc="Go to previous screen"),
    Key([mod], "a", lazy.to_screen(0),
        desc="Go to screen 0"),
    Key([mod], "b", lazy.to_screen(1),
        desc="Go to screen 1"),
    Key([mod], "c", lazy.to_screen(2),
        desc="Go to screen 2"),

    # Move windows to different physical screens

    Key([mod, "shift"], "period",
        lazy.function(window_to_previous_screen)),
    Key([mod, "shift"], "comma",
        lazy.function(window_to_next_screen)),
    Key([mod], "t", lazy.function(switch_screens)),


    # Enable Volume keys

    Key([], "XF86AudioMute",
        lazy.spawn("amixer -D pulse sset Master toggle"),
        desc="Mute/Unmute"),
    Key([], "XF86AudioLowerVolume",
        lazy.spawn("amixer -D pulse sset Master 5%-"),
        desc="Decrease Volume"),
    Key([], "XF86AudioRaiseVolume",
        lazy.spawn("amixer -D pulse sset Master 5%+"),
        desc="Increase Volume"),
]

# Allocate layouts and labels

group_names = 'DEV SYS WRK WEB MED VID MUS NET ETC'.split()
group_layouts = ["monadtall", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall",]
group_labels = ["", "", "🖧", "", "", "", "", "𝄞",  "",]
group_attributes = list(zip(group_names, group_layouts, group_labels))
groups = [Group(name, layout=layout, label=label) for name, layout, label in group_attributes]
for i, name in enumerate(group_names, 1):
  indx = str(i)
  keys.extend([
      # Workspace navigation
    Key([mod], indx, lazy.group[name].toscreen()),
    Key([mod], 'Tab', lazy.screen.next_group()),
    Key([mod, 'control'], indx, lazy.window.togroup(name)),
    Key([mod, 'shift'], indx, lazy.window.togroup(name), lazy.group[name].toscreen()),
    ])


def init_layout_theme():
    return {"margin":5,
            "border_width":2,
            "border_focus": "#5e81ac",
            "border_normal": "#4c566a"
            }

layout_theme = init_layout_theme()


layouts = [
    layout.Columns(border_focus_stack=['#d75f5f', '#8f3d3d'], border_width=4),
    layout.MonadTall(margin=5, border_width=2, border_focus="#5e81ac", border_normal="#4c566a"),
    layout.MonadWide(margin=8, border_width=2, border_focus="#5e81ac", border_normal="#4c566a"),
    layout.Floating(**layout_theme),
    layout.Max(**layout_theme),
    layout.Stack(num_stacks=2, **layout_theme),
    layout.Bsp(),
    layout.Matrix(),
    layout.RatioTile(),
    layout.Tile(),
    layout.TreeTab(),
    layout.VerticalTile(),
    layout.Zoomy()
]

   # Bar colours

def init_colors():
    return [["#2E3440", "#2E3440"], # color 0
            ["#2E3440", "#2E3440"], # color 1
            ["#c0c5ce", "#c0c5ce"], # color 2
            ["#fba922", "#fba922"], # color 3
            ["#3384d0", "#3384d0"], # color 4
            ["#f3f4f5", "#f3f4f5"], # color 5
            ["#cd1f3f", "#cd1f3f"], # color 6
            ["#62FF00", "#62FF00"], # color 7
            ["#6790eb", "#6790eb"], # color 8
            ["#a9a9a9", "#a9a9a9"]] # color 9


colors = init_colors()


   # Widgets

def init_widgets_defaults():
    return dict(font="UbuntuMono Nerd Font",
                fontsize = 14,
                padding = 2,
                background=colors[1])

widget_defaults = init_widgets_defaults()

def init_widgets_list():
    prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())
    widgets_list = [
        widget.GroupBox(font="FontAwesome",
                        fontsize = 14,
                        margin_y = 3,
                        margin_x = 0,
                        padding_y = 6,
                        padding_x = 5,
                        borderwidth = 0,
                        disable_drag = True,
                        active = colors[9],
                        inactive = colors[5],
                        rounded = False,
                        highlight_method = "text",
                        this_current_screen_border = colors[8],
                        foreground = colors[2],
                        background = colors[1]
                        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        widget.CurrentLayout(
            font = "UbuntuMono Nerd Font",
            fontsize = 14,
            foreground = colors[5],
            background = colors[1]
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        widget.WindowName(font="UbuntuMono Nerd Font",
                          fontsize = 14,
                          foreground = colors[5],
                          background = colors[1]
                        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        widget.CurrentLayoutIcon(
            foreground = colors[5],
            background = colors[1],
            padding = 0,
            scale = 0.7
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        widget.TextBox(
            text="  ✏️  ",
            foreground=colors[6],
            background=colors[1],
            mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn('geany ' + home + '/.config/qtile/config.py')},
            padding = 2,
            fontsize=12
        ), 
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),          
        widget.TextBox(
            text="  🖥️ ",
            foreground=colors[6],
            background=colors[1],
            mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn(myTerm + ' -e gtop')},
            padding = 2,
            fontsize=12
                        ),
        widget.CPU(
            format = '{load_percent}% ',
            font = "UbuntuMono Nerd Font",
            fontsize = 14,
            foreground = colors[5],
            background = colors[1],
            update_interval = 3
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        widget.TextBox(
            text="  🧠 ",
            foreground=colors[4],
            background=colors[1],
            mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn(myTerm + ' -e htop')},
            padding = 0,
            fontsize=12
        ),
        widget.Memory(
            font="UbuntuMono Nerd Font",
            format = '{MemUsed}M/{MemTotal}M ',
            update_interval = 1,
            fontsize = 14,
            foreground = colors[5],
            background = colors[1]
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        widget.TextBox(
            text = " 🌡️",
            padding = 2,
            foreground = colors[5],
            background = colors[1],
            mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn('xsensors')},
            fontsize = 12
        ),
        widget.ThermalSensor(
            font="UbuntuMono Nerd Font",
            fontsize = 14,
            fmt = '{} ',
            foreground = colors[5],
            background = colors[1],
            threshold = 90,
            padding = 5
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        widget.TextBox(
            text = " 💾",
            foreground = colors[5],
            background = colors[1],
            mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn('xdiskusage')},
            padding = 0,
            fontsize = 12
        ),
        #widget.GenPollText(
        #    update_interval=60,
        #    fmt = ' {} ',
        #    font="UbuntuMono Nerd Font",
        #    fontsize = 14,
        #    func = lambda: subprocess.check_output(home + "/dwmscripts/qtiledisk").decode("utf-8").replace('\n', ''),
        #    foreground = colors[5],
        #    background = colors[1]
        #),         
        #widget.Sep(
        #    linewidth = 1,
        #    padding = 10,
        #    foreground = colors[2],
        #    background = colors[1]
        #),         
        widget.TextBox(
            text = " 🔊",
            foreground = colors[5],
            background = colors[1],
            mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn(myTerm + ' -e pulsemixer')},
            padding = 0,
            fontsize = 12
        ),
        widget.Volume(
            font="UbuntuMono Nerd Font",
            fontsize = 14,
            fmt = '{} ',
            foreground = colors[5],
            background = colors[1],
            padding = 5
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        widget.TextBox(
            text = " 🛠️ ",
            foreground = colors[5],
            background = colors[1],
            mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn('pamac-manager')},
            padding = 0,
            fontsize = 12
        ),
        #widget.GenPollText(
        #    update_interval=1800,
        #    fmt = ' {} ',
        #    font="UbuntuMono Nerd Font",
        #    fontsize = 14,
        #    func = lambda: subprocess.check_output(ome + "/dwmscripts/qtileupdates").decode("utf-8").replace('\n', ''),
        #    foreground = colors[5],
        #    background = colors[1]
        #),
        #widget.Sep(
        #    linewidth = 1,
        #    padding = 10,
        #    foreground = colors[2],
        #    background = colors[1]
        #),
        widget.TextBox(
            text = " 📅 ",
            padding = 0,
            mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn(myTerm + ' -e calcurse')},
            fontsize=12
        ),
        widget.Clock(
            foreground = colors[5],
            background = colors[1],
            font="UbuntuMono Nerd Font",
            fontsize = 14,
            format="%d-%m-%Y %a %H:%M %p"
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
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


# We only have 1 screen on the laptop
def init_screens():
    return [
        Screen(top=bar.Bar(widgets=init_widgets_screen1(), size=26)),
            # Screen(top=bar.Bar(widgets=init_widgets_screen2(), size=26)),
            # Screen(top=bar.Bar(widgets=init_widgets_screen1(), size=26))
            ]

screens = init_screens()


   # Mouse config

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size())
]

dgroups_key_binder = None
dgroups_app_rules = []


main = None

@hook.subscribe.startup
def start_always():
    # Set the cursor to something sane in X
    subprocess.Popen(['xsetroot', '-cursor_name', 'left_ptr'])

@hook.subscribe.client_new
def set_floating(window):
    if (window.window.get_wm_transient_for()
            or window.window.get_wm_type() in floating_types):
        window.floating = True

floating_types = ["notification", "toolbar", "splash", "dialog"]


follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'Arandr'},
    {'wname': 'branchdialog'},
    {'wname': 'Open File'},
    {'wname': 'pinentry'}
    # {'wmclass': 'ssh-askpass'},

],  fullscreen_border_width = 0, border_width = 0)
auto_fullscreen = True

focus_on_window_activation = "focus" # or smart

wmname = "LG3D"
