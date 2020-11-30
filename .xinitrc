xrdb ~/.Xresources

# Configure Tap to Click
# order: RT, RB, LT, LB, F1, F2, F3.
#                        ^^^^^^^^^^ one finger tap, two finger tap, ...
# http://manpages.ubuntu.com/manpages/xenial/man4/synaptics.4.html
xinput set-prop "Synaptics TM3336-003" "Synaptics Tap Action" 0 0 0 0 1 0 0

# invert two finger scroll
xinput set-prop "Synaptics TM3336-003" "Synaptics Scrolling Distance" -48 -48

# Deactivate touchpad while typing
syndaemon -i 1 -d -t -k

# Launch fg daemons in tmux shell
tmux new-session -d -s '*daemons*' -n emacs emacs --fg-daemon
tmux new-window -t '*daemons*' -n redshift redshift
tmux new-window -t '*daemons*' -n alttab sh -c 'alttab -w 1 -d 2'
tmux new-window -t '*daemons*' -n xcompmgr sh -c 'xcompmgr -c -f -r 10 -D 3'
tmux new-window -t '*daemons*' -n flashfocus sh -c 'flashfocus'

exec xmonad
