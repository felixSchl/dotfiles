xrdb ~/.Xresources

xinput set-prop "Synaptics TM3336-003" "Synaptics Tap Action" 0
xinput set-prop "Synaptics TM3336-003" "Synaptics Scrolling Distance" -48 -48
# xinput set-prop "Synaptics TM3336-003" "Synaptics Two-Finger Scrolling" 0 0

synclient PalmDetect=1

exec xmonad
