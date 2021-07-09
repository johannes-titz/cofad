setxkbmap -layout de
fuser -k 1234/tcp
Rscript -e "shiny::runApp(cofad::run_app(), port = 1234)" &
sleep 3
xdotool exec firefox
sleep 2
xdotool type 'localhost:1234'
xdotool key "Return"
sleep 3
# start furr test ----
xdotool mousemove 67 240 click 1
sleep 2
xdotool key Control_L+l
xdotool type '/home/jt/programming/cofad/data-raw/furr_p4.csv'
sleep 1
xdotool key "Return"
sleep 4
# works better than manual commands
xmacroplay "$Display" < macrotest_furr
xdotool key Control_L+s
sleep 0.5
xdotool key Shift_L+End
xdotool type '/home/jt/programming/cofad/tests/testthat/furr.html'
sleep 0.5
# two times, if file already exists
xdotool key "Return"
sleep 0.5
xdotool key "Return"
sleep 1

# start rosenthal tbl 53 test ----
xdotool mousemove 67 240 click 1
sleep 2
xdotool key Control_L+l
xdotool type '/home/jt/programming/cofad/data-raw/sedlmeier_p537.csv'
xdotool key "Return"
sleep  2
# works better than manual commands
xmacroplay "$Display" < macrotest_sedlmeier537
xdotool key Control_L+s
sleep 0.5
xdotool key Shift_L+End
xdotool type '/home/jt/programming/cofad/tests/testthat/sedlmeier537.html'
sleep 0.5
# two times, if file already exists
xdotool key "Return"
sleep 0.5
xdotool key "Return"
sleep 1

# start test ----
xdotool mousemove 67 240 click 1
sleep 2
xdotool key Control_L+l
xdotool type '/home/jt/programming/cofad/data-raw/rosenthal_tbl53.csv'
xdotool key "Return"
sleep  2
# works better than manual commands
xmacroplay "$Display" < macrotest_rosenthal2
xdotool key Control_L+s
sleep 0.5
xdotool key Shift_L+End
xdotool type '/home/jt/programming/cofad/tests/testthat/rosenthal_tbl53.html'
sleep 0.5
# two times, if file already exists
xdotool key "Return"
sleep 0.5
xdotool key "Return"
sleep 0.5
xdotool key Super_L+q
