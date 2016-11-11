#/bin/bash


# $1 -> Node Number
function node_up_down {
    echo "start"
    erl -sname rm$1 -setcookie rm -detached -noinput -noshell
    sleep 5
    ps aux | grep beam | grep rm$1 | awk '{print $2}' | xargs kill
    sleep 1
}






while true; do
    for i in `seq 1 10`; do
        node_up_down $i &
    done
    sleep 15
    echo "Looping"
done