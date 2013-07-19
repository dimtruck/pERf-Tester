#! /bin/bash

start(){
  nohup nodejs /home/hERmes/test_responder/server.js > /dev/null 2>&1&
  echo "started";
  exit 0
}

stop(){
  killall nodejs
  echo "stopped"
}

case $1 in start)
  start
  ;;
  stop)
  stop
  ;;
  *)

  echo $ "usage $0 {start | stop}"
  exit 1
esac
exit 0
