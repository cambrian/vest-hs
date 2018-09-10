#!/bin/bash
# stack hoogle -- generate --local

HOOGLE_PORT=19000
PID_USING_HOOGLE_PORT=$(lsof -i :$HOOGLE_PORT | awk 'END {print $2}')
if [ ! -z "$PID_USING_HOOGLE_PORT" ]
then
  kill -INT $PID_USING_HOOGLE_PORT
  sleep 1
fi
stack hoogle -- server --local --port=$HOOGLE_PORT > /dev/null &

rm -f rabbit.log
# /usr/local/sbin/rabbitmqctl shutdown
# NOTE: If you get weird errors, run this manually:
/usr/local/sbin/rabbitmq-server > rabbit.log &
stack test --fast --haddock-deps --file-watch
