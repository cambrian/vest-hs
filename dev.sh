#!/bin/bash
rm -f rabbit.log
/usr/local/bin/redis-cli shutdown > /dev/null 2>&1
/usr/local/bin/redis-server /usr/local/etc/redis.conf > ~/.vest-hs/redis-run.log &

# To completely clear RabbitMQ:
# rabbitmqctl stop_app
# rabbitmqctl reset
# rabbitmqctl shutdown

# NOTE: If you get weird errors, run this manually:
/usr/local/sbin/rabbitmq-server > ~/.vest-hs/rabbit-run.log &

# Enable recent history for RabbitMQ.
/usr/local/sbin/rabbitmq-plugins enable rabbitmq_recent_history_exchange

stack test --fast --file-watch --no-rerun-tests

# Append this for hspec only:
# --test-arguments '--rerun --failure-report=TESTREPORT --rerun-all-on-success'
