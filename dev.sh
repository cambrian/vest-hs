#!/bin/bash
rm -f rabbit.log
redis-cli shutdown > /dev/null 2>&1
redis-server /usr/local/etc/redis.conf > ~/.vest-hs/redis-run.log &

# Enable recent history for RabbitMQ
rabbitmq-plugins enable rabbitmq_recent_history_exchange

# /usr/local/sbin/rabbitmqctl shutdown
# NOTE: If you get weird errors, run this manually:
/usr/local/sbin/rabbitmq-server > ~/.vest-hs/rabbit-run.log &
stack test --fast --file-watch --no-rerun-tests

# Append this for hspec only
# --test-arguments '--rerun --failure-report=TESTREPORT --rerun-all-on-success'
