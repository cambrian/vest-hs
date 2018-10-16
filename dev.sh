#!/bin/bash
rm -f rabbit.log
# /usr/local/sbin/rabbitmqctl shutdown
# NOTE: If you get weird errors, run this manually:
/usr/local/sbin/rabbitmq-server > ~/.vest-hs/rabbit-run.log &
stack test --fast --file-watch --no-rerun-tests

# Append this for hspec only
# --test-arguments '--rerun --failure-report=TESTREPORT --rerun-all-on-success'
