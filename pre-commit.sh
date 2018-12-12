#!/bin/sh

stack exec hindent
export VEST_SWALLOW_LOGS=true && stack test --fast --no-rerun-tests
