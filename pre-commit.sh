#!/bin/sh

stack exec hindent
export ___TEST_MODE_DO_NOT_SET=true && stack test --fast --no-rerun-tests