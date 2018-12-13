#!/bin/sh

stack exec hindent
stack test --fast --no-rerun-tests
