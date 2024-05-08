#!/usr/bin/env sh
rsync --rsync-path="sudo rsync" -ravz ./   intersect:/root/govtool/tests/test-infrastructure
