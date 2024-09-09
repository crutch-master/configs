#!/usr/bin/env bash
tac < /etc/protocols | tr -s ' ' | cut -d ' ' -f 2,1 | head -n 5
