#!/usr/bin/env bash
tar -cf archive.tar $(find . -name "*.$1" -type f | tr '\n' ' ')
