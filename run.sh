#!/usr/bin/env bash

set -e errexit

cd "$(dirname $0)"

stack run -- -- games.json
