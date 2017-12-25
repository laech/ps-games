#!/usr/bin/env bash

set -e errexit

cd "$(dirname $0)"

stack build

stack exec ps-game-price-history games.json
