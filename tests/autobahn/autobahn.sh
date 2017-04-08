#!/usr/bin/bash
set -o nounset -o errexit -o pipefail

# Note that this script will be executed from the project root.

echo "Setting up virtualenv..."
if ! -e "$HOME/pyenv_autobahn"; then
    virtualenv "$HOME/pyenv_autobahn"
    pip install autobahntestsuite
fi
source "$HOME/pyenv_autobahn/bin/activate"

echo "Launching websockets server in background..."
stack exec websockets-autobahn &
WEBSOCKETS_AUTOBAHN_PID="$!"

echo "Getting config..."
cp tests/autobahn/fuzzingclient.json .

echo "Running autobahn testsuite..."
wstest -m fuzingclient

echo "Killing websockets server..."
kill "$WEBSOCKETS_AUTOBAHN_PID"
