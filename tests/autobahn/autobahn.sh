#!/usr/bin/bash
set -o errexit -o pipefail

# Note that this script will be executed from the project root.
AUTOBAHN_ENV="$HOME/pyenvs/autobahn"
echo "Setting up virtualenv..."
if [[ ! -e "$AUTOBAHN_ENV" ]]; then
    virtualenv "$AUTOBAHN_ENV"
    source "$AUTOBAHN_ENV/bin/activate"
    pip install autobahntestsuite
else
    source "$AUTOBAHN_ENV/bin/activate"
fi

echo "Launching websockets server in background..."
stack exec websockets-autobahn &
WEBSOCKETS_AUTOBAHN_PID="$!"

echo "Getting config..."
cp tests/autobahn/fuzzingclient.json .

echo "Running autobahn testsuite..."
wstest -m fuzingclient

echo "Killing websockets server..."
kill "$WEBSOCKETS_AUTOBAHN_PID"
