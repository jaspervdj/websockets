#!/usr/bin/bash
set -o errexit -o pipefail

# Finding the right python
if command -v python2.7; then
    AUTOBAHN_PYTHON="python2.7"
else
    AUTOBAHN_PYTHON="python"
fi

# Note that this script will be executed from the project root.
AUTOBAHN_ENV="$HOME/pyenvs/autobahn"
echo "Setting up virtualenv..."
if [[ ! -e "$AUTOBAHN_ENV" ]]; then
    virtualenv --python="$AUTOBAHN_PYTHON" "$AUTOBAHN_ENV"
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
wstest -m fuzzingclient

echo "Killing websockets server..."
kill "$WEBSOCKETS_AUTOBAHN_PID"
