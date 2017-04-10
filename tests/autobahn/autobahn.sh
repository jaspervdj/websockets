#!/usr/bin/bash
set -o errexit -o pipefail

# Finding the right python
AUTOBAHN_PYTHON="python2.7"

# Note that this script will be executed from the project root.
AUTOBAHN_ENV="$HOME/.virtualenvs/autobahn"
echo "Setting up virtualenv..."
if [[ ! -e "$AUTOBAHN_ENV" ]]; then
    virtualenv --python="$AUTOBAHN_PYTHON" "$AUTOBAHN_ENV"
    source "$AUTOBAHN_ENV/bin/activate"
    pip install 'autobahn>=0.18'
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

echo "Producing report..."
python tests/autobahn/mini-report.py reports/servers/index.json
