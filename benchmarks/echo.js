/* This WebSockets client opens an increasingly larger number of connections to
 * localhost and sends messages on all connections.
 *
 * It is written in JavaScript since benchmarking my own library using my own
 * library might give a skewed perspective.
 *
 * Requires the `ws` npm module, install using:
 *
 *     npm install --user-install ws
 *
 * */
var WebSocket = require('ws');

/* Configuration. */
var websocketPort       = 9160;
var spawnClientInterval = 100;
var nextClientId        = 0;
var messageInterval     = 100;

setInterval(function() {
    var numberOfSentMessages = 0;

    var clientId = nextClientId;
    nextClientId += 1;
    console.log('Client ' + clientId + ': spawning...');

    var sentMessage = undefined;

    var ws = new WebSocket('http://localhost:' + websocketPort + '/echo');

    ws.on('open', function() {
        ws.on('message', function(msg) {
            msg = msg.toString();
            if (msg === sentMessage && numberOfSentMessages % 100 === 0) {
                console.log('Client ' + clientId + ': ' + numberOfSentMessages +
                        ' OK messages');
            }
            if (msg !== sentMessage) {
                console.error('Client ' + clientId + ': unexpected response: ' +
                        'got "' + msg + '", expected: "' + sentMessage + '"');
            }
        });

        setInterval(function() {
            sentMessage = 'Hello ' + Math.floor(Math.random() * 10);
            ws.send(sentMessage, {binary: true, mask: false});
            numberOfSentMessages++;
        }, messageInterval);
    });
}, spawnClientInterval);
