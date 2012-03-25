/*******************************************************************************
* Utilities                                                                    *
*******************************************************************************/

function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8000' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}


/*******************************************************************************
* Actual tests                                                                 *
*******************************************************************************/

module('QUnit');

test('demo', function () {
    ok(true, 'Demo test');
});

module('Hybi00');

asyncTest('echo', function() {
    var ws = createWebSocket('/echo');
    var messages = ['Hi folks', 'Hello there', 'What up'];

    ws.onopen = function() {
        ws.send(messages[0]);
    };

    ws.onmessage = function (event) {
        var message = event.data;
        equal(message, messages[0]);
        messages = messages.slice(1);
        if(messages.length > 0) {
            ws.send(messages[0]);
        } else {
            ws.close();
            start();
        }
    };
});

asyncTest('close me', function () {
    var ws = createWebSocket('/close-me');
    ws.onopen = function() {
        ws.send('Close me!');
    };
    ws.onclose = function() {
        ok(true, 'closed');
        start();
    };
});

asyncTest('concurrent send', function () {
    var ws = createWebSocket('/concurrent-send');
    var expected = [];
    for(var i = 1; i <= 100; i++) {
        expected.push('Herp-a-derp ' + i);
    }

    ws.onmessage = function(event) {
        var msg = event.data;
        var idx = expected.indexOf(msg);
        expected.splice(idx, 1);

        if(expected.length <= 0) {
            ws.close();
            ok(true, 'all received');
            start();
        }
    }
});

module('Hybi10');

asyncTest('ping', function() {
    var ws = createWebSocket('/ping');

    ws.onmessage = function (event) {
        if(event.data == 'OK') {
            ws.close();
            ok(true, 'ping');
            start();
        }
    };
});
