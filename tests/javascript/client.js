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

test('demo', function() {
    ok(true, 'Demo test');
});

asyncTest('echo-text', function() {
    var ws = createWebSocket('/echo-text');
    var messages = ['Hi folks', 'Hello there', 'λ±…'];

    ws.onopen = function() {
        ws.send(messages[0]);
    };

    ws.onmessage = function(event) {
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

asyncTest('close me', function() {
    var ws = createWebSocket('/close-me');
    ws.onopen = function() {
        ws.send('Close me!');
    };
    ws.onclose = function() {
        ok(true, 'closed');
        start();
    };
});

asyncTest('ping', function() {
    var ws = createWebSocket('/ping');

    ws.onmessage = function(event) {
        if(event.data == 'OK') {
            ws.close();
            ok(true, 'ping');
            start();
        }
    };
});

asyncTest('blob', function() {
    var ws = createWebSocket('/echo');

    ws.onopen = function() {
        ws.binaryType = 'blob';
        var b = new Blob(['Hello world.'], {"type": "text/plain"});
        ws.send(b);
    };

    ws.onmessage = function(event){
        console.log(event.data)
        console.log(event.data.type)
    };
});
