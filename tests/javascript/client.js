/*******************************************************************************
* Utilities                                                                    *
*******************************************************************************/
var big = "FooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFoo"
function createWebSocket(path, subproto) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8000' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    if (subproto) {
        return new Socket(uri, subproto);
    } else {
        return new Socket(uri);
    }
}


/*******************************************************************************
* Actual tests                                                                 *
*******************************************************************************/

test('demo', function() {
    ok(true, 'Demo test');
});

function randomString(len, charSet) {
    charSet = charSet || 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    // charSet = charSet || 'A';
    var randomString = '';
    for (var i = 0; i < Math.pow(2,len); i++) {
        var randomPoz = Math.floor(Math.random() * charSet.length);
        randomString += charSet.substring(randomPoz,randomPoz+1);
    }
    return randomString;
}

asyncTest('echo-text', function() {
    var ws = createWebSocket('/echo-text');

    // var messages = ['Hi folks', 'Hello there', 'λ±…','Hi folks', 'Hello there', 'λ±…', 'Hi folks', 'Hello there', 'λ±…', big, big, big, big];
    
    var tests = 1;
    var message = randomString(tests);
    ws.onopen = function() {
        ws.send(message);
    };

    ws.onmessage = function(event) {
        var message1 = event.data;
        equal(message, message1);
        tests = tests + 1;
        message = randomString(tests);
        if(tests < 20 ) {
            ws.send(message);
        } else {
            ws.close(4002, "Goodbye");
        }
    };

    ws.onclose = function(event) {
        equal(event.code, 4002);
        equal(event.reason, "Goodbye");
        start();
    };
});

asyncTest('close me', function() {
    var ws = createWebSocket('/close-me');
    ws.onopen = function() {
        ws.send('Close me!');
    };
    ws.onclose = function(event) {
        equal(event.code, 1000);
        equal(event.reason, "Closing");
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
        console.log(event.data);
        console.log(event.data.type);
        ws.close();
    };

    ws.onclose = function(event) {
        equal(event.code, 1000);
        equal(event.reason, "");
        start();
    };
});

asyncTest('subprotocol', function() {
    var ws = createWebSocket("/subprotocol", ["abc", "def"]);

    ws.onopen = function() {
        ws.send("FooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFoo");
    };

    ws.onmessage = function(event) {
        var message = event.data;
        equal(message, "FooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFooFoo");
        ws.close(4711, "Bar");
    };

    ws.onclose = function(event) {
        equal(event.code, 4711);
        equal(event.reason, "Bar");
        start();
    };
});
