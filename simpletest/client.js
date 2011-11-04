
function createWebSocket(path) {
    // var host = window.location.hostname;
    // if(host == '') host = 'localhost';
    host = 'localhost'
    var uri = 'ws://' + host + ':8001' + path;

    // var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    var Socket = WebSocket;
    return new Socket(uri);
}

console.log("creating web socket");

var ws = createWebSocket('/simple');
console.log("createWebSocket returned");
if (typeof ws == "undefined") {
    console.log("ERROR: couldn't create websocket")
}

ws.onopen = function() {
    console.log("onopen");
    ws.send("Hello World!");
    console.log("sent");
}

ws.onmessage = function(event) {
    var message = event.data;
    console.log("received:");
    console.log(message);
}

ws.onclose = function() {
    console.log("close");
}

