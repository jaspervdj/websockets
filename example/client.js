function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':9160' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

function addMessage(message) {
    var div = $(document.createElement('div'));
    div.text(message);
    $('#messages').append(div);
}

$(document).ready(function () {
    $('#join-form').submit(function () {
        $('#warnings').html('');
        var user = $('#user').val();
        var ws = createWebSocket('/');

        ws.onopen = function() {
            ws.send('Hi! I am ' + user);
        }

        ws.onmessage = function(event) {
            if(event.data == 'Welcome!') {
                $('#join').hide();
                $('#chat').show();

                ws.onmessage = function(event) {
                    addMessage(event.data);
                };

                $('#message-form').submit(function () {
                    var text = $('#text').val();
                    ws.send(text);
                    $('#text').val('');
                    return false;
                });

            } else {
                $('#warnings').append(event.data);
                ws.close();
            }
        }

        return false;
    });
});

