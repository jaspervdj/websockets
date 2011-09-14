function addMessage(message) {
    var div = $(document.createElement('div'));
    div.text(message);
    $('#messages').append(div);
}

$(document).ready(function () {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8088';
    var ws = new WebSocket(uri);
    ws.onopen = function() {
        $('#message').submit(function() {
            var message = $('#text').val();
            $('#text').val('');
            ws.send(message);
            return false;
        });
    };

    ws.onmessage = function (event) {
        addMessage(event.data);
    };

    onclose = function() {
    };
});

