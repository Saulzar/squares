

function runMain() {
  var main = Elm.fullscreen(Elm.Multi);

  websocket = new WebSocket("ws://localhost:9160");

  websocket.onopen = function(evt) {
    main.ports.outgoing.subscribe(sendMessage);
    main.ports.connected.send(true);

    
    function sendMessage(message) {
      websocket.send(message);
    }  
 
  };

  websocket.onclose = function(evt) {
    main.ports.errors.send("Connection closed");
  };

  websocket.onmessage = function(evt) {  
    main.ports.incoming.send( evt.data );
  };





}



