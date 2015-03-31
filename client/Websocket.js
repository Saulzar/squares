

function runMain() {
  var main = Elm.fullscreen(Elm.Main);

  websocket = new WebSocket("ws://localhost:9160");

  websocket.onopen = function(evt) {
    main.ports.outgoing.subscribe(sendMessage);
    
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



