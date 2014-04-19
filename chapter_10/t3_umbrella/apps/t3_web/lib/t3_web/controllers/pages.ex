defmodule T3Web.Controllers.Pages do
  use Phoenix.Controller

  def index(conn) do
    html conn, """
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
        <title>Websocket client</title>
        <script src="//cdnjs.cloudflare.com/ajax/libs/jquery/1.11.0/jquery.min.js"></script>
        <script type="text/javascript">
          
          var websocket;
          $(document).ready(init);
          
          function init() {
              if(!("WebSocket" in window)){  
                  $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
                  $("#navigation").hide();  
              } else {
                  $('#status').append('<p><span style="color: green;">websockets are supported </span></p>');
                  // connect();
              };
                  $("#connected").hide();   
                  $("#content").hide();   
          };

          function connect()
          {
              wsHost = 'ws://localhost:4000/ws/';
              wsHost += $("#server").val()
              websocket = new WebSocket(wsHost);
              showScreen('<b>Connecting to: ' +  wsHost + '</b>'); 
              websocket.onopen = function(evt) { onOpen(evt) }; 
              websocket.onclose = function(evt) { onClose(evt) }; 
              websocket.onmessage = function(evt) { onMessage(evt) }; 
              websocket.onerror = function(evt) { onError(evt) }; 
          };  
          
          function disconnect() {
              websocket.close();
          }; 

          function toggle_connection(){
              connect();

              if(websocket.readyState == websocket.OPEN){
                  disconnect();
              } else {
                  connect();
              };
          };

          function sendTxt() {
              if(websocket.readyState == websocket.OPEN){
                  txt = $("#send_txt").val();
                  websocket.send(txt);
                  showScreen('sending: ' + txt); 
              } else {
                  showScreen('websocket is not connected'); 
              };
          };

          function onOpen(evt) { 
              showScreen('<span style="color: green;">CONNECTED </span>'); 
              $("#connected").fadeIn('slow');
              $("#content").fadeIn('slow');
          };  

          function onClose(evt) { 
              showScreen('<span style="color: red;">DISCONNECTED </span>');
          };  

          function onMessage(evt) { 
              showScreen('<span style="color: blue;">RESPONSE: ' + evt.data+ '</span>'); 
          };  

          function onError(evt) {
              showScreen('<span style="color: red;">ERROR: ' + evt.data+ '</span>');
          };

          function showScreen(txt) { 
              $('#output').prepend('<p>' + txt + '</p>');
          };

          function clearScreen() 
          { 
              $('#output').html("");
          };
        </script>
      </head>

      <body>
        <div id="header">
          <h1>T3 WebSocket Client</h1>
          <div id="status"></div>
        </div>

        <div id="navigation">

          <p id="connecting">
      <input type='text' id="server" value="" placeholder="Enter game id"></input>
      <button type="button" onclick="toggle_connection()">connection</button>
          </p>
          <div id="connected">        
      <p>
        <input type='text' id="send_txt" value=></input>
        <button type="button" onclick="sendTxt();">send</button>
      </p>
          </div>

          <div id="content">            
      <button id="clear" onclick="clearScreen()" >Clear text</button>
      <div id="output"></div>
          </div>

        </div>
      </body>
    </html> 
    """
  end

end
