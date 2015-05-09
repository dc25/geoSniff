/// <reference path="/home/dave/repos/gitnc/DefinitelyTyped/googlemaps/google.maps.d.ts" />

var connectionMarkers = {};
var map;

function initialize() {
  var mapOptions = {
    zoom: 2,
    center: new google.maps.LatLng(20.0, 13.410186)
  };

  map = new google.maps.Map(document.getElementById('map-canvas'),
          mapOptions);
}

function placeMarker_ffi(hash:string, localIp: string, localPort:number, remoteIp:string, remotePort:number, latitude:number, longitude:number) {
    var marker = new google.maps.Marker({
        position: new google.maps.LatLng(latitude, longitude),
        map: map,
        animation: google.maps.Animation.DROP
        });

    var infowindow = new google.maps.InfoWindow({
    content: 
    '<div class="connection-info">                    ' +
    '   <div class="container-fluid">                 ' +
    '      <row>                                      ' +
    '          <div class="col-xs-1">                 ' +
    '              <b>FROM:</b>                       ' +
    '          </div>                                 ' +
    '          <div class="col-xs-11">                ' +
    '              <tt>                               ' +
                       localIp + ' : ' + localPort      +  
    '              </tt>                              ' +
    '          </div>                                 ' +
    '      </row>                                     ' +
    '      <row>                                      ' +
    '          <div class="col-xs-1">                 ' +
    '              <b>TO:</b>                         ' +
    '          </div>                                 ' +
    '          <div class="col-xs-11">                ' +
    '              <tt>                               ' +
                       remoteIp + ' : ' + remotePort    +  
    '              </tt>                              ' +
    '          </div>                                 ' +
    '      </row>                                     ' +
    '   </div>                                        ' +
    '</div>                                           ' 
    });

    google.maps.event.addListener(marker, 'mouseover', function() {
        infowindow.open(map,marker);
    });

    google.maps.event.addListener(marker, 'mouseout', function() {
        infowindow.close();
    });

    connectionMarkers[hash] = marker;
}

function removeMarker_ffi(hash:string) {
    console.log(hash);
    if (connectionMarkers.hasOwnProperty(hash)) {
        connectionMarkers[hash].setMap(null);
        delete connectionMarkers[hash]
    } else {
        console.log(hash, " not found!");
    }
}

google.maps.event.addDomListener(window, 'load', initialize);
