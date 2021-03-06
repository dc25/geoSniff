/// <reference path="/home/dave/repos/gitnc/DefinitelyTyped/googlemaps/google.maps.d.ts" />

// A & B are required by haste for callbacks.  See: 
// https://github.com/valderman/haste-compiler/blob/master/doc/js-externals.txt
// for details.
var A:any;
var B:any;

// For debugging.
function consoleLog_ffi(msg:string) {
    console.log(msg);
}

var startClient:any;

function setStartClientCallback_ffi(cb) {
    startClient=cb;
}

function connectToServer() {
    var serverAddress:string = (<HTMLInputElement>document.getElementById('server-address')).value;
    B(A(startClient, [[0,serverAddress], 0]));
    document.getElementById("globe-icon").className += " green";
}

var connectionMarkers = {};
var map;

var mapOptions = {
    zoom: 2,
    center: new google.maps.LatLng(20.0, 0.0)
};

function worldView() {
  map.setCenter(mapOptions.center); 
  map.setZoom(mapOptions.zoom); 
}

function initialize() {
  map = new google.maps.Map(document.getElementById('map-canvas'), mapOptions);
}

function placeMarker_ffi(hash:string, localIp: string, localPort:number, remoteIp:string, remotePort:number, latitude:number, longitude:number) {
    var marker = new google.maps.Marker({
        position: new google.maps.LatLng(latitude, longitude),
        map: map,
        animation: google.maps.Animation.DROP
        });

    marker.setIcon('http://maps.google.com/mapfiles/ms/icons/green-dot.png')

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

    google.maps.event.addListener(marker, 'click', function() {
        map.setCenter(marker.getPosition());
        map.setZoom(15); 
    });

    connectionMarkers[hash] = marker;
}

var autoClear : boolean = false;
var removalQueue : string[] = [];

function toggleAutoClear(element)
{
    autoClear = (element.checked);
    if (autoClear) {
        clearExpired();
    }
}

function removeMarkerImmediate(hash:string) {
    if (connectionMarkers.hasOwnProperty(hash)) {
        connectionMarkers[hash].setMap(null);
        delete connectionMarkers[hash]
    } else {
        console.log(hash, " not found!");
    }
}


function removeMarker_ffi(hash:string) {
    if (autoClear) {
        removeMarkerImmediate(hash);
    } else {
        if (connectionMarkers.hasOwnProperty(hash)) {
            removalQueue.push(hash);
            var marker = connectionMarkers[hash]
            marker.setIcon('http://maps.google.com/mapfiles/ms/icons/red-dot.png')
        }
    }
}

function clearExpired() {
    var removalQueueLength = removalQueue.length;
    for (var i = 0; i < removalQueueLength ; i++) {
        removeMarkerImmediate(removalQueue[i]);
    }
    removalQueue.length = 0;
}

google.maps.event.addDomListener(window, 'load', initialize);
