/// <reference path="/home/dave/repos/gitnc/DefinitelyTyped/googlemaps/google.maps.d.ts" />

var connectionMarkers = {};
var map;

function initialize() {
  var mapOptions = {
    zoom: 2,
    center: new google.maps.LatLng(52.520816, 13.410186)
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
        content: '<h1>' + localIp + ' : ' + localPort + ' -> ' + remoteIp + ' : ' + remotePort + '</h1>' 
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
