/// <reference path="/home/dave/repos/gitnc/DefinitelyTyped/googlemaps/google.maps.d.ts" />

var berlin = new google.maps.LatLng(52.520816, 13.410186);

var connectionMarkers = {};
var map;

function initialize() {
  var mapOptions = {
    zoom: 2,
    center: berlin
  };

  map = new google.maps.Map(document.getElementById('map-canvas'),
          mapOptions);
}

// Place a marker on the map
function placeMarker_ffi(hash:string, latitude:number, longitude:number) {
    console.log(hash, latitude, longitude);
    var coord = new google.maps.LatLng(latitude, longitude);
    connectionMarkers[hash] = (new google.maps.Marker({
      position: coord,
      map: map,
      animation: google.maps.Animation.DROP
    }));
}

// Place a marker on the map
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
