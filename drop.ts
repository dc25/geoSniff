/// <reference path="/home/dave/repos/gitnc/DefinitelyTyped/googlemaps/google.maps.d.ts" />

// If you're adding a number of markers, you may want to drop them on the map
// consecutively rather than all at once. This example shows how to use
// window.setTimeout() to space your markers' animation.

var berlin = new google.maps.LatLng(52.520816, 13.410186);

var neighborhoods = [
  new google.maps.LatLng(52.511467, 13.447179),
  new google.maps.LatLng(52.549061, 13.422975),
  new google.maps.LatLng(52.497622, 13.396110),
  new google.maps.LatLng(52.517683, 13.394393)
];

var markers = [];
var map;

function initialize() {
  var mapOptions = {
    zoom: 2,
    center: berlin
  };

  map = new google.maps.Map(document.getElementById('map-canvas'),
          mapOptions);
}

function drop() {
  clearMarkers();
  for (var i = 0; i < neighborhoods.length; i++) {
     window.setTimeout(addMarker, i*200, neighborhoods[i]);
  }
}

function addMarker(position) {
  markers.push(new google.maps.Marker({
    position: position,
    map: map,
    animation: google.maps.Animation.DROP
  }));
}

function clearMarkers() {
  for (var i = 0; i < markers.length; i++) {
    markers[i].setMap(null);
  }
  markers = [];
}

google.maps.event.addDomListener(window, 'load', initialize);
