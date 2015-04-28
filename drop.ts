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

    var contentString = '<div id="content">'+
        '<div id="siteNotice">'+
        '</div>'+
        '<h1 id="firstHeading" class="firstHeading">Uluru</h1>'+
        '<div id="bodyContent">'+
        '<p><b>Uluru</b>, also referred to as <b>Ayers Rock</b>, is a large ' +
        'sandstone rock formation in the southern part of the '+
        'Northern Territory, central Australia. It lies 335&#160;km (208&#160;mi) '+
        'south west of the nearest large town, Alice Springs; 450&#160;km '+
        '(280&#160;mi) by road. Kata Tjuta and Uluru are the two major '+
        'features of the Uluru - Kata Tjuta National Park. Uluru is '+
        'sacred to the Pitjantjatjara and Yankunytjatjara, the '+
        'Aboriginal people of the area. It has many springs, waterholes, '+
        'rock caves and ancient paintings. Uluru is listed as a World '+
        'Heritage Site.</p>'+
        '<p>Attribution: Uluru, <a href="https://en.wikipedia.org/w/index.php?title=Uluru&oldid=297882194">'+
        'https://en.wikipedia.org/w/index.php?title=Uluru</a> '+
        '(last visited June 22, 2009).</p>'+
        '</div>'+
        '</div>';
  

    var infowindow = new google.maps.InfoWindow({
        content: contentString
    });

    var coord = new google.maps.LatLng(latitude, longitude);

    var marker = new google.maps.Marker({
        position: coord,
        map: map,
        animation: google.maps.Animation.DROP
        });

    google.maps.event.addListener(marker, 'mouseover', function() {
      infowindow.open(map,marker);
    });

    google.maps.event.addListener(marker, 'mouseout', function() {
        infowindow.close();
    });


    connectionMarkers[hash] = marker;
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
