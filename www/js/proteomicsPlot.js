javascript = function(el, x){
  el.on('plotly_click', function(data) {
    // initialise
    colors = [];
    
    var base_color = "rgb(128, 128, 128)";

    activeNavTab = document.getElementById("navbarPage").getElementsByClassName("active")[0].childNodes[1].getAttribute("data-value");
    
    var plot_name_prefix = "bar_";
    var plot_name_postfix = "_history"
    var instrument = data.points[0].data.name.match(/<br \/>.*/)[0];
    
    if (activeNavTab === "Test") {
      plot_name_prefix = "mstest_bar_";
    }
    instrument = instrument.match(/(?:<br \/>|.*)/g)[1];
    if (instrument === "database") {
      instrument = data.points[0].data.key[0];
    } else if ((instrument === "comments") || (instrument === "threshold")) {
      instrument = null;
    }
    
    if (instrument !== null) {
      selected_instrument = plot_name_prefix.concat(instrument, plot_name_postfix);
    
      // set color of the bars to default
      for (var i = 0; i < document.getElementById(selected_instrument).data.length; i += 1) {
      // for (var i = 0; i < document.querySelector( ('[id*='.concat(selected_instrument)).concat(']') ) ) {
        colors = [];

        // database is grey
        if (i === 0) {

          for (var j = 0; j < document.getElementById(selected_instrument).data[i].x.length; j += 1) {
          // for (var j = 0; j < document.querySelector( ('[id*='.concat(selected_instrument)).concat(']') ).data[i].x.length; j += 1) {
            colors.push("rgb(128, 128, 128)");
          }
          if (i === data.points[0].curveNumber) {
            colors[data.points[0].pointNumber] = '#00cc00'; //'#ff9933';
          }
          Plotly.restyle(el, {'marker':{color: colors}},
            [i]
          );

        } else {
          if (document.getElementById(selected_instrument).data[i].key.includes("input")) {
          // if (document.querySelector( ('[id*='.concat(selected_instrument)).concat(']') ).data[i].key.includes("input")) {
            for (var j = 0; j < document.getElementById(selected_instrument).data[i].x.length; j += 1) {
            // for (var j = 0; j < document.querySelector( ('[id*='.concat(selected_instrument)).concat(']') ).data[i].x.length; j += 1) {
              colors.push("rgb(230, 0, 0)");
            } 
            if (i === data.points[0].curveNumber) {
              colors[data.points[0].pointNumber] = '#00cc00'; //'#ff9933';
            }
            Plotly.restyle(el, {'marker':{color: colors}},
              [i]
            );
          }
  //       console.log(document.getElementById(selected_instrument).data[i].key.includes("input")); //{ //anything that is not the database is red
        } 

      }
    }
      
  });

  
}
