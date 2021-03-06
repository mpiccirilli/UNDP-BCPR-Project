// main JS file

//tooltip
d3.helper = {};
d3.helper.tooltip = function(){
  var tooltipDiv;
  var bodyNode = d3.select('body').node();
  var attrs = {};
  var text = '';
  var styles = {};

  function tooltip(selection){

    selection.on('mouseover.tooltip', function(pD, pI){
      var name, value;
                // Clean up lost tooltips
                d3.select('body').selectAll('div.tooltip').remove();
                // Append tooltip
                tooltipDiv = d3.select('body').append('div');
                tooltipDiv.attr(attrs);
                tooltipDiv.style(styles);
                var absoluteMousePos = d3.mouse(bodyNode);
                tooltipDiv.style({
                  left: (absoluteMousePos[0] + 10)+'px',
                  top: (absoluteMousePos[1] - 15)+'px',
                  position: 'absolute',
                  'z-index': 1001
                });
                // Add text using the accessor function, Crop text arbitrarily
                tooltipDiv.style('width', function(d, i){ return (text(pD, pI).length > 80) ? '300px' : null; })
                .html(function(d, i){return text(pD, pI);});
              })
    .on('mousemove.tooltip', function(pD, pI){
                // Move tooltip
                var absoluteMousePos = d3.mouse(bodyNode);
                tooltipDiv.style({
                  left: (absoluteMousePos[0] + 10)+'px',
                  top: (absoluteMousePos[1] - 15)+'px'
                });
                // Keep updating the text, it could change according to position
                tooltipDiv.html(function(d, i){ return text(pD, pI); });
              })
    .on('mouseout.tooltip', function(pD, pI){
                // Remove tooltip
                tooltipDiv.remove();
              });

  }

  tooltip.attr = function(_x){
    if (!arguments.length) return attrs;
    attrs = _x;
    return this;
  };

  tooltip.style = function(_x){
    if (!arguments.length) return styles;
    styles = _x;
    return this;
  };

  tooltip.text = function(_x){
    if (!arguments.length) return text;
    text = d3.functor(_x);
    return this;
  };

  return tooltip;
};
// end of tooltip

var color = d3.scale.threshold()
.domain([0.0,0.2,0.4,0.6,0.8,1.0,1.26, 1.4,1.8,2.0,2.5,3.5,5.5,6.5])
.range(["#fff","rgb(57,49,104)","rgb(75,65,138)","rgb(94,81,173)","rgb(110,98,181)","rgb(142,133,198)","rgb(174,168,214)"  ,  "rgb(176,198,133)", "rgb(153,181,98)","rgb(128,156,73)", "rgb(142,173,81)","rgb(113,138,65)","rgb(85,104,49)","rgb(56,69,33)"]); // dark purple to dark green 

// ratios

//ratio data
var ratio = {};
var gold = {};
var country = {};
d3.csv("ratios_with_iso.csv", function(d) {
  for (var i = 0; i < d.length; i++) {
    ratio[d[i].iso] = +d[i].ratio;
    gold[d[i].iso] = +d[i].avg_goldstein;
    country[d[i].country_code] = d[i].iso;
  };
});

var m0,
o0;
var drag = d3.behavior.drag()
.on("dragstart", function() {
  var proj = projection.rotate();
  m0 = [d3.event.sourceEvent.pageX, d3.event.sourceEvent.pageY];
  o0 = [-proj[0],-proj[1]];
})
.on("drag", function() {
  if (m0) {
    var m1 = [d3.event.sourceEvent.pageX, d3.event.sourceEvent.pageY],
    o1 = [o0[0] + (m0[0] - m1[0]) / 4, o0[1] + (m1[1] - m0[1]) / 4];
    projection.rotate([-o1[0], -o1[1]]);
  }
    // Update the map
    path = d3.geo.path().projection(projection);
    d3.selectAll("path").attr("d", path);
  });

// map 
d3.json("world-topo.json", function(error, world) {
  var countries = topojson.feature(world, world.objects.countries).features,
  neighbors = topojson.neighbors(world.objects.countries.geometries);

  svg.selectAll(".country")
  .data(countries)
  .enter().insert("path", ".graticule")
  .attr("class", "country")
  .attr("class", function(d) { return d.properties.name; })
  .attr("id", function(d) { return "id" + d.id; })
  .attr("d", path)
  .style("fill", function(d) { return color(ratio[+d.id]); })
  .call(d3.helper.tooltip()
    .style({ color: '#111', 'font-family': 'Helvetica', 'background': 'rgba(220,220,220,0.8)'})
    .text(function(d){ 
      if (ratio[d.id]===undefined) { console.log("undefined ratio"); }
      else{ return d.properties.name + " " + (gold[d.id]).toFixed(2); };
    }))
  .on('mouseover', function(d){ 
    if (ratio[d.id]===undefined) { console.log("undefined ratio"); } 
    else{ d3.select(this).style({opacity: '0.6'}); };
  })
  .on('mouseout', function(d){ d3.select(this).style({opacity: '1.0'}); });

  svg.insert("path", ".graticule")
  .datum(topojson.mesh(world, world.objects.countries, function(a, b) { return a !== b; }))
  .attr("class", "boundary")
  .attr("d", path);

  d3.select(self.frameElement).style("height", height + "px");

}); 
