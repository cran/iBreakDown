function getColors(n, type){
    var temp = ["#8bdcbe", "#f05a71", "#371ea3", "#46bac2", "#ae2c87", "#ffa58c", "#4378bf"];
    var ret = [];

    if (type == "bar") {
      switch(n){
        case 1:
          return ["#46bac2"];
        case 2:
          return ["#46bac2", "#4378bf"];
        case 3:
          return ["#8bdcbe", "#4378bf", "#46bac2"];
        case 4:
          return ["#46bac2", "#371ea3", "#8bdcbe", "#4378bf"];
        case 5:
          return ["#8bdcbe", "#f05a71", "#371ea3", "#46bac2", "#ffa58c"];
        case 6:
          return ["#8bdcbe", "#f05a71", "#371ea3", "#46bac2", "#ae2c87", "#ffa58c"];
        case 7:
          return temp;
        default:
          for (var i = 0; i <= n%7; i++) {
            ret = ret.concat(temp);
          }
          return ret;
      }
    } else if (type == "line") {
      switch(n){
        case 1:
          return ["#46bac2"];
        case 2:
          return ["#8bdcbe", "#4378bf"];
        case 3:
          return ["#8bdcbe", "#f05a71", "#4378bf"];
        case 4:
          return ["#8bdcbe", "#f05a71", "#4378bf", "#ffa58c"];
        case 5:
          return ["#8bdcbe", "#f05a71", "#4378bf", "#ae2c87", "#ffa58c"];
        case 6:
          return ["#8bdcbe", "#f05a71", "#46bac2", "#ae2c87", "#ffa58c", "#4378bf"];
        case 7:
          return temp;
        default:
          for (var j = 0; j <= n%7; j++) {
            ret = ret.concat(temp);
          }
          return ret;
      }
    } else if (type == "point") {
      switch(n){
        default:
          return ["#371ea3", "#46bac2", "#ceced9"];
      }
    } else if (type == "breakDown") {
      switch(n){
        default:
          return ["#8bdcbe", "#f05a71", "#371ea3"];
      }
    }
}

function calculateTextWidth(text) {
  // calculate max width of 11px text array

  var temp = svg.selectAll()
                .data(text)
                .enter();

  var textWidth = [];

  temp.append("text")
      .attr("class", "toRemove")
      .text(function(d) { return d;})
      .style("font-size", "11px")
      .style('font-family', 'Fira Sans, sans-serif')
      .each(function(d,i) {
          var thisWidth = this.getComputedTextLength();
          textWidth.push(thisWidth);
      });

  svg.selectAll('.toRemove').remove();
  temp.remove();

  var maxLength = d3.max(textWidth);

  return maxLength;
}
