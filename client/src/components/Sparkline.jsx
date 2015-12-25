'use strict';

/* Example of usning React-faux-dom to embed D3 visualizations in a
 * React component.
 *
 * Taken from: http://oli.me.uk/2015/09/09/d3-within-react-the-right-way/
 */

const d3 = require('d3');
const React = require('react');
const ReactFauxDOM = require('react-faux-dom');

const Sparkline = React.createClass({
  propTypes: {
    width: React.PropTypes.number,
    height: React.PropTypes.number,
    data: React.PropTypes.array,
    interpolation: React.PropTypes.oneOfType([
      React.PropTypes.string,
      React.PropTypes.function
    ])
  },

  render: function () {
    const {width, height, data, interpolation} = this.props;

    console.log("Sparkline data");
    console.log(data);
    const el = d3.select(ReactFauxDOM.createElement('svg'))
                 .attr(this.props)
                 .attr('data', null);

    const x = d3.scale.linear()
                .range([0, width])
                .domain(d3.extent(data, (d, i) => i));

    const y = d3.scale.linear()
                .range([height, 0])
                .domain(d3.extent(data, (d) => d));

    const line = d3.svg.line()
                   .x((d, i) => x(i))
                   .y((d) => y(d))
                   .interpolate(interpolation);

    el.append('path')
                   .datum(data)
                   .attr({
                     key: 'sparkline',
                     className: 'sparkline',
                     d: line
                   });

    return el.node().toReact();
  }
});

module.exports = Sparkline;
