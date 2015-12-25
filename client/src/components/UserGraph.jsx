'use strict'

const d3 = require('d3');
const React = require('react');
const ReactFauxDom = require('react-faux-dom');
const Sparkline = require('./Sparkline.jsx');

require('./UserGraph.css');


const extractAge = function(data) {
  return data.map(function(user) {
    return user.age;
  });
};
// const sampledata = [85, 66, 71, 10, 5, 16, 71, 1, 16, 24, 54, 85, 37, 36, 43, 67, 63, 23, 96, 53, 25]

const UserGraph = React.createClass({
  propTypes: {
    width: React.PropTypes.number,
    height: React.PropTypes.number,
    url: React.PropTypes.string
  },

  getInitialState: function() {
    return {data: []};
  },

  componentDidMount: function() {
    $.ajax({
      url: this.props.url,
      dataType: 'json',
      cache: false,
      success: function(data) {
        this.setState({data: extractAge(data)});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error(this.props.url, status, err.toString());
      }.bind(this)
    });
  },

  render: function () {
    console.log("UserGraph data:");
    console.log(this.state.data);
    return (
      <Sparkline className='usergraph'
                 width={this.props.width}
                 height={this.props.height}
                 data={this.state.data}
      />
    );
  }
});

module.exports = UserGraph;
