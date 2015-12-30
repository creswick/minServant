'use strict'

const d3 = require('d3');
const React = require('react');
const ReactFauxDom = require('react-faux-dom');
const Sparkline = require('./Sparkline.jsx');
const server = require('../generated/server.js');

require('./NoteGraph.css');

const extractLength = function(data) {
  return data.map(function(note) {
    return note.content.length;
  });
};

const NoteGraph = React.createClass({
  propTypes: {
    width: React.PropTypes.number,
    height: React.PropTypes.number,
    pollInterval: React.PropTypes.number
  },

  loadNotesFromServer: function() {
    server.getNotes(function(data) {
                      this.setState({data: extractLength(data)});
                    }.bind(this),
                    function(xhr, status, err) {
                      console.error("Could not load notes for graph.", status, err.toString());
                    }.bind(this)
    );
  },

  getInitialState: function() {
    return { data: []
           , pollTimer: null };
  },

  componentDidMount: function() {
    this.loadNotesFromServer();
    const pollTimer = setInterval(this.loadNotesFromServer, this.props.pollInterval);

    this.setState({pollTimer: pollTimer});
  },

  componentWillUnmount: function () {
    clearInterval(this.state.pollTimer);
  },

  render: function () {
    return (
      <Sparkline className='notegraph'
                 width={this.props.width}
                 height={this.props.height}
                 data={this.state.data}
      />
    );
  }
});

module.exports = NoteGraph;
