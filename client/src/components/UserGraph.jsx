'use strict'

const d3 = require('d3');
const React = require('react');
const ReactFauxDom = require('react-faux-dom');
const Sparkline = require('./Sparkline.jsx');
const server = require('../generated/server.js');

require('./UserGraph.css');

const extractAge = function(data) {
  return data.map(function(user) {
    return user.age;
  });
};

const UserGraph = React.createClass({
  propTypes: {
    width: React.PropTypes.number,
    height: React.PropTypes.number,
    url: React.PropTypes.string,
    pollInterval: React.PropTypes.number
  },

  loadUsersFromServer: function() {
    server.getUsers(function(data) {
                      this.setState({data: extractAge(data)});
                    }.bind(this),
                    function(xhr, status, err) {
                      console.error(this.props.url, status, err.toString());
                    }.bind(this)
    );
  },

  getInitialState: function() {
    return { data: []
           , pollTimer: null };
  },

  componentDidMount: function() {
    this.loadUsersFromServer();
    const pollTimer = setInterval(this.loadUsersFromServer, this.props.pollInterval);

    this.setState({pollTimer: pollTimer});
  },

  componentWillUnmount: function () {
    clearInterval(this.state.pollTimer);
  },

  render: function () {
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
