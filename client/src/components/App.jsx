'use strict';

const React = require('react');
const { Router, Route, Link } = require('react-router');

const App = React.createClass({

  render: function() {
    return (
      <div>
          <h1>App</h1>
          <ul>
              <li><Link to="/notes">Notes</Link></li>
              <li><Link to="/notegraph">Note Graph</Link></li>
              <li><Link to="/docs">API Docs</Link></li>
          </ul>

          {this.props.children}
      </div>
    )}
});

module.exports = App;
