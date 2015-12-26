'use strict'

const $ = require('jquery');

// bootstrap does not export anything:
require("bootstrap-webpack");

const React = require('react');
const { render } = require('react-dom');
const { Router, Route, Link } = require('react-router');

const App = require('./components/App.jsx');
const UserList = require('./components/UserList.jsx');
const UserGraph = require('./components/UserGraph.jsx');
const Docs = require('./components/Docs.jsx');

// Helper to pass static props to a React Component when used with React Router.
const wrapComponent = function(Component, props) {
  return React.createClass({
    render: function() {
      return React.createElement(Component, props);
    }
  });
};

render((
  <Router>
    <Route path="/" component={App}>
        <Route path="users"
               component={wrapComponent(UserList,
                                        { url: '/users'
                                        , pollInterval: 2000 })} />
      <Route path="usergraph"
             component={wrapComponent(UserGraph,
                                      { url: '/users'
                                      , width: 400
                                      , height: 400
                                      , pollInterval: 2000
                                      })} />
      <Route path="docs" component={Docs} />
    </Route>
  </Router>
), document.getElementById('content'));
