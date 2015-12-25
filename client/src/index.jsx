'use strict'

const $ = require('jquery');

// bootstrap does not export anything:
require("bootstrap-webpack");

const React = require('react');
const { render } = require('react-dom');
const { Router, Route, Link } = require('react-router');

const App = require('./components/App.jsx');
const UserList = require('./components/UserList.jsx');
const Docs = require('./components/Docs.jsx');

render((
  <Router>
    <Route path="/" component={App}>
      <Route path="users" component={UserList} />
      <Route path="docs" component={Docs} />
    </Route>
  </Router>
), document.getElementById('content'));
