'use strict'

const $ = require('jquery');
const React = require('react');
const { render } = require('react-dom');
const { Router, Route, Link } = require('react-router');

const App = require('./components/App.js');
const UserList = require('./components/UserList.js');
const Docs = require('./components/Docs.js');

render((
  <Router>
    <Route path="/" component={App}>
      <Route path="users" component={UserList} />
      <Route path="docs" component={Docs} />
    </Route>
  </Router>
), document.body);
