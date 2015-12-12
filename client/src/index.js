'use strict'

const $ = require(jquery);
const React = require('react');
const { render } = require('react-dom');
// First we import some components...
const { Router, Route, Link } = require('react-router');

// Then we delete a bunch of code from App and
// add some <Link> elements...
const App = React.createClass({
  getInitialState: function() {
    return {data: []};
  },

  componentDidMount: function() {
    $.ajax({
      url: this.props.url,
      dataType: 'json',
      cache: false,
      success: function(data) {
        this.setState({data: data});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error(this.props.url, status, err.toString());
      }.bind(this)
    });
  },

  render: function() {
    var userNodes = this.state.data.map(function(user) {
      return (
          <User user={user} />
      );
    });
    return (
      <div>
        <h1>Min Servant: List of Users</h1>
          <ul>
             {userNodes}
          </ul>
      </div>
    )
  }
});

const User = React.createClass ({
  render() {
    return (
      <div>
        <h2>{this.props.user.name}</h2>
        <ul>
        <li>age: {this.props.user.age}</li>
        <li>email: {this.props.user.email}</li>
        <li>registered: {this.props.user.registration_date}</li>
        </ul>
      </div>
    )
  }
});

var data = [{"email":"isaac@newton.co.uk","registration_date":"1683-3-1","age":372,"name":"Isaac Newton","user_id":1},{"email":"ae@mc2.org","registration_date":"1905-12-1","age":136,"name":"Albert Einstein","user_id":2}]

// Finally, we render a <Router> with some <Route>s.
// It does all the fancy routing stuff for us.
render((
    <App url="/users" />
), document.body);
