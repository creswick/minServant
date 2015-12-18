'use strict'

const $ = require('jquery');
const React = require('react');
const User = require('./User.jsx');

const UserList = React.createClass({
  getInitialState: function() {
    return {data: []};
  },

  componentDidMount: function() {
    $.ajax({
      url: '/users', //this.props.url,
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
        <h2>Min Servant: List of Users</h2>
          <ul>
             {userNodes}
          </ul>
      </div>
    )
  }
});

module.exports = UserList;
