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
      /* Set a key here to help react differentiate between
         different user entries.  This also gets rid of the "Each
         child in an array or iterator should have a unique "key"
         prop." warning.
         See: https://fb.me/react-warning-keys for more details.
       */
      return (
          <User key={user.user_id} user={user} />
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
