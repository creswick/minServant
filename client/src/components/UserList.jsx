'use strict'

const $ = require('jquery');
const React = require('react');
const User = require('./User.jsx');
const AddUserForm = require('./AddUserForm.jsx');
const server = require('../generated/server.js');

const UserList = React.createClass({
  propTypes: {
    url: React.PropTypes.string,
    pollInterval: React.PropTypes.number
  },

  getInitialState: function() {
    return { data: []
           , pollTimer: null };
  },

  loadUsersFromServer: function() {
    server.getUsers(function(data) {
                      this.setState({data: data});
                    }.bind(this),
                    function(xhr, status, err) {
                      console.error(this.props.url, status, err.toString());
                    }.bind(this));
  },

  componentDidMount: function() {
    this.loadUsersFromServer();
    const pollTimer = setInterval(this.loadUsersFromServer, this.props.pollInterval);

    this.setState({pollTimer: pollTimer});
  },

  componentWillUnmount: function () {
    clearInterval(this.state.pollTimer);
  },

  onUserSubmit: function(newUser) {
    const users = this.state.data;

    // Optomistically update the UI:
    newUser.user_id = Date.now();
    newUser.registration_date = ""+Date.now();

    const newusers = users.concat(newUser);
    this.setState({data: newusers});

    console.log("newuser: "+JSON.stringify(newUser));

    // POST to the server:
    server.postAdduser(newUser,
                function(data) {
                  this.setState({data: data});
                }.bind(this),
                function(xhr, status, err) {
                  // roll-back the optomistic update on failure:
                  this.setState({data: users});
                  console.error('/adduser', status, err.toString());
                }.bind(this));
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
          <AddUserForm onUserSubmit={this.onUserSubmit} />
      </div>
    )
  }
});

module.exports = UserList;
