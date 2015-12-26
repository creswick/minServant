'use strict';

const React = require('react');

const AddUserForm = React.createClass({
  getInitialState: function() {
    return {name: '', email: '', age: ''};
  },

  handleNameChange: function(e) {
    this.setState({name: e.target.value});
  },

  handleAgeChange: function(e) {
    const num = parseInt(e.target.value);
    if ( !isNaN(num)) {
      this.setState({age: num});
    }
  },

  handleEmailChange: function(e) {
    this.setState({email: e.target.value});
  },

  handleSubmit: function(e) {
    e.preventDefault();
    var name = this.state.name.trim();
    var age = this.state.age;
    var email = this.state.email.trim();
    if (!name || !age || !email) {
      return;
    }
    this.props.onUserSubmit({'name': name, 'age': age, 'email': email});
    this.setState(this.getInitialState());
  },

  render: function() {
    return (
      <form className="addUserForm"  onSubmit={this.handleSubmit} >
          <input type="text"
                 placeholder="User name"
                 value={this.state.name}
                 onChange={this.handleNameChange} />
          <input type="text"
                 placeholder="Age"
                 value={this.state.age}
                 onChange={this.handleAgeChange} />
          <input type="text"
                 placeholder="email address"
                 value={this.state.email}
                 onChange={this.handleEmailChange} />

          <input type="submit" value="Post"/>
      </form>
    );
  }
});

module.exports = AddUserForm;
