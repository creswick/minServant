'use strict'

const React = require('react');

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

module.exports = User;
