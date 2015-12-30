'use strict'

const React = require('react');

const Note = React.createClass ({
  render() {
    return (
      <div>
        <h2>{this.props.note.title}</h2>
        <ul>
          <li>Content: {this.props.note.content}</li>
          <li>posted: {this.props.note.note_date}</li>
        </ul>
      </div>
    )
  }
});

module.exports = Note;
