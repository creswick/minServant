'use strict';

const React = require('react');

const AddNoteForm = React.createClass({
  getInitialState: function() {
    return {title: '', content: ''};
  },

  handleTitleChange: function(e) {
    this.setState({title: e.target.value});
  },

  handleContentChange: function(e) {
    this.setState({content: e.target.value});
  },

  handleSubmit: function(e) {
    e.preventDefault();
    var title = this.state.title.trim();
    var content = this.state.content.trim();
    if (!title || !content ) {
      return;
    }
    this.props.onNoteSubmit({'title': title, 'content': content});
    this.setState(this.getInitialState());
  },

  render: function() {
    return (
      <form className="addNoteForm"  onSubmit={this.handleSubmit} >
          <input type="text"
                 placeholder="Note title"
                 value={this.state.title}
                 onChange={this.handleTitleChange} />
          <input type="text"
                 placeholder="Content"
                 value={this.state.content}
                 onChange={this.handleContentChange} />

          <input type="submit" value="Post"/>
      </form>
    );
  }
});

module.exports = AddNoteForm;
