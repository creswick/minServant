'use strict'

const $ = require('jquery');
const React = require('react');
const marked = require('marked');

const Docs = React.createClass ({
  getInitialState: function() {
    return { docText: 'Loading...'};
  },

  componentDidMount: function() {
    $.ajax({
      url: '/docs', // this.props.url -- should set a default.
      cache: false,
      success: function(data) {
        this.setState({docText: data});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error('/docs', status, err.toString());
      }.bind(this)
    });
  },

  rawMarkup: function() {
    var rawMarkup = marked(this.state.docText, {sanitize: true});
    return { __html: rawMarkup };
  },

  render: function() {
    return (
      <div>
        <h2>Min Servant Demo Documentation</h2>
        <span dangerouslySetInnerHTML={this.rawMarkup()} />
      </div>
    )
  }
});

module.exports = Docs;
