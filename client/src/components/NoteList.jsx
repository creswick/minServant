'use strict'

const $ = require('jquery');
const React = require('react');
const Note = require('./Note.jsx');
const AddNoteForm = require('./AddNoteForm.jsx');
const server = require('../generated/server.js');

const NoteList = React.createClass({
  propTypes: {
    pollInterval: React.PropTypes.number
  },

  getInitialState: function() {
    return { data: []
           , pollTimer: null };
  },

  loadNotesFromServer: function() {
    server.getNotes(function(data) {
                      console.log("Loaded notes");
                      this.setState({data: data});
                    }.bind(this),
                    function(xhr, status, err) {
                      console.error("Could not load notes", status, err.toString());
                    }.bind(this));
  },

  componentDidMount: function() {
    this.loadNotesFromServer();
    const pollTimer = setInterval(this.loadNotesFromServer, this.props.pollInterval);

    this.setState({pollTimer: pollTimer});
  },

  componentWillUnmount: function () {
    clearInterval(this.state.pollTimer);
  },

  onNoteSubmit: function(newNote) {
    const notes = this.state.data;

    // Optomistically update the UI:
    newNote.note_id = Date.now();
    newNote.note_date = ""+Date.now();

    const newnotes = notes.concat(newNote);
    this.setState({data: newnotes});

    console.log("newnote: "+JSON.stringify(newNote));

    // POST to the server:
    server.postAddnote(newNote,
                function(data) {
                  this.setState({data: data});
                }.bind(this),
                function(xhr, status, err) {
                  // roll-back the optomistic update on failure:
                  this.setState({data: notes});
                  console.error('/addnote', status, err.toString());
                }.bind(this));
  },

  render: function() {
    var noteNodes = this.state.data.map(function(note) {
      /* Set a key here to help react differentiate between
         different note entries.  This also gets rid of the "Each
         child in an array or iterator should have a unique "key"
         prop." warning.
         See: https://fb.me/react-warning-keys for more details.
       */
      return (
          <Note key={note.note_id} note={note} />
      );
    });
    return (
      <div>
        <h2>Min Servant: List of Notes</h2>
          <AddNoteForm onNoteSubmit={this.onNoteSubmit} />
          <ul>
             {noteNodes}
          </ul>
      </div>
    )
  }
});

module.exports = NoteList;
