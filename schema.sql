CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE users (
  user_id SERIAL PRIMARY KEY,
  name text NOT NULL UNIQUE,
  password text NOT NULL,
  registration_date TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now() -- UTCTime in Haskell (works with fromfield)
);

CREATE TABLE notes (
  note_id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  content TEXT NOT NULL,
  note_date TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(), -- UTCTime in Haskell (works with fromfield)
  user_id INTEGER NOT NULL,

  FOREIGN KEY(user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

CREATE TABLE sessions (
  session_id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id INTEGER NOT NULL,

  -- Delete a session when / if the user is deleted:
  FOREIGN KEY(user_id) REFERENCES users(user_id) ON DELETE CASCADE
);


insert into users (user_id, name, password) values (1, 'test', 'test');
insert into users (user_id, name, password) values (2, 'tester', 'test');
