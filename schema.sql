CREATE TABLE users (
  user_id SERIAL NOT NULL,
  name text NOT NULL,
  age integer NOT NULL,
  email text NOT NULL,
  registration_date text NOT NULL
);

insert into users (name, age, email, registration_date) values ('Isaac Newton', 372, 'isaac@newton.co.uk', '1683-3-1');
insert into users (name, age, email, registration_date) values ('Albert Einstein', 136, 'ae@mc2.org', '1905-12-1');
insert into users (name, age, email, registration_date) values ('I. Newton', 200, 'isaac@newton.co.uk', '1683-3-1');
insert into users (name, age, email, registration_date) values ('A. Einstein', 50, 'ae@mc2.org', '1905-12-1');
insert into users (name, age, email, registration_date) values ('Newt Newt', 72, 'isaac@newton.co.uk', '1683-3-1');
insert into users (name, age, email, registration_date) values ('Albert', 6, 'ae@mc2.org', '1905-12-1');
insert into users (name, age, email, registration_date) values ('Isaac N.', 18, 'isaac@newton.co.uk', '1683-3-1');
insert into users (name, age, email, registration_date) values ('A. E.', 136, 'ae@mc2.org', '1905-12-1');
