CREATE TABLE users (
  user_id SERIAL NOT NULL,
  name text NOT NULL,
  age integer NOT NULL,
  email text NOT NULL,
  registration_date text NOT NULL
);

insert into users (name, age, email, registration_date) values ('Isaac Newton', 372, 'isaac@newton.co.uk', '1683-3-1');
insert into users (name, age, email, registration_date) values ('Albert Einstein', 136, 'ae@mc2.org', '1905-12-1');
