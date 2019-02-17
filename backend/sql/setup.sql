CREATE TABLE subjects (
  uid int PRIMARY KEY,
  name text NOT NULL,
  description text NOT NULL
);

CREATE TABLE users (
  uid int PRIMARY KEY,
  username text UNIQUE NOT NULL
);

CREATE TABLE opinions (
  subject int NOT NULL REFERENCES subjects(uid),
  uid int,
  author int NOT NULL REFERENCES users(uid),
  description text NOT NULL,
  UNIQUE (author, subject, description),
  PRIMARY KEY (subject, uid)
);

CREATE TABLE votes (
  uid int PRIMARY KEY,
  voter int NOT NULL REFERENCES users(uid),
  subject int NOT NULL,
  option1 int NOT NULL,
  option2 int NOT NULL,
  FOREIGN KEY (subject, option1) REFERENCES opinions(subject, uid),
  FOREIGN KEY (subject, option2) REFERENCES opinions(subject, uid),
  UNIQUE (voter, subject, option1, option2)
);
