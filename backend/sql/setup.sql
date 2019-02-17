CREATE TABLE debates (
  uid int PRIMARY KEY,
  name text NOT NULL,
  description text NOT NULL
);

CREATE TABLE users (
  uid int PRIMARY KEY,
  username text UNIQUE NOT NULL
);

CREATE TABLE opinions (
  debate int NOT NULL REFERENCES debates(uid),
  uid int,
  author int NOT NULL REFERENCES users(uid),
  description text NOT NULL,
  UNIQUE (author, debate, description),
  PRIMARY KEY (debate, uid)
);

CREATE TABLE votes (
  uid int PRIMARY KEY,
  voter int NOT NULL REFERENCES users(uid),
  debate int NOT NULL,
  winner int NOT NULL,
  loser int NOT NULL,
  FOREIGN KEY (debate, winner) REFERENCES opinions(debate, uid),
  FOREIGN KEY (debate, loser) REFERENCES opinions(debate, uid),
  UNIQUE (voter, debate, winner, loser)
);
