CREATE TABLE IF NOT EXISTS debate (
  uid serial PRIMARY KEY,
  title text UNIQUE NOT NULL,
  imageUrl text NOT NULL,
  subtitle text NOT NULL,
  description text NOT NULL,
  viewCount int NOT NULL,
  opinionCount int NOT NULL,
  bookmarked boolean NOT NULL,
  opined boolean NOT NULL,
  voted boolean NOT NULL
);

CREATE TABLE IF NOT EXISTS opinion (
  uid serial PRIMARY KEY,
  debate int NOT NULL REFERENCES debate(uid),
  author int NOT NULL REFERENCES snap_auth_user(uid),
  description text NOT NULL,
  UNIQUE (uid, debate),
  UNIQUE (author, debate, description)
);

CREATE TABLE IF NOT EXISTS vote (
  uid serial PRIMARY KEY,
  voter int NOT NULL REFERENCES snap_auth_user(uid),
  debate int NOT NULL,
  winner int NOT NULL,
  loser int NOT NULL,
  FOREIGN KEY (debate, winner) REFERENCES opinion(debate, uid),
  FOREIGN KEY (debate, loser) REFERENCES opinion(debate, uid),
  UNIQUE (voter, debate, winner, loser)
);
