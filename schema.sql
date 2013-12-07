CREATE TABLE deposits (
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	fn TEXT NOT NULL,
	email TEXT NOT NULL,
	tel TEXT NOT NULL,
	ripple TEXT NOT NULL,
	amount REAL NOT NULL,
	complete INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE verifications (
	item_id INTEGER NOT NULL,
	item_table TEXT NOT NULL,
	verification_type TEXT NOT NULL,
	notes TEXT
);
