CREATE TABLE pending_transactions (
  sender_id TEXT,
  recipient_id TEXT NOT NULL,
  sender_private_key TEXT NOT NULL,
  amount int NOT NULL
);