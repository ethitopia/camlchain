CREATE TABLE accounts (
  name TEXT NOT NULL,
  balance INTEGER NOT NULL,
  public_key TEXT NOT NULL,
  private_key TEXT NOT NULL
);
INSERT INTO accounts (name, balance,public_key,private_key)
VALUES ('Alice', 1000,'12312', '2131');
INSERT INTO accounts (name, balance,public_key,private_key)
VALUES ('Bob', 1500,'12431','1231');
INSERT INTO accounts (name, balance,public_key,private_key)
VALUES ('Charlie', 500,'test','5345');
INSERT INTO accounts (name, balance,public_key, private_key)
VALUES ('test_account', 500,'test2','23413');
