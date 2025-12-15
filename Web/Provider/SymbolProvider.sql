INSERT INTO symbol (code, name, symbol_type, list_date, delist_date)
VALUES (?, ?, ?, ?, ?)
ON CONFLICT (code, symbol_type) DO NOTHING;