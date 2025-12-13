-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TYPE NODE_TYPE AS ENUM ('folder', 'file');
CREATE TYPE CELL_TYPE AS ENUM ('raw', 'image', 'backtest');
CREATE TYPE SYMBOL_TYPE AS ENUM ('stock', 'etf', 'future', 'option', 'fund');
CREATE TABLE symbol (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    code TEXT NOT NULL,
    name TEXT NOT NULL,
    symbol_type SYMBOL_TYPE NOT NULL,
    list_date TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    delist_date TIMESTAMP WITHOUT TIME ZONE,
    UNIQUE(code, symbol_type)
);
CREATE TABLE basket_tree (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    node_type NODE_TYPE NOT NULL,
    name TEXT NOT NULL,
    parent_tree_id UUID,
    "position" INT NOT NULL
);
CREATE TABLE candle (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    symbol_id UUID,
    timeframe SMALLINT NOT NULL,
    datetime TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    open REAL NOT NULL,
    "close" REAL NOT NULL,
    high REAL NOT NULL,
    low REAL NOT NULL,
    volume REAL NOT NULL,
    amount REAL NOT NULL
);
CREATE TABLE cell (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    cell_type CELL_TYPE NOT NULL,
    content TEXT,
    image_url TEXT,
    backtest JSONB
);
CREATE TABLE strategy (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
);
CREATE TABLE strategy_tree (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    strategy_id UUID,
    node_type NODE_TYPE NOT NULL,
    parent_tree_id UUID,
    "position" INT NOT NULL
);
CREATE TABLE strategy_cell (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    cell_id UUID,
    "position" INT DEFAULT 0 NOT NULL
);
CREATE TABLE note (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
);
CREATE TABLE note_cell (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    cell_id UUID,
    "position" INT DEFAULT 0 NOT NULL
);
