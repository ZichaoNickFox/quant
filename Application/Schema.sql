CREATE TYPE NODE_TYPE AS ENUM ('folder', 'file');
CREATE TYPE CELL_TYPE AS ENUM ('raw', 'image', 'backtest');
CREATE TYPE CELL_OWNER_TYPE AS ENUM ('note', 'strategy');
CREATE TYPE SYMBOL_TYPE AS ENUM ('stock', 'index', 'etf', 'future', 'option', 'fund');
CREATE TYPE TREE_OWNER_TYPE AS ENUM ('note', 'strategy');
CREATE TABLE symbol (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    code TEXT NOT NULL,
    symbol_type SYMBOL_TYPE NOT NULL,
    name TEXT NOT NULL,
    list_date TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    delist_date TIMESTAMP WITHOUT TIME ZONE,
    UNIQUE(code, symbol_type)
);
CREATE TABLE data_freshness (
    dataset_key TEXT PRIMARY KEY NOT NULL,
    last_refreshed_at TIMESTAMP WITHOUT TIME ZONE DEFAULT '1970-01-01 00:00:00' NOT NULL,
    ttl_seconds INTEGER DEFAULT 604800 NOT NULL
);
CREATE TABLE tree (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    owner_type TREE_OWNER_TYPE NOT NULL,
    owner_id UUID NOT NULL,
    node_type NODE_TYPE NOT NULL,
    name TEXT NOT NULL,
    parent_tree_id UUID,
    node_order INT NOT NULL
);
CREATE TABLE basket (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL UNIQUE
);
CREATE TABLE basket_symbol (
    basket_id UUID NOT NULL,
    symbol_id UUID NOT NULL,
    PRIMARY KEY (basket_id, symbol_id)
);
CREATE TABLE candle (
    symbol_id UUID NOT NULL,
    timeframe SMALLINT NOT NULL,
    datetime TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    open REAL NOT NULL,
    "close" REAL NOT NULL,
    high REAL NOT NULL,
    low REAL NOT NULL,
    volume REAL NOT NULL,
    amount REAL NOT NULL,
    PRIMARY KEY(symbol_id, timeframe, datetime)
);
CREATE TABLE cell (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    cell_type CELL_TYPE NOT NULL,
    owner_type CELL_OWNER_TYPE NOT NULL,
    owner_id UUID NOT NULL,
    cell_order INT NOT NULL,
    content TEXT,
    image_url TEXT,
    backtest JSONB,
    UNIQUE(owner_type, owner_id, cell_order)
);
CREATE TABLE strategy (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
);
CREATE TABLE note (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
);
