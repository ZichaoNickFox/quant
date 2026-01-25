-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TYPE "NODE_TYPE" AS ENUM ('folder', 'file');
CREATE TYPE "CELL_TYPE" AS ENUM ('raw', 'image', 'backtest');
CREATE TYPE "SYMBOL_TYPE" AS ENUM ('stock', 'index', 'etf', 'future', 'option', 'fund');
CREATE TABLE symbol (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    code TEXT NOT NULL,
    symbol_type SYMBOL_TYPE NOT NULL,
    name TEXT NOT NULL,
    list_date TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    delist_date TIMESTAMP WITHOUT TIME ZONE,
    UNIQUE(code, symbol_type)
);
CREATE TABLE basket_tree (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    node_type NODE_TYPE NOT NULL,
    name TEXT NOT NULL,
    parent_tree_id UUID,
    node_order INT NOT NULL
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
    node_order INT NOT NULL
);
CREATE TABLE strategy_cell (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    cell_id UUID,
    cell_order INT NOT NULL
);
CREATE TABLE note (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL
);
CREATE TABLE note_cell (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    cell_id UUID,
    cell_order INT NOT NULL
);
CREATE TABLE update_symbol_job (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    symbol_type SYMBOL_TYPE NOT NULL
);
