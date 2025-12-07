CREATE TABLE candle_table (
    symbol_code CHARACTER VARYING NOT NULL,
    timeframe SMALLINT NOT NULL,
    datetime TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    open REAL NOT NULL,
    "close" REAL NOT NULL,
    high REAL NOT NULL,
    low REAL NOT NULL,
    volume REAL NOT NULL,
    amount REAL NOT NULL,
    PRIMARY KEY(symbol_code, timeframe, datetime)
);
