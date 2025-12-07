-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.

CREATE TABLE candle (
    symbol_code   VARCHAR NOT NULL,
    timeframe     SMALLINT NOT NULL,
    datetime      TIMESTAMP NOT NULL,
    open          REAL NOT NULL,
    close         REAL NOT NULL,
    high          REAL NOT NULL,
    low           REAL NOT NULL,
    volume        REAL NOT NULL,
    amount        REAL NOT NULL,
    PRIMARY KEY (symbol_code, timeframe, datetime)
);