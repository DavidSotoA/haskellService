CREATE TABLE restaurant (
    id      SERIAL PRIMARY KEY,
    name        VARCHAR(40),
    city        VARCHAR(40),
    address VARCHAR(100),
    phone       INTEGER
);

CREATE TABLE menu (
    id      SERIAL PRIMARY KEY,
    name        VARCHAR(40),
    description     VARCHAR(40),
    price       INTEGER,
    restaurant  INTEGER NOT NULL references restaurant(id)
);

INSERT INTO restaurant (name, city, address, phone) values('miChuzo','Medellin','calle falsa 123',23435365);

INSERT INTO menu (name, description, price, restaurant) values('frijoles','ñam ñam',15000,1);
