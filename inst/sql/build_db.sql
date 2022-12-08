drop table if exists test_platform.source cascade;
CREATE TABLE test_platform.source
(
    id integer NOT NULL,
    name character varying NOT NULL,
    name_long character varying,
    url character varying,
    PRIMARY KEY (id)
);

INSERT INTO test_platform.source(
  id, name, name_long, url)
VALUES (1, 'SURS', 'Statistčni urad RS', 'https://pxweb.stat.si/SiStat/sl');

drop table if exists test_platform.category cascade;
CREATE TABLE test_platform.category
(
    id integer NOT NULL,
    name character varying NOT NULL,
    source_id integer NOT NULL REFERENCES test_platform.source (id),
    PRIMARY KEY (id, source_id)
);
INSERT INTO test_platform.category (id, name, source_id)
                       VALUES (0, 'SiStat', 1);

drop table if exists test_platform.category_relationship cascade;
CREATE TABLE test_platform.category_relationship
(
    category_id integer NOT NULL,
    parent_id integer NOT NULL,
    source_id integer NOT NULL,
    foreign key (category_id, source_id) references test_platform.category ( id, source_id),
    foreign key (parent_id, source_id) references test_platform.category ( id, source_id),
    PRIMARY KEY (category_id, parent_id, source_id)
);


drop table if exists test_platform."interval" cascade;
CREATE TABLE test_platform."interval"
(
    id character varying NOT NULL,
    name character varying NOT NULL,
    PRIMARY KEY (id)
);

drop table if exists test_platform.unit cascade;
CREATE TABLE test_platform.unit
(
    id int GENERATED ALWAYS AS IDENTITY,
    name character varying NOT NULL,
    PRIMARY KEY (id),
	UNIQUE(name)
);

drop table if exists test_platform."table" cascade;
CREATE TABLE test_platform."table"
(
    id bigint GENERATED ALWAYS AS IDENTITY,
    code character varying UNIQUE NOT NULL,
    name character varying NOT NULL,
	source_id integer NOT NULL REFERENCES test_platform."source" (id),
    url character varying,
    description character varying,
    notes json,
    PRIMARY KEY (id)
);

drop table if exists test_platform.category_table cascade;
CREATE TABLE test_platform.category_table
(
    table_id integer NOT NULL REFERENCES test_platform."table" (id),
    category_id integer NOT NULL,
    source_id integer not null ,
    PRIMARY KEY (table_id, category_id),
    foreign key (category_id, source_id) references test_platform.category ( id, source_id)
);

drop table if exists test_platform.table_dimensions cascade;
CREATE TABLE test_platform.table_dimensions
(
    id bigint GENERATED ALWAYS AS IDENTITY,
    table_id integer NOT NULL REFERENCES test_platform."table" (id),
    dimension character varying NOT NULL,
	  is_time boolean NOT NULL,
    PRIMARY KEY (id),
    UNIQUE (table_id, dimension)
);

drop table if exists test_platform.dimension_levels cascade;
CREATE TABLE test_platform.dimension_levels
(
    tab_dim_id integer NOT NULL REFERENCES test_platform.table_dimensions (id),
    level_value character varying NOT NULL,
    level_text character varying,
    PRIMARY KEY (tab_dim_id, level_value)
);

drop table if exists test_platform.series cascade;
CREATE TABLE test_platform.series
(
    id bigint GENERATED ALWAYS AS IDENTITY,
    table_id integer NOT NULL REFERENCES test_platform."table" (id),
    name_long character varying NOT NULL,
	unit_id integer REFERENCES test_platform.unit (id),
    code character varying NOT NULL,
	interval_id character varying REFERENCES test_platform.interval (id),
    PRIMARY KEY (id),
	unique(table_id, code)
);

drop table if exists test_platform.series_levels cascade;
CREATE TABLE test_platform.series_levels
(
    series_id integer NOT NULL REFERENCES test_platform.series (id),
    tab_dim_id integer NOT NULL,
    level_value character varying NOT NULL,
    PRIMARY KEY (series_id, tab_dim_id),
    FOREIGN KEY (tab_dim_id, level_value) REFERENCES test_platform.dimension_levels (tab_dim_id, level_value)
);

drop table if exists test_platform.vintage cascade;
CREATE TABLE test_platform.vintage
(
    id int GENERATED ALWAYS AS IDENTITY,
    series_id integer NOT NULL  REFERENCES test_platform.series (id),
    published timestamp NOT NULL,
	  UNIQUE (series_id, published),
    PRIMARY KEY (id)
);

drop table if exists test_platform.period cascade;
CREATE TABLE test_platform.period
(
    id character varying NOT NULL,
    interval_id character varying NOT NULL  REFERENCES test_platform."interval" (id),
    PRIMARY KEY (id)
);

drop table if exists test_platform.data_points cascade;
CREATE TABLE test_platform.data_points
(
    vintage_id integer NOT NULL REFERENCES test_platform.vintage (id),
    period_id character varying NOT NULL REFERENCES test_platform.period (id),
    value numeric,
    PRIMARY KEY (vintage_id, period_id)
);

create index ind_vintage_id_period_id on test_platform.data_points
(vintage_id, period_id);

drop table if exists test_platform.flag cascade;
CREATE TABLE test_platform.flag
(
    id character varying NOT NULL,
    name character varying NOT NULL,
    PRIMARY KEY (id)
);

drop table if exists test_platform.flag_datapoint cascade;
CREATE TABLE test_platform.flag_datapoint
(
    vintage_id integer NOT NULL,
    period_id character varying NOT NULL,
    flag_id character varying NOT NULL REFERENCES test_platform.flag (id),
    PRIMARY KEY (vintage_id, period_id, flag_id),
    FOREIGN KEY (vintage_id, period_id) REFERENCES test_platform.data_points (vintage_id, period_id)
);

INSERT INTO test_platform."interval"(
  id, name)
VALUES ('D', 'daily'),
('W', 'weekly'),
('F', 'biweekly'),
('M', 'monthly'),
('B', 'bimonthly'),
('Q', 'quarterly'),
('S', 'semiannually'),
('A', 'annualy');

INSERT INTO test_platform.flag(
  id, name)
VALUES ('M', 'manj zanesljiva ocena'),
('T', 'začasni podatki'),
('Z', 'zaupno'),
('N', 'za objavo premalo zanesljiva ocena'),
('b', 'break in time series'),
('c', 'confidential'),
('d', 'definition differs, see metadata'),
('e', 'estimated'),
('f', 'forecast'),
('n', 'not significant'),
('p', 'provisional'),
('r', 'revised'),
('s', 'eurostat estimate'),
('u', 'low reliability'),
('z', 'not applicable');

INSERT INTO test_platform."unit"(
  name)
VALUES ('1000');

GRANT ALL ON ALL TABLES IN SCHEMA test_platform TO mzaloznik;
