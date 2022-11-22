-- Prepare data in a temporary table for inserting data_points
--
-- This function prepares data in a temporary table for inserting data_points.
-- It is passed the code_no and while the data_point values are in a temporary
-- table called `tmp_data_points`, with columns `value`, one time-dimension column
-- and at least one non-time-dimension column.
-- The function gets the correct vintage codes for each series and returns a table
-- ready for insertion into the `data_points` table.

CREATE OR REPLACE FUNCTION test_platform.prepare_data_points(p_code_no TEXT)
RETURNS table ( series_id int, level_value varchar, dimension varchar)
AS $$
DECLARE dim_id bigint[];
DECLARE tbl_dimz text[];
BEGIN
-- Get the dimension ids for the non-time dimension
dim_id := array(SELECT id
FROM test_platform.table_dimensions
WHERE table_id = (
        SELECT id
        FROM test_platform.table
        WHERE code = p_code_no)
    AND is_time = FALSE);
-- Replace spaces with periods in the dimension names for these ids
tbl_dimz := array(SELECT
    REPLACE(t.dimension, ' ', '.') as dimension
FROM test_platform.table_dimensions t
WHERE id = any(dim_id)
order by 1);

RETURN QUERY
-- Get the levels for the non-time dimensions for each series
SELECT
    series_levels.series_id,
    series_levels.level_value,
    dimz.dimension
FROM
    test_platform.series_levels
    LEFT JOIN (
        SELECT
            id,
            table_dimensions.dimension
        FROM
            test_platform.table_dimensions
        WHERE
            table_id = (
            SELECT id
            FROM test_platform.table
            WHERE code = p_code_no)
            AND is_time = FALSE) dimz on tab_dim_id = dimz.id;
END;
$$ LANGUAGE plpgsql;






-- tbl_dims_str_w_types < - toString(
--     paste(sprintf('"%s"', tbl_dims $ dimension), "text")
-- ) tbl_dims_str < - toString(paste(sprintf('"%s"', tbl_dims $ dimension))) # series_levels <- dbGetQuery(con,
-- #                             sprintf("SELECT series_id, level_value, j.dimension FROM test_platform.series_levels left join
-- #                                     (SELECT id, dimension FROM test_platform.table_dimensions
-- #                                      where id in (%s)) as j
-- #                                     on tab_dim_id = j.id
-- #                                     where tab_dim_id in (%s) ",
-- #                                     dim_id_str, dim_id_str))
-- series_levels_wide < - dbGetQuery(
--     con,
--     sprintf(
--         "select * from test
--                                     left join
--                                     (select *
--                                     from crosstab(
--                                     'SELECT series_id,  j.dimension, level_value FROM test_platform.series_levels
--                                     left join
--                                     (SELECT id, dimension FROM test_platform.table_dimensions
--                                      where id in (%s)) as j
--                                     on tab_dim_id = j.id
--                                     where tab_dim_id in (%s)
--                                     ORDER BY 1,2',
--                                     'select distinct dimz.dimension from
--                                     (SELECT id, dimension FROM test_platform.table_dimensions
--                                      where id in (%s)) as dimz')
--                                     as t(series_id int, %s )) i using(%s)
--                                     left join
--                                     (select distinct on (series_id)
--                                     id series_id from
--                                     test_platform.vintage
--                                     order by series_id, published) as vinz using (series_id)
--                                     ",
--         dim_id_str,
--         dim_id_str,
--         dim_id_str,
--         tbl_dims_str_w_types,
--         tbl_dims_str
--     )
-- ) head(series_levels_wide)


CREATE OR REPLACE FUNCTION test_platform.f_dynamic_copy(_file    text
                                        , _tbl     text = 'tmp1'
                                        , _delim   text = E'\t'
                                        , _nodelim text = chr(127)) -- see below!
  RETURNS text
 AS
$func$
DECLARE
   row_ct int;
BEGIN
   -- create staging table for 1st row as  single text column
   CREATE TEMP TABLE tmp0(cols text) ON COMMIT DROP;

   -- fetch 1st row
   EXECUTE format($$COPY tmp0 FROM PROGRAM 'gc %I | select -first 1' WITH (DELIMITER %L)$$  -- impossible delimiter
                , _file, _nodelim);

   -- create actual temp table with all columns text
   EXECUTE (
      SELECT format('CREATE TEMP TABLE %I(', _tbl)
          || string_agg(quote_ident(col) || ' text', ',')
          || ')'
      FROM  (SELECT cols FROM tmp0 LIMIT 1) t
           , unnest(string_to_array(t.cols, E'\t')) col
      );

   -- Import data
   EXECUTE format($$COPY %I FROM %L WITH (FORMAT csv, HEADER, NULL '\N', DELIMITER %L)$$
                , _tbl, _file, _delim);

   GET DIAGNOSTICS row_ct = ROW_COUNT;
   RETURN format('Created table %I with %s rows.', _tbl, row_ct);
END
$func$
LANGUAGE plpgsql;
