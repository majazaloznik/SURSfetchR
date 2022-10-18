-- prva verzija dela, druga ne. razlika je, da je select v vrsticah 15-21 iz prve verzije
-- v drugi "kao" assignan v spremenljivko `dim_id` ki je potem uporabljena v vrstici 44.
-- Ampak druga funkcija ne dela, dobim ` ERROR:  syntax error at or near "dim_id"`

CREATE OR REPLACE FUNCTION test_platform.prepare_data_points1(p_code_no TEXT) 
RETURNS table (dimensions text)
AS $$ 
DECLARE dim_id table_dimensions.id%TYPE;
BEGIN 
-- Replace spaces with periods in the dimension names for these ids
RETURN QUERY SELECT
    REPLACE(t.dimension, ' ', '.') as dimension
FROM test_platform.table_dimensions t
WHERE id IN (
    SELECT id -- Get the dimension ids for the non-time dimension
    FROM test_platform.table_dimensions
    WHERE table_id = (
        SELECT id
        FROM test_platform.table
        WHERE code = p_code_no)
    AND is_time = FALSE)
order by 1;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test_platform.prepare_data_points2(p_code_no TEXT) 
RETURNS table (dimensions text)
AS $$ 
DECLARE dim_id bigint[];
BEGIN 
-- Get the dimension ids for the non-time dimension
SELECT array(id) INTO dim_id
FROM test_platform.table_dimensions
WHERE table_id = (
        SELECT id
        FROM test_platform.table
        WHERE code = p_code_no)
    AND is_time = FALSE;
-- Replace spaces with periods in the dimension names for these ids
RETURN QUERY SELECT
    REPLACE(t.dimension, ' ', '.') as dimension
    FROM test_platform.table_dimensions t
    WHERE id = any(dim_id)
    order by 1;
END;
$$ LANGUAGE plpgsql;