-- Add new table
--
-- Inserts new table into the `table` table.
--
CREATE OR REPLACE FUNCTION insert_new_table(code TEXT,
                                        name TEXT,
                                        source_id INTEGER,
                                        url TEXT,
                                        notes JSONB,
                                        schema TEXT,
                                        OUT table_id integer)
RETURNS INTEGER AS $$
BEGIN
    EXECUTE FORMAT('INSERT INTO %I.table (code, name, source_id, url, notes) 
    VALUES (code, name, source_id, url, notes)
    RETURNING id INTO table_id;', schema);
END;
$$ LANGUAGE plpgsql;

