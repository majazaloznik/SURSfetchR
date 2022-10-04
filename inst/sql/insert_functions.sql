-- Add new table
--
-- Inserts new table into the `table` table.
--
CREATE OR REPLACE FUNCTION insert_new_table(code TEXT,
                                        name TEXT,
                                        source_id INTEGER,
                                        url TEXT,
                                        notes JSONB,
                                        OUT table_id INTEGER)
RETURNS INTEGER AS $$
BEGIN
    INSERT INTO test_platform.table (code, name, source_id, url, notes)
    VALUES (code, name, source_id, url, notes)
    RETURNING id   INTO table_id;
END;
$$ LANGUAGE plpgsql;
