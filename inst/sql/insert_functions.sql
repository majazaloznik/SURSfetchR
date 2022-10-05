-- Add new table
--
-- Inserts new table into the `table` table.
--
CREATE OR REPLACE FUNCTION test_platform.insert_new_table(code TEXT,
                                        name TEXT,
                                        source_id INTEGER,
                                        url TEXT,
                                        notes JSONB,
                                        OUT bool BOOLEAN)
AS $$
BEGIN
    INSERT INTO test_platform.table (code, name, source_id, url, notes)
    VALUES (code, name, source_id, url, notes)
    ON CONFLICT DO NOTHING;
    bool := FOUND;
END;
$$ LANGUAGE plpgsql;



-- Add new category
--
-- Insert new category into the `category` table.
--
CREATE OR REPLACE FUNCTION test_platform.insert_new_category(id INTEGER,
                                            name TEXT,
                                            source_id INTEGER)
RETURNS INTEGER AS $$
DECLARE
count INTEGER;
BEGIN
    INSERT INTO test_platform.category (id, name, source_id)
    VALUES (id, name, source_id)
    ON CONFLICT DO NOTHING;
    GET DIAGNOSTICS count = ROW_COUNT;
  return count;
END;
$$ LANGUAGE plpgsql;


create or replace function test_platform.sum(int, int)
returns int as $$
declare
res int;
begin
res := $1 + $2;
return res;
end;
$$ LANGUAGE plpgsql;
