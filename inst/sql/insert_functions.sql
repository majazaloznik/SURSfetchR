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
                                            source_id INTEGER,
                                            OUT count INTEGER)
AS $$
BEGIN
    INSERT INTO test_platform.category (id, name, source_id)
    VALUES (id, name, source_id)
    ON CONFLICT DO NOTHING;
    GET DIAGNOSTICS count = ROW_COUNT;
END;
$$ LANGUAGE plpgsql;


-- Add new category-relationship
--
-- Insert new row into the `category-relationship` table.
--
CREATE OR REPLACE FUNCTION test_platform.insert_new_category_relationship(id INTEGER,
                                            parent_id INTEGER,
                                            source_id INTEGER,
                                            OUT count INTEGER)
AS $$
BEGIN
    INSERT INTO test_platform.category_relationship (category_id, parent_id, source_id)
    VALUES (id, parent_id, source_id)
    ON CONFLICT DO NOTHING;
    GET DIAGNOSTICS count = ROW_COUNT;
END;
$$ LANGUAGE plpgsql;


-- Add new category-table row   
--
-- Insert new row into the `category-table` table.
--
CREATE OR REPLACE FUNCTION test_platform.insert_new_category_table(category_id INTEGER,
                                            table_id INTEGER,
                                            source_id INTEGER,
                                            OUT count INTEGER)
AS $$
BEGIN
    INSERT INTO test_platform.category_table (category_id, table_id, source_id)
    VALUES (category_id, table_id, source_id)
    ON CONFLICT DO NOTHING;
    GET DIAGNOSTICS count = ROW_COUNT;
END;
$$ LANGUAGE plpgsql;

-- Add new table_dimensions row
--
-- Insert new row into the `table_dimensions` table.
--
CREATE OR REPLACE FUNCTION test_platform.insert_new_table_dimensions(table_id INTEGER,
                                            dimension TEXT,
                                            is_time BOOLEAN,
                                            OUT count INTEGER)
AS $$
BEGIN
    INSERT INTO test_platform.table_dimensions(table_id, dimension, is_time)
    VALUES (table_id, dimension, is_time)
    ON CONFLICT DO NOTHING;
    GET DIAGNOSTICS count = ROW_COUNT;
END;
$$ LANGUAGE plpgsql;

-- Add new dimension_levels row
--
-- Insert new row into the `dimension_levels` table.
--
CREATE OR REPLACE FUNCTION test_platform.insert_new_dimension_levels(tab_dim_id INTEGER,
                                            level_value TEXT,
                                            level_text TEXT,
                                            OUT count INTEGER)
AS $$
BEGIN
    INSERT INTO test_platform.dimension_levels(tab_dim_id, level_value, level_text)
    VALUES (tab_dim_id, level_value, level_text)
    ON CONFLICT DO NOTHING;
    GET DIAGNOSTICS count = ROW_COUNT;
END;
$$ LANGUAGE plpgsql;
