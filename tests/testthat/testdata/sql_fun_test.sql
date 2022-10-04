CREATE OR REPLACE FUNCTION Sum(a int, b int)
RETURNS int AS $$
  BEGIN
/*
  *  My first trivial PL/pgSQL function.
*/
  RETURN a + b;
END;
$$ LANGUAGE plpgsql;
