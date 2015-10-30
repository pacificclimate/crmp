-- http://tech.jonathangardner.net/wiki/PostgreSQL/Materialized_Views#Lazy_Materialized_Views
-- Relies upon each matview having a single integer primary key (not necessarily DEFINED as pkey however...)

CREATE TABLE matview_changes (
  mv_id INTEGER NOT NULL,
  pkey INTEGER NOT NULL
);

-- Add id to matviews table if does not exist
BEGIN;
CREATE SEQUENCE matview_id_seq;
ALTER TABLE matviews ADD COLUMN id INTERGER DEFAULT nextval('matview_id_seq');
COMMIT;

-- Generic queue refresh row function
CREATE OR REPLACE FUNCTION matview_queue_refresh_row(NAME, INTEGER) RETURNS VOID
SECURITY DEFINER LANGUAGE 'plpgsql' AS '
DECLARE
  mv INTEGER;
  test INTEGER;
BEGIN
  SELECT id INTO mv FROM matviews WHERE mv_name = $1;

  SELECT pkey INTO test FROM matview_changes
    WHERE matview_changes.mv_id = mv
    AND matview_changes.pkey = $2;

  IF NOT FOUND THEN
      INSERT INTO matview_changes (mv_id, pkey) VALUES (mv, $2);
  END IF;

  RETURN;
END
';

-- These views have not been converted to lazy views yet

-- This one is a ~4 second query
CREATE OR REPLACE VIEW collapsed_vars_v AS 
  SELECT history_id, array_to_string(array_agg(standard_name || regexp_replace(cell_method, 'time: ', '_', 'g')), ', '::text) AS vars,
         array_to_string(array_agg(display_name), '|') AS display_names
  FROM vars_per_history_mv NATURAL JOIN meta_vars
  GROUP BY history_id;

SELECT create_matview('collapsed_vars_mv', 'collapsed_vars_v');
CREATE INDEX collapsed_vars_idx ON collapsed_vars_mv(history_id);
