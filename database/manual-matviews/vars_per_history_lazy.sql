-- ~2 minute query -> ~8seconds
CREATE OR REPLACE VIEW vars_per_history_v AS
  SELECT DISTINCT history_id, vars_id FROM obs_raw;

SELECT create_matview('vars_per_history_mv', 'vars_per_history_v');
CREATE INDEX var_hist_idx ON vars_per_history_mv(history_id, vars_id);

-- Incremental matview for this kinda sucks.  No real single primary key, but hist_id + var combo.
-- Result is that need to update all vars for each history_id on triggers, even if some vars don't change

CREATE OR REPLACE FUNCTION vars_per_history_mv_refresh_row(vars_per_history_mv.history_id%TYPE) RETURNS VOID
SECURITY DEFINER
LANGUAGE 'plpgsql' AS '
BEGIN
  DELETE FROM vars_per_history_mv WHERE history_id = $1;
  INSERT INTO vars_per_history_mv SELECT * FROM vars_per_history_v WHERE history_id = $1;
  RETURN;
END
';

-- vars_per_history_mv_refresh()
CREATE OR REPLACE FUNCTION vars_per_history_mv_refresh() RETURNS VOID
SECURITY DEFINER LANGUAGE 'plpgsql' AS '
DECLARE
  mv INTEGER;
BEGIN
  SELECT id INTO mv FROM matviews WHERE mv_name = ''vars_per_history_mv'';

  PERFORM vars_per_history_mv_refresh_row(pkey)
    FROM matview_changes
    WHERE mv_id = mv;

  DELETE FROM matview_changes
    WHERE mv_id = mv; 

  UPDATE matviews
    SET last_refresh = now()
    WHERE mv_name = ''vars_per_history_mv'';
END
';

/* BEGIN vars_per_history_mv TRIGGERS. Depends on obs_raw and meta_history.  Need triggers on ins/update/del for both */

 -- obs_raw update trigger
CREATE FUNCTION vars_per_history_mv_obs_raw_update() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  IF OLD.obs_raw_id = NEW.obs_raw_id THEN
    PERFORM matview_queue_refresh_row(''vars_per_history_mv'', NEW.history_id); 
  ELSE
    PERFORM matview_queue_refresh_row(''vars_per_history_mv'', NEW.history_id);
    PERFORM matview_queue_refresh_row(''vars_per_history_mv'', OLD.history_id);
  END IF;
  RETURN NULL;
END
';
CREATE TRIGGER vars_per_history_mv_update AFTER UPDATE ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE vars_per_history_mv_obs_raw_update();

-- obs_raw delete trigger
CREATE FUNCTION vars_per_history_mv_obs_raw_delete() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''vars_per_history_mv'', OLD.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER vars_per_history_mv_delete AFTER DELETE ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE vars_per_history_mv_obs_raw_delete();

-- obs_raw insert trigger
CREATE FUNCTION vars_per_history_mv_obs_raw_insert() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''vars_per_history_mv'', NEW.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER vars_per_history_mv_insert AFTER INSERT ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE vars_per_history_mv_obs_raw_insert();
