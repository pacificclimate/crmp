-- ~23 minute query (1376450ms) -> 27 seconds after one hourly update
CREATE OR REPLACE VIEW obs_count_per_month_history_v AS
  SELECT count(*) AS count, date_trunc('month'::text, obs_raw.obs_time) AS date_trunc, obs_raw.history_id
  FROM obs_raw
  GROUP BY date_trunc('month'::text, obs_raw.obs_time), obs_raw.history_id;

SELECT create_matview('obs_count_per_month_history_mv', 'obs_count_per_month_history_v');
CREATE INDEX obs_count_per_month_history_idx ON obs_count_per_month_history_mv(date_trunc, history_id);

-- Lazy update obs_count_per_month_history_mv functions
CREATE OR REPLACE FUNCTION obs_count_per_month_history_mv_refresh_row(obs_count_per_month_history_mv.history_id%TYPE) RETURNS VOID
SECURITY DEFINER
LANGUAGE 'plpgsql' AS '
BEGIN
  DELETE FROM obs_count_per_month_history_mv WHERE history_id = $1;
  INSERT INTO obs_count_per_month_history_mv SELECT * FROM obs_count_per_month_history_v WHERE history_id = $1;
  RETURN;
END
';

-- obs_count_per_month_history_mv_refresh()
CREATE OR REPLACE FUNCTION obs_count_per_month_history_mv_refresh() RETURNS VOID
SECURITY DEFINER LANGUAGE 'plpgsql' AS '
DECLARE
  mv INTEGER;
BEGIN
  SELECT id INTO mv FROM matviews WHERE mv_name = ''obs_count_per_month_history_mv'';

  PERFORM obs_count_per_month_history_mv_refresh_row(pkey)
    FROM matview_changes
    WHERE mv_id = mv;

  DELETE FROM matview_changes
    WHERE mv_id = mv; 
    
  UPDATE matviews
    SET last_refresh = now()
    WHERE mv_name = ''obs_count_per_month_history_mv'';
END
';

/* BEGIN obs_count_per_month_history_mv TRIGGERS. Depends on obs_raw.  Need triggers on ins/update/del */

 -- obs_raw update trigger
CREATE FUNCTION obs_count_per_month_history_mv_obs_raw_update() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  IF OLD.obs_raw_id = NEW.obs_raw_id THEN
    PERFORM matview_queue_refresh_row(''obs_count_per_month_history_mv'', NEW.history_id); 
  ELSE
    PERFORM matview_queue_refresh_row(''obs_count_per_month_history_mv'', NEW.history_id);
    PERFORM matview_queue_refresh_row(''obs_count_per_month_history_mv'', OLD.history_id);
  END IF;
  RETURN NULL;
END
';
CREATE TRIGGER obs_count_per_month_history_mv_update AFTER UPDATE ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE obs_count_per_month_history_mv_obs_raw_update();

-- obs_raw delete trigger
CREATE FUNCTION obs_count_per_month_history_mv_obs_raw_delete() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''obs_count_per_month_history_mv'', OLD.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER obs_count_per_month_history_mv_delete AFTER DELETE ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE obs_count_per_month_history_mv_obs_raw_delete();

-- obs_raw insert trigger
CREATE FUNCTION obs_count_per_month_history_mv_obs_raw_insert() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''obs_count_per_month_history_mv'', NEW.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER obs_count_per_month_history_mv_insert AFTER INSERT ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE obs_count_per_month_history_mv_obs_raw_insert();