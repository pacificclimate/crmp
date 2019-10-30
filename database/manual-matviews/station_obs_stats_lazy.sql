-- ~19 min query (1133909 ms) -> 11 seconds after one hourly update
CREATE OR REPLACE VIEW station_obs_stats_v AS
  SELECT station_id, history_id, min_obs_time, max_obs_time, obs_count
    FROM (
      SELECT min(obs_time) AS min_obs_time, max(obs_time) AS max_obs_time, history_id, count(DISTINCT obs_time) AS obs_count
      FROM obs_raw
      GROUP BY history_id
    ) AS foo
  NATURAL JOIN meta_history;

SELECT create_matview('station_obs_stats_mv', 'station_obs_stats_v');
CREATE INDEX station_obs_stats_mv_idx ON station_obs_stats_mv(min_obs_time, max_obs_time, station_id, history_id);


CREATE OR REPLACE FUNCTION station_obs_stats_mv_refresh_row(station_obs_stats_mv.history_id%TYPE) RETURNS VOID
SECURITY DEFINER
LANGUAGE 'plpgsql' AS '
BEGIN
  DELETE FROM station_obs_stats_mv WHERE history_id = $1;
  INSERT INTO station_obs_stats_mv SELECT * FROM station_obs_stats_v WHERE history_id = $1;
  RETURN;
END
';

-- station_obs_stats_mv_refresh()
CREATE OR REPLACE FUNCTION station_obs_stats_mv_refresh() RETURNS VOID
SECURITY DEFINER LANGUAGE 'plpgsql' AS '
DECLARE
  mv INTEGER;
BEGIN
  SELECT id INTO mv FROM matviews WHERE mv_name = ''station_obs_stats_mv'';

  PERFORM station_obs_stats_mv_refresh_row(pkey)
    FROM matview_changes
    WHERE mv_id = mv;

  DELETE FROM matview_changes
    WHERE mv_id = mv;
    
  UPDATE matviews
    SET last_refresh = now()
    WHERE mv_name = ''station_obs_stats_mv'';
END
';

/* BEGIN station_obs_stats_mv TRIGGERS. Depends on obs_raw and meta_history.  Need triggers on ins/update/del for both */

 -- obs_raw update trigger
CREATE FUNCTION station_obs_stats_mv_obs_raw_update() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  IF OLD.obs_raw_id = NEW.obs_raw_id THEN
    PERFORM matview_queue_refresh_row(''station_obs_stats_mv'', NEW.history_id); 
  ELSE
    PERFORM matview_queue_refresh_row(''station_obs_stats_mv'', NEW.history_id);
    PERFORM matview_queue_refresh_row(''station_obs_stats_mv'', OLD.history_id);
  END IF;
  RETURN NULL;
END
';
CREATE TRIGGER station_obs_stats_mv_update AFTER UPDATE ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE station_obs_stats_mv_obs_raw_update();

-- obs_raw delete trigger
CREATE FUNCTION station_obs_stats_mv_obs_raw_delete() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''station_obs_stats_mv'', OLD.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER station_obs_stats_mv_delete AFTER DELETE ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE station_obs_stats_mv_obs_raw_delete();

-- obs_raw insert trigger
CREATE FUNCTION station_obs_stats_mv_obs_raw_insert() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''station_obs_stats_mv'', NEW.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER station_obs_stats_mv_insert AFTER INSERT ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE station_obs_stats_mv_obs_raw_insert();

 -- meta_history update trigger
CREATE FUNCTION station_obs_stats_mv_meta_history_update() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  IF OLD.history_id = NEW.history_id THEN
    PERFORM matview_queue_refresh_row(''station_obs_stats_mv'', NEW.history_id); 
  ELSE
    PERFORM matview_queue_refresh_row(''station_obs_stats_mv'', NEW.history_id);
    PERFORM matview_queue_refresh_row(''station_obs_stats_mv'', OLD.history_id);
  END IF;
  RETURN NULL;
END
';
CREATE TRIGGER station_obs_stats_mv_update AFTER UPDATE ON meta_history
  FOR EACH ROW EXECUTE PROCEDURE station_obs_stats_mv_meta_history_update();

-- meta_history delete trigger
CREATE FUNCTION station_obs_stats_mv_meta_history_delete() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''station_obs_stats_mv'', OLD.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER station_obs_stats_mv_delete AFTER DELETE ON meta_history
  FOR EACH ROW EXECUTE PROCEDURE station_obs_stats_mv_meta_history_delete();

-- meta_history insert trigger
CREATE FUNCTION station_obs_stats_mv_meta_history_insert() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''station_obs_stats_mv'', NEW.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER station_obs_stats_mv_insert AFTER INSERT ON meta_history
  FOR EACH ROW EXECUTE PROCEDURE station_obs_stats_mv_meta_history_insert();
