-- This one is a ~4 second query
CREATE OR REPLACE VIEW collapsed_vars_v AS 
  SELECT history_id, array_to_string(array_agg(standard_name || regexp_replace(cell_method, 'time: ', '_', 'g')), ', '::text) AS vars,
         array_to_string(array_agg(display_name), '|') AS display_names
  FROM (
    SELECT DISTINCT history_id, vars_id FROM obs_raw
    ) AS vars_per_history NATURAL JOIN meta_vars
  GROUP BY history_id;

SELECT create_matview('collapsed_vars_mv', 'collapsed_vars_v');
CREATE INDEX collapsed_vars_idx ON collapsed_vars_mv(history_id);

-- Lazy update collapsed_vars_mv functions
CREATE OR REPLACE FUNCTION collapsed_vars_mv_refresh_row(collapsed_vars_mv.history_id%TYPE) RETURNS VOID
SECURITY DEFINER
LANGUAGE 'plpgsql' AS '
BEGIN
  DELETE FROM collapsed_vars_mv WHERE history_id = $1;
  INSERT INTO collapsed_vars_mv SELECT * FROM collapsed_vars_v WHERE history_id = $1;
  RETURN;
END
';

-- collapsed_vars_mv_refresh()
CREATE OR REPLACE FUNCTION collapsed_vars_mv_refresh() RETURNS VOID
SECURITY DEFINER LANGUAGE 'plpgsql' AS '
DECLARE
  mv INTEGER;
BEGIN
  SELECT id INTO mv FROM matviews WHERE mv_name = ''collapsed_vars_mv'';

  PERFORM collapsed_vars_mv_refresh_row(pkey)
    FROM matview_changes
    WHERE mv_id = mv;

  DELETE FROM matview_changes
    WHERE mv_id = mv; 
    
  UPDATE matviews
    SET last_refresh = now()
    WHERE mv_name = ''collapsed_vars_mv'';
END
';

/* BEGIN collapsed_vars_mv TRIGGERS. Depends on obs_raw and meta_vars.  
   Need triggers on ins/update/del for both 
*/

 -- obs_raw update trigger
CREATE OR REPLACE FUNCTION collapsed_vars_mv_obs_raw_update() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  IF OLD.obs_raw_id = NEW.obs_raw_id THEN
    PERFORM matview_queue_refresh_row(''collapsed_vars_mv'', NEW.history_id); 
  ELSE
    PERFORM matview_queue_refresh_row(''collapsed_vars_mv'', NEW.history_id);
    PERFORM matview_queue_refresh_row(''collapsed_vars_mv'', OLD.history_id);
  END IF;
  RETURN NULL;
END
';
CREATE TRIGGER collapsed_vars_mv_update AFTER UPDATE ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE collapsed_vars_mv_obs_raw_update();

-- obs_raw delete trigger
CREATE OR REPLACE FUNCTION collapsed_vars_mv_obs_raw_delete() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''collapsed_vars_mv'', OLD.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER collapsed_vars_mv_delete AFTER DELETE ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE collapsed_vars_mv_obs_raw_delete();

-- obs_raw insert trigger
CREATE OR REPLACE FUNCTION collapsed_vars_mv_obs_raw_insert() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''collapsed_vars_mv'', NEW.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER collapsed_vars_mv_insert AFTER INSERT ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE collapsed_vars_mv_obs_raw_insert();

/*
Meta Vars updates...
These are a bit funny cause on meta_vars changes, you need to find 
each history_id that had that vars_id to queue an update
*/

 -- meta_vars update trigger
CREATE OR REPLACE FUNCTION collapsed_vars_mv_meta_vars_update() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  -- This is a bad way to do this.  We should find all history_ids
  -- by selecting distinct on obs_raw by vars_id, but that takes WAY too
  -- long (80s). Downside is this method refreshes all history_ids which MAY
  -- contain this var_id rather than those that actually do.

  PERFORM matview_queue_refresh_row(''collapsed_vars_mv'', history_id)
    FROM meta_vars 
    NATURAL JOIN meta_network
    NATURAL JOIN meta_station
    NATURAL JOIN meta_history
    WHERE vars_id = OLD.vars_id;
  RETURN NULL;
END
';
DROP TRIGGER IF EXISTS collapsed_vars_mv_update ON meta_vars;
CREATE TRIGGER collapsed_vars_mv_update AFTER UPDATE ON meta_vars
  FOR EACH ROW EXECUTE PROCEDURE collapsed_vars_mv_meta_vars_update();

-- meta_vars delete trigger
CREATE OR REPLACE FUNCTION collapsed_vars_mv_meta_vars_delete() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  -- This is a bad way to do this.  We should find all history_ids
  -- by selecting distinct on obs_raw by vars_id, but that takes WAY too
  -- long (80s). Downside is this method refreshes all history_ids which MAY
  -- contain this var_id rather than those that actually do.

  PERFORM matview_queue_refresh_row(''collapsed_vars_mv'', history_id)
    FROM meta_vars 
    NATURAL JOIN meta_network
    NATURAL JOIN meta_station
    NATURAL JOIN meta_history
    WHERE vars_id = OLD.vars_id;
  RETURN NULL;
END
';
DROP TRIGGER IF EXISTS collapsed_vars_mv_delete ON meta_vars;
CREATE TRIGGER collapsed_vars_mv_delete AFTER DELETE ON meta_vars
  FOR EACH ROW EXECUTE PROCEDURE collapsed_vars_mv_meta_vars_delete();

-- meta_vars insert trigger
CREATE OR REPLACE FUNCTION collapsed_vars_mv_meta_vars_insert() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  -- This is a bad way to do this.  We should find all history_ids
  -- by selecting distinct on obs_raw by vars_id, but that takes WAY too
  -- long (80s). Downside is this method refreshes all history_ids which MAY
  -- contain this var_id rather than those that actually do.

  PERFORM matview_queue_refresh_row(''collapsed_vars_mv'', history_id)
    FROM meta_vars 
    NATURAL JOIN meta_network
    NATURAL JOIN meta_station
    NATURAL JOIN meta_history
    WHERE vars_id = NEW.vars_id;
  RETURN NULL;
END
';
DROP TRIGGER IF EXISTS collapsed_vars_mv_insert ON meta_vars;
CREATE TRIGGER collapsed_vars_mv_insert AFTER INSERT ON meta_vars
  FOR EACH ROW EXECUTE PROCEDURE collapsed_vars_mv_meta_vars_insert();
