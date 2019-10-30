-- ~80 second query
CREATE OR REPLACE VIEW climo_obs_count_v AS
  SELECT count(*) AS count, history_id
  FROM obs_raw NATURAL JOIN meta_vars
  WHERE cell_method ~ '(within|over)'
  GROUP BY history_id;

SELECT create_matview('climo_obs_count_mv', 'climo_obs_count_v');
CREATE INDEX climo_obs_count_idx ON climo_obs_count_mv(history_id);

-- Lazy update climo_obs_count_mv functions
CREATE OR REPLACE FUNCTION climo_obs_count_mv_refresh_row(climo_obs_count_mv.history_id%TYPE) RETURNS VOID
SECURITY DEFINER
LANGUAGE 'plpgsql' AS '
BEGIN
  DELETE FROM climo_obs_count_mv WHERE history_id = $1;
  INSERT INTO climo_obs_count_mv SELECT * FROM climo_obs_count_v WHERE history_id = $1;
  RETURN;
END
';

-- climo_obs_count_mv_refresh()
CREATE OR REPLACE FUNCTION climo_obs_count_mv_refresh() RETURNS VOID
SECURITY DEFINER LANGUAGE 'plpgsql' AS '
DECLARE
  mv INTEGER;
BEGIN
  SELECT id INTO mv FROM matviews WHERE mv_name = ''climo_obs_count_mv'';

  PERFORM climo_obs_count_mv_refresh_row(pkey)
    FROM matview_changes
    WHERE mv_id = mv;

  DELETE FROM matview_changes
    WHERE mv_id = mv; 
    
  UPDATE matviews
    SET last_refresh = now()
    WHERE mv_name = ''climo_obs_count_mv'';
END
';

/* BEGIN climo_obs_count_mv TRIGGERS. Depends on obs_raw and meta_vars.  
   Need triggers on ins/update/del for both 
*/

 -- obs_raw update trigger
CREATE OR REPLACE FUNCTION climo_obs_count_mv_obs_raw_update() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  IF OLD.obs_raw_id = NEW.obs_raw_id THEN
    PERFORM matview_queue_refresh_row(''climo_obs_count_mv'', NEW.history_id); 
  ELSE
    PERFORM matview_queue_refresh_row(''climo_obs_count_mv'', NEW.history_id);
    PERFORM matview_queue_refresh_row(''climo_obs_count_mv'', OLD.history_id);
  END IF;
  RETURN NULL;
END
';
CREATE TRIGGER climo_obs_count_mv_update AFTER UPDATE ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE climo_obs_count_mv_obs_raw_update();

-- obs_raw delete trigger
CREATE OR REPLACE FUNCTION climo_obs_count_mv_obs_raw_delete() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''climo_obs_count_mv'', OLD.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER climo_obs_count_mv_delete AFTER DELETE ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE climo_obs_count_mv_obs_raw_delete();

-- obs_raw insert trigger
CREATE OR REPLACE FUNCTION climo_obs_count_mv_obs_raw_insert() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  PERFORM matview_queue_refresh_row(''climo_obs_count_mv'', NEW.history_id);
  RETURN NULL;
END
';
CREATE TRIGGER climo_obs_count_mv_insert AFTER INSERT ON obs_raw
  FOR EACH ROW EXECUTE PROCEDURE climo_obs_count_mv_obs_raw_insert();

/*
Meta Vars updates...
These are a bit funny cause on meta_vars cnahges, you need to find 
each history_id that had that vars_id to queue an update
*/

 -- meta_vars update trigger

 -- Actually, we don't care about updates. This is just a count.


-- meta_vars delete trigger
CREATE OR REPLACE FUNCTION climo_obs_count_mv_meta_vars_delete() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  -- This is a bad way to do this.  We should find all history_ids
  -- by selecting distinct on obs_raw by vars_id, but that takes WAY too
  -- long (80s). Downside is this method refreshes all history_ids which MAY
  -- contain this var_id rather than those that actually do.

  PERFORM matview_queue_refresh_row(''climo_obs_count_mv'', history_id)
    FROM meta_vars 
    NATURAL JOIN meta_network
    NATURAL JOIN meta_station
    NATURAL JOIN meta_history
    WHERE vars_id = OLD.vars_id;
  RETURN NULL;
END
';
DROP TRIGGER IF EXISTS climo_obs_count_mv_delete ON meta_vars;
CREATE TRIGGER climo_obs_count_mv_delete AFTER DELETE ON meta_vars
  FOR EACH ROW EXECUTE PROCEDURE climo_obs_count_mv_meta_vars_delete();

-- meta_vars insert trigger
CREATE OR REPLACE FUNCTION climo_obs_count_mv_meta_vars_insert() RETURNS TRIGGER
SECURITY DEFINER LANGUAGE 'plpgsql' AS'
BEGIN
  -- This is a bad way to do this.  We should find all history_ids
  -- by selecting distinct on obs_raw by vars_id, but that takes WAY too
  -- long (80s). Downside is this method refreshes all history_ids which MAY
  -- contain this var_id rather than those that actually do.

  PERFORM matview_queue_refresh_row(''climo_obs_count_mv'', history_id)
    FROM meta_vars 
    NATURAL JOIN meta_network
    NATURAL JOIN meta_station
    NATURAL JOIN meta_history
    WHERE vars_id = NEW.vars_id;
  RETURN NULL;
END
';
DROP TRIGGER IF EXISTS climo_obs_count_mv_insert ON meta_vars;
CREATE TRIGGER climo_obs_count_mv_insert AFTER INSERT ON meta_vars
  FOR EACH ROW EXECUTE PROCEDURE climo_obs_count_mv_meta_vars_insert();
