CREATE OR REPLACE VIEW history_join_station_network AS (
    SELECT meta_station.network_id, meta_history.station_id, meta_history.history_id, meta_history.station_name, meta_history.lon, meta_history.lat, meta_history.elev, meta_history.sdate, meta_history.edate, meta_history.tz_offset, meta_history.province, meta_history.country, meta_history.comments, meta_history.the_geom, meta_history.sensor_id, meta_station.native_id, meta_network.network_name, meta_network.description, meta_network.virtual
    FROM meta_history
    NATURAL JOIN meta_station
    NATURAL JOIN meta_network
);

CREATE OR REPLACE VIEW history_join_station_network_geoserver AS (
    SELECT meta_station.network_id, meta_history.station_id, meta_history.history_id, meta_history.station_name, meta_history.lon, meta_history.lat, meta_history.elev, meta_history.sdate, meta_history.edate, meta_history.tz_offset, meta_history.province, meta_history.country, meta_history.comments, meta_history.the_geom, meta_history.sensor_id, meta_station.native_id, meta_network_geoserver.network_name, meta_network.description, meta_network_geoserver.col_hex, foo.vars
    FROM meta_history
    NATURAL JOIN meta_station
    NATURAL JOIN meta_network_geoserver
    NATURAL JOIN meta_network
    NATURAL JOIN 
    (
        SELECT meta_vars.network_id, array_to_string(array_agg(meta_vars.net_var_name), ', '::text) AS vars
        FROM meta_vars
        GROUP BY meta_vars.network_id
    ) foo
);
