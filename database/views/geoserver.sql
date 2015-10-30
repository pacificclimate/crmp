CREATE OR REPLACE VIEW crmp_network_geoserver AS (
    SELECT meta_network.network_name, meta_station.native_id, meta_history.station_name, meta_history.lon, meta_history.lat, meta_history.elev, station_obs_stats_mv.min_obs_time, station_obs_stats_mv.max_obs_time, meta_history.freq::text AS freq, meta_history.tz_offset, meta_history.province, meta_history.station_id, meta_history.history_id, meta_history.country, meta_history.comments, meta_history.the_geom, meta_history.sensor_id, meta_network.description, meta_station.network_id, meta_network.col_hex, collapsed_vars_mv.vars, collapsed_vars_mv.display_names
    FROM meta_history
    NATURAL JOIN meta_station
    NATURAL JOIN meta_network
    LEFT JOIN collapsed_vars_mv USING (history_id)
    LEFT JOIN station_obs_stats_mv USING (history_id)
   WHERE meta_history.province = 'BC'
);