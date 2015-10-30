CREATE OR REPLACE VIEW all_stns_dist_v AS (
    SELECT h1.station_id AS s1_id, h2.station_id AS s2_id, st_distance(geography(st_transform(h1.the_geom, 4326)), geography(st_transform(h2.the_geom, 4326))) AS dist
    FROM meta_history h1
    CROSS JOIN meta_history h2
    WHERE h1.station_id < h2.station_id
);