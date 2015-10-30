CREATE TYPE public.composite AS
   ("time" timestamp without time zone,
    val real);

CREATE TYPE public.composite_var AS ENUM
   ('PRECIP_AND_ANY_TEMP',
    'ANY_TEMP');

CREATE TYPE public.frequency AS ENUM
   ('monthly',
    'daily',
    'hourly',
    'irregular');

CREATE TYPE public.monthly_ts AS
   (month timestamp without time zone,
    datum real);

CREATE TYPE public.timescale AS ENUM
   ('1-minute',
    '2-minute',
    '5-minute',
    '15-minute',
    '30-minute',
    '1-hourly',
    '3-hourly',
    '6-hourly',
    '12-hourly',
    'daily',
    'monthly',
    'yearly',
    'other',
    'irregular');