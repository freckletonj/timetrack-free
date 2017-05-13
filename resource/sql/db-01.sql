CREATE USER timetrack_dev_admin WITH PASSWORD a;
CREATE DATABASE local_timetrack;
GRANT ALL PRIVILEGES ON DATABASE local_timetrack TO timetrack_dev_admin;
CREATE EXTENSION "uuid-ossp";
