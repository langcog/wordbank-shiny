# turn off innodb_strict_mode (otherwise db has "Row size too large" error)
# https://mariadb.com/kb/en/innodb-strict-mode/#row-size-too-large
SET GLOBAL innodb_strict_mode = 0;

# add mysql reader user
CREATE USER 'wordbank_reader'@'%' IDENTIFIED BY 'ICanOnlyRead@99';
GRANT SELECT ON wordbank.* TO 'wordbank_reader'@'%';
