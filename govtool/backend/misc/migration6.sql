ALTER TABLE governance_action ADD COLUMN temp_column json;

UPDATE governance_action SET temp_column = json_build_object('message', details::text);

ALTER TABLE governance_action DROP COLUMN details;

ALTER TABLE governance_action RENAME COLUMN temp_column TO details;
