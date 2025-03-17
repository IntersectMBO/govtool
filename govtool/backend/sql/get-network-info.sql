SELECT
	(SELECT MAX(no) FROM epoch) AS current_epoch,
	(SELECT MAX(block_no) FROM block) AS current_block,
	network_name
FROM
	meta;