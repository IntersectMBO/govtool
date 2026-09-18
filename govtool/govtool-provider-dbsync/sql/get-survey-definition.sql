SELECT encode(tx_metadata.bytes, 'hex') AS payload_cbor_hex
FROM tx_metadata
JOIN tx ON tx.id = tx_metadata.tx_id
WHERE tx.hash = decode($1, 'hex')
  AND tx_metadata.key = 17
LIMIT 1;