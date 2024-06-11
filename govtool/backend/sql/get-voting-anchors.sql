select id, url, encode(data_hash, 'hex'), type::text
from voting_anchor
where voting_anchor.id > ?