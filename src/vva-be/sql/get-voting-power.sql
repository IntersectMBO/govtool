select drep_distr.amount
from drep_distr
join drep_hash
on drep_hash.id = drep_distr.hash_id
where drep_hash.raw = decode(?,'hex')