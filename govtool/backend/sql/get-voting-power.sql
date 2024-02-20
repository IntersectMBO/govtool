select coalesce(drep_distr.amount, 0) as amount
from drep_hash
left join drep_distr
on drep_hash.id = drep_distr.hash_id
where drep_hash.raw = decode(?,'hex')
order by epoch_no desc
limit 1