select exists (select * from tx where tx.hash = decode(?, 'hex'))
