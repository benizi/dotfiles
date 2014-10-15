SELECT
  granted,
  locktype as typ,
  relation::regclass as tbl,
  now() - xact_start as tx_time,
  now() - query_start as q_time,
  date_trunc('minute', now() - state_change) as d_time,
  application_name as app,
  client_addr as client,
  l.pid,
  query
FROM pg_catalog.pg_locks as l
JOIN pg_catalog.pg_stat_activity as s
  ON (l.pid = s.pid)
WHERE l.pid <> pg_backend_pid()
ORDER by date_trunc('minute', now() - state_change) desc,
  client_addr
;
