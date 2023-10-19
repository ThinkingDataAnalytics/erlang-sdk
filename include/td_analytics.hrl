-record(td_debug_consumer, {
  server_url = "",
  app_id = "",
  is_write = true,
  device_id = ""
}).

-record(td_log_consumer, {
  fun_log = fun(E) -> E end
}).

-record(td_analytics, {
  consumer = #td_log_consumer{},
  fun_add = fun() -> ok end,
  fun_flush = fun() -> ok end,
  fun_close = fun() -> ok end
}).
