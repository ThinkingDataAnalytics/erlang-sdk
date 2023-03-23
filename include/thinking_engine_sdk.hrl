-record(te_debug_consumer, {
  server_url = "",
  app_id = "",
  is_write = true,
  device_id = ""
}).

-record(te_log_consumer, {
  fun_log = fun(E) -> E end
}).

-record(te_sdk, {
  consumer = #te_log_consumer{},
  fun_add = fun() -> ok end,
  fun_flush = fun() -> ok end,
  fun_close = fun() -> ok end
}).
