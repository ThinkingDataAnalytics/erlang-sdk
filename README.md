# Erlang SDK 使用指南

本指南将会为您介绍如何使用 Erlang SDK 接入您的项目，您可以在访问 [GitHub](https://github.com/ThinkingDataAnalytics/erlang-sdk) 获取 SDK 的源代码。

**最新版本为**：v1.1.3

**更新时间为**：2022-10-18

[Erlang SDK 下载地址](https://github.com/ThinkingDataAnalytics/erlang-sdk)

## 一、集成并初始化 SDK


（1）ta_consumer_log： 批量实时写本地文件，默认以天为分隔，需要与 LogBus 搭配使用进行数据上传。

（2）ta_consumer_debug： 逐条实时地向 TA 服务器传输数据，不需要搭配传输工具，如果数据出现错误，整条数据都将不会入库，并且返回详细的错误说明，不建议在生产环境中使用。

具体使用方式可以参考 SDK 中 example 文件夹中的示例文件。


### 1.1 集成 SDK

#### v1.1.3 以及之后的新版本，采用 rebar3 引入：
第一步：需要您的项目已经引入 rebar3 环境。

第二步：修改您的 rebar.config 文件，添加对 thinkingdata_analytics SDK 的引用。

``` Erlang
{erl_opts, [debug_info,
    {parse_transform, lager_transform} %% 使用 lager 库所必须的参数
]}.

{deps, [
	%% 添加数数采集SDK
    {thinkingdata_analytics, {git, "git@github.com:ThinkingDataAnalytics/erlang-sdk.git",{tag, "v1.1.3"}}}
]}.

{shell, [
	%% 启用配置文件
    {config, "config/sys.config"},
    {apps, [app_3]}
]}.

```
执行命令：

``` shell
$ rebar3 compile
```

第三步：修改您项目的配置文件，在您的配置文件中，添加对 lager 库的配置，主要是增加一个数数SDK单独使用的sink。

```Erlang
[
  %% lager日志库配置
  {lager, [
    {colored, true},
    {log_root, "./log"},
    %% 这里增加一个数数SDK单独使用的sink，名字固定为：ta_logger_lager_event
    {extra_sinks,
      [
        {ta_logger_lager_event,
          [{handlers, [
            {lager_file_backend, [
              {file, "ta/ta.log"}, %% 配置采集数据的文件路径以及名字
              {level, info},
              {formatter, lager_default_formatter},
              {formatter_config, [message, "\n"]},
              {size, 10485760}, %% 单个文件的分页大小10Mb
              {rotator, ta_lager_rotator} %% 自定义日志轮转
            ]}]},
            {async_threshold, 500},
            {async_threshold_window, 50}
          ]
        }]
    }
  ]
  }
].
```
提示：在SDK目录中有示例文件 example_sys.config，可以参考示例配置。

第四步：在您的app项目配置文件中配置启动参数。在 **.app.src 文件中添加数数SDK的启动项。

``` Erlang
{application, your_name,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {your_name_app, []}},
  {applications,
   [kernel,
    stdlib,
    thinkingdata_analytics %% 这里添加数数SDK
   ]},
  {env,[]},
  {modules, []},

  {licenses, ["Apache-2.0"]},
  {links, []}
 ]}.

```


#### v1.1.2 以及之前的版本，采用源码引入：

通过代码方式引入：下载 Erlang SDK，进行解压后可将 thinking_data 文件夹引入项目中，直接调用 SDK 对应的 module 进行使用。


### 1.2 初始化 SDK

您可以通过以下方法获得 SDK 实例：

（1）ta_consumer_log：

```Erlang
%% 必须先调用init，SDK内部进行必要的初始化
ta_consumer_log:init(),

%% 配置完成之后，开始初始化 thinking_analytics_sdk 模块
%% 初始化SDK，传入上报方式类型
thinking_analytics_sdk:init(thinking_analytics_sdk:consumer_type_log()),

%% 普通事件
thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"key_1" => "value_1", "key_2" => "value_2"}),

%% 复杂数据类型
thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"custom_property_1" => [#{"key_1" => "value_1"}, #{"key_2" => "value_2"}, #{"key_3_list" => ["a", "b", #{"child_key" => "child_value"}]}]}),

%% ⚠️ 如果您的属性值有时间类型，请一定要用 ta_utils:format_time() 函数进行格式化，然后才能传入。例如：
%% 包含时间的属性值
thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"register_time" => ta_utils:format_time(os:timestamp())}),

%% 关闭SDK时候需要调用。如果关闭后，需要再开启SDK时，必须重新执行上文的初始化代码。
thinking_analytics_sdk:close().
```

`ta_consumer_log`将会批量实时地生成日志文件，文件默认以天切分，需要搭配 LogBus 进行上传。

需要调用`config_directory()` 函数来更改日志存储的目录，要注意文件目录的访问权限。您只需将 LogBus 的监听文件夹地址设置为此处的地址，即可使用 LogBus 进行数据的监听上传。



(2) ta_consumer_debug：

```Erlang

%% 必须先调用init，SDK内部进行必要的初始化
ta_consumer_debug:init(),
%% 配置上报地址
ta_consumer_debug:config_server_url("server_url"),
%% 配置app_id
ta_consumer_debug:config_app_id("app_id"),
%% 配置是否写入数据库
ta_consumer_debug:config_is_write(true),

%% 配置完成之后，开始初始化 thinking_analytics_sdk 模块
%% 初始化SDK，传入上报方式类型
thinking_analytics_sdk:init(thinking_analytics_sdk:consumer_type_debug()),

%% 普通事件
thinking_analytics_sdk:track("account_id_Erlang", "distinct_id", "ViewProduct", #{"key_1" => "value_1", "key_2" => "value_2"}),

%% 复杂数据类型
thinking_analytics_sdk:track("account_id_Erlang", "distinct_id", "ViewProduct", #{"custom_property_1" => [#{"key_1" => "value_1"}, #{"key_2" => "value_2"}, #{"key_3_list" => ["a", "b", #{"child_key" => "child_value"}]}]}),

%% ⚠️ 如果您的属性值有时间类型，请一定要用 ta_utils:format_time() 函数进行格式化，然后才能传入。例如：
%% 包含时间的属性值
thinking_analytics_sdk:track("account_id_Erlang", "distinct_id", "ViewProduct", #{"register_time" => ta_utils:format_time(os:timestamp())}),

%% 关闭SDK时候需要调用。如果关闭后，需要再开启SDK时，必须重新执行上文的初始化代码。
thinking_analytics_sdk:close().


```

需要调用`config_server_url()`函数，配置传输数据的 URL。调用`config_app_id()`函数设置您项目的 APP ID。

`config_is_write()`函数，用来配置是否入库。false 表示不入库，只做数据验证，默认为true。

不建议在生产环境中使用，只用来在集成初期进行数据验证。

## 二、上报数据

在 SDK 初始化完成之后，您就可以调用`track`来上传事件，一般情况下，您可能需要上传十几到上百个不同的事件，如果您是第一次使用 TA 后台，我们推荐您先上传几个关键事件。

如果您对需要发送什么样的事件有疑惑，可以查看[快速使用指南](https://thinkingdata.feishu.cn/wiki/wikcnArHwMmAD6B8hXCLTYLyCne)了解更多信息。

### 2.1 发送事件

您可以调用`track`来上传事件，建议您根据先前梳理的文档来设置事件的属性以及发送信息的条件，此处以用户付费作为范例：

```Erlang
%% 注意account_id 和 distinct_id 必须至少设置其中一个
%% 设置用户的ip地址，TA系统会根据IP地址解析用户的地理位置信息，如果不设置的话，则默认不上报
%% 设置事件发生的时间，如果不设置的话，则默认使用为当前时间。注意：#time的类型必须是timestamp()类型

%% 上报事件
thinking_analytics_sdk:track("account_id", "distinct_id", "EventName", #{"#ip" => "123.123.123.123", "#time" => os:timestamp()}),
```

- 事件的名称是`strin``g()`类型，只能以字母开头，可包含数字，字母和下划线 “_”，长度最大为 50 个字符，对字母大小写不敏感。

- 自定义属性的 Key 的值为属性的名称，为 string 类型，规定只能以字母开头，包含数字，字母和下划线 “_”，长度最大为 50 个字符，对字母大小写不敏感。
- 自定义属性的 Value 为该属性的值。

### 2.2 时间属性
如果您的属性值有时间类型，请一定要用 `ta_utils:format_time()` 函数进行格式化，然后才能传入。例如：

```erlang

%% 包含时间的属性值
%%  thinking_analytics_sdk:track("account_id_Erlang", "distinct_logbus", "ViewProduct", #{"register_time" => ta_utils:format_time(os:timestamp())}),

```
## 三、用户属性

### 3.1 user_set

对于一般的用户属性，您可以调用`user_set`来进行设置，使用该接口上传的属性将会覆盖原有的属性值，如果之前不存在该用户属性，则会新建该用户属性，类型与传入属性的类型一致：

```Erlang
thinking_analytics_sdk:user_set("account_id", "distinct_id", #{"age" => 18, "abc" => ["a", "b", "c"]}),
```

> 属性格式要求与事件属性保持一致。

### 3.2 user_setOnce

如果您要上传的用户属性只要设置一次，则可以调用`user_setOnce`来进行设置，当该属性之前已经有值的时候，将会忽略这条信息：

```Erlang
thinking_analytics_sdk:user_set_once("account_id", "distinct_id", #{"firstvalue" => 1}),
```

> 属性格式要求与事件属性保持一致。

### 3.3 user_add

当您要上传数值型的属性时，您可以调用`user_add`来对该属性进行累加操作，如果该属性还未被设置，则会赋值 0 后再进行计算，可传入负值，等同于相减操作。

```Erlang
thinking_analytics_sdk:user_add("account_id", "distinct_id", #{"amount" => 100}),
```

> 设置的属性key为字符串，Value 只允许为数值。

### 3.4 user_del

如果您要删除某个用户，可以调用`user_del`将这名用户删除，您将无法再查询该名用户的用户属性，但该用户产生的事件仍然可以被查询到

```Erlang
thinking_analytics_sdk:user_del("account_id", "distinct_id"),
```

### 3.5 user_append

您可以调用 `user_append` 对 array 类型的用户属性进行追加操作。

```Erlang
thinking_analytics_sdk:user_append("account_id", "distinct_id", #{"array" => ["arr3", "arr4"]}),
```

### 3.6 user_unset

当您需要清空某个用户的用户属性的值时，可以调用 `user_unset` 进行清空。

```Erlang
thinking_analytics_sdk:user_unset("account_id", "distinct_id", ["age", "abc"]),
```

> user_unset: 的传入值为被清空属性的 Key 值。

### 3.7 user_uniq_append

当您要为 list 类型追加用户属性值，但不希望出现重复值时，您可以调用 `ta_user_uniq_append` 来对指定属性进行追加操作，如果该属性还未在集群中被创建，则 `ta_user_uniq_append` 创建该属性

```Erlang
thinking_analytics_sdk:user_unique_append("account_id", "distinct_id", #{"array" => ["arr3", "arr4", "arr5"]}),
```

## 四、其他操作

### 4.1 关闭 sdk

```Erlang
%% 关闭SDK时候需要调用
thinking_analytics_sdk:close()
```

关闭并退出 sdk，**请在关闭服务器前调用本接口****。主要是为了释放ETS表里的数据**

## 五、相关预置属性

### 5.1 所有事件带有的预置属性

以下预置属性，是 Erlang SDK 中所有事件（包括自动采集事件）都会带有的预置属性

| **属性名**    | **中文名** | **说明**                                                     |
| ------------- | ---------- | ------------------------------------------------------------ |
| #ip           | IP 地址    | 用户的 IP 地址，需要进行手动设置，TA 将以此获取用户的地理位置信息 |
| #country      | 国家       | 用户所在国家，根据 IP 地址生成                               |
| #country_code | 国家代码   | 用户所在国家的国家代码(ISO 3166-1 alpha-2，即两位大写英文字母)，根据 IP 地址生成 |
| #province     | 省份       | 用户所在省份，根据 IP 地址生成                               |
| #city         | 城市       | 用户所在城市，根据 IP 地址生成                               |
| #lib          | SDK 类型   | 您接入 SDK 的类型，如 tga_c_sdk 等                           |
| #lib_version  | SDK 版本   | 您接入 C SDK 的版本                                          |

## 六、进阶功能

### 6.1 可更新事件

您可以通过可更新事件实现特定场景下需要修改事件数据的需求。可更新事件需要指定标识该事件的 ID，并在创建可更新事件对象时传入。TA 后台将根据事件名和事件 ID 来确定需要更新的数据。

```Erlang
EventName = "event_name",
EventId = "event_id",
%% 上报后，事件属性 status 为 3, price 为 100
thinking_analytics_sdk:track_update("account_id_Erlang", "distinct_id", EventName, EventId, #{"price" => 100, "status" => 3}),

%% 上报后，事件属性 status 为 5, price 为 100 不变
thinking_analytics_sdk:track_update("account_id_Erlang", "distinct_id", EventName, EventId, #{"price" => 100, "status" => 5}),
```

### 6.2 可重写事件

可重写事件与可更新事件类似，区别在于可重写事件会用最新的数据完全覆盖历史数据，从效果上看相当于删除前一条数据，并入库最新的数据。TA 后台将根据事件名和事件 ID 来确定需要更新的数据。

```Erlang
EventName = "overWrite_event",
EventId = "event_id",
%% 上报后，事件属性 price 为 100, status 为 5
thinking_analytics_sdk:track_overwrite("account_id_Erlang", "distinct_id", EventName, EventId, #{"price" => 100, "status" => 5}),

%% 上报后，事件属性 price 为 20, status 属性被删除
thinking_analytics_sdk:track_overwrite("account_id_Erlang", "distinct_id", EventName, EventId, #{"price" => 20}),
```

### 6.3 首次事件

使用“首次事件校验”特性，必须设置first_check_id，类型为字符串，该字段是校验首次事件的标识 ID，该 ID 首条出现的数据将入库，之后出现的都无法入库。不同事件的first_check_id互相独立，因此每个事件的首次校验互不干扰

```Erlang
EventName = "first_event",
FirstCheckId = "first_check_id",
%% 首次事件
thinking_analytics_sdk:track_first("account_id_Erlang", "distinct_id", EventName, FirstCheckId, #{"key1" => "value1", "key2" => "value2"}),
```

## ChangeLog

### v1.0.0 2022/05/23

- 支持debug_consumer
- 支持log_consumer

### v1.1.0 2022/07/13

- 自定义属性支持复杂数据类型

### v1.1.1 2022/09/25

- 兼容 19.3 版本

### v1.1.2 2022/09/29

- 优化文件IO性能

### v1.1.3 2022/10/18

- 使用包管理工具集成SDK