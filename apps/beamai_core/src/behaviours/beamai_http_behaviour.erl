%%%-------------------------------------------------------------------
%%% @doc HTTP 客户端行为定义
%%%
%%% 定义 HTTP 客户端的标准接口，支持多种后端实现（hackney, gun 等）。
%%%
%%% == 回调函数 ==
%%%
%%% 必须实现：
%%% - request/5: 发送 HTTP 请求
%%% - stream_request/6: 发送流式请求
%%% - ensure_started/0: 确保客户端已启动
%%%
%%% == 实现示例 ==
%%%
%%% ```erlang
%%% -module(my_http_client).
%%% -behaviour(beamai_http_behaviour).
%%% -export([request/5, stream_request/6, ensure_started/0]).
%%%
%%% request(Method, Url, Headers, Body, Opts) ->
%%%     %% 实现 HTTP 请求
%%%     ...
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_behaviour).

%%====================================================================
%% 类型定义
%%====================================================================

-type method() :: get | post | put | delete | head | options | patch.
-type url() :: binary() | string().
-type headers() :: [{binary(), binary()}].
-type body() :: binary() | iodata().
-type opts() :: #{
    timeout => pos_integer(),
    connect_timeout => pos_integer(),
    headers => headers(),
    pool => atom(),
    init_acc => term()
}.

-type response() :: {ok, term()} | {error, term()}.
-type chunk_handler() :: fun((binary(), term()) -> {continue, term()} | {done, term()}).
-type stream_response() :: {ok, term()} | {error, term()}.

-export_type([method/0, url/0, headers/0, body/0, opts/0]).
-export_type([response/0, chunk_handler/0, stream_response/0]).

%%====================================================================
%% 行为回调定义
%%====================================================================

%% @doc 发送 HTTP 请求
%%
%% @param Method HTTP 方法
%% @param Url 请求 URL
%% @param Headers 请求头
%% @param Body 请求体
%% @param Opts 选项
%% @returns {ok, Response} | {error, Reason}
-callback request(Method :: method(),
                  Url :: url(),
                  Headers :: headers(),
                  Body :: body(),
                  Opts :: opts()) -> response().

%% @doc 发送流式请求
%%
%% @param Method HTTP 方法
%% @param Url 请求 URL
%% @param Headers 请求头
%% @param Body 请求体
%% @param Opts 选项
%% @param Handler 数据块处理函数
%% @returns {ok, Result} | {error, Reason}
-callback stream_request(Method :: method(),
                         Url :: url(),
                         Headers :: headers(),
                         Body :: body(),
                         Opts :: opts(),
                         Handler :: chunk_handler()) -> stream_response().

%% @doc 确保 HTTP 客户端已启动
%%
%% @returns ok
-callback ensure_started() -> ok.

%%====================================================================
%% 可选回调（带默认实现）
%%====================================================================

%% @doc 关闭连接（可选）
-callback close(Ref :: term()) -> ok.

-optional_callbacks([close/1]).
