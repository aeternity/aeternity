%% This code is generated from apps/aehttp/priv/oas3.yaml
%% Using swagger_endpoints rebar3 plugin version: 0.2.1
%% Do not manually change this code!
%%
%% json_schema/0 implements a JSON Schema for the definitions
%% Reference should be fixed!
%% Use jsx:prettify(jsx:encode(json_schema())) to get a JSON string.

-module(oas_endpoints).

-export([operation/1, operations/0, definitions/0, json_schema/0,
         validate_request/3, validate_response/4, path/3, query/3,
         validate/2]).

operations() ->
    #{'GetKeyBlockByHeight' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","height"},
                    {"description","The height"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> => 42,
                       <<"maximum">> => 18446744073709552000,
                       <<"minimum">> => 0,<<"type">> => <<"integer">>}}]],
              path => <<"/key-blocks/height/{height}">>,
              responses => #{200 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'PostChannelCloseSolo' =>
      #{post =>
            #{parameters => [],path => <<"/debug/channels/close/solo">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"internal">>,<<"channel">>,<<"debug">>]}},
  'GetChainEnds' =>
      #{get =>
            #{parameters => [],path => <<"/status/chain-ends">>,
              responses => #{200 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'GetCurrentKeyBlock' =>
      #{get =>
            #{parameters => [],path => <<"/key-blocks/current">>,
              responses => #{200 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'PostNameTransfer' =>
      #{post =>
            #{parameters => [],path => <<"/debug/names/transfer">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"name_service">>,<<"debug">>]}},
  'GetNetworkStatus' =>
      #{get =>
            #{parameters => [],path => <<"/debug/network">>,
              responses => #{200 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"node_info">>,<<"debug">>]}},
  'GetAccountByPubkey' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","The public key of the account"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ak_dArxCkAsk1mZB1L9CX3cdz1GDN4hN84L3Q8dMLHN4v8cU85TF">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/accounts/{pubkey}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"account">>]}},
  'GetTransactionByHash' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","hash"},
                    {"description","The hash of the transaction"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"th_2w75xjDjHEmphsHDSXrThRnPx6hSUiS7hhSRcuytJABZZ2KkdG">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/transactions/{hash}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"transaction">>]}},
  'GetAccountByPubkeyAndHeight' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","The public key of the account"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ak_dArxCkAsk1mZB1L9CX3cdz1GDN4hN84L3Q8dMLHN4v8cU85TF">>,
                       <<"type">> => <<"string">>}}],
                   [{"in","path"},
                    {"name","height"},
                    {"description","The height"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> => 42,
                       <<"maximum">> => 18446744073709552000,
                       <<"minimum">> => 0,<<"type">> => <<"integer">>}}]],
              path => <<"/accounts/{pubkey}/height/{height}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"account">>]}},
  'GetNameEntryByName' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","name"},
                    {"description","The name key of the name entry"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> => <<"dimitar.chain">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/names/{name}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"name_service">>]}},
  'GetCommitmentId' =>
      #{get =>
            #{parameters =>
                  [[{"in","query"},
                    {"name","name"},
                    {"description","Name"},
                    {"required",true},
                    {"schema",#{<<"type">> => <<"string">>}}],
                   [{"in","query"},
                    {"name","salt"},
                    {"description","Salt"},
                    {"required",true},
                    {"schema",
                     #{<<"minimum">> => 0,<<"type">> => <<"integer">>}}]],
              path => <<"/debug/names/commitment-id">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"internal">>,<<"name_service">>,<<"debug">>]}},
  'PostKeyBlock' =>
      #{post =>
            #{parameters => [],path => <<"/key-blocks">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"internal">>,<<"chain">>]}},
  'GetStatus' =>
      #{get =>
            #{parameters => [],path => <<"/status">>,
              responses => #{200 => undefined},
              tags => [<<"external">>,<<"node_info">>]}},
  'PostChannelSettle' =>
      #{post =>
            #{parameters => [],path => <<"/debug/channels/settle">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"internal">>,<<"channel">>,<<"debug">>]}},
  'PostOracleRespond' =>
      #{post =>
            #{parameters => [],path => <<"/debug/oracles/respond">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"oracle">>,<<"debug">>]}},
  'PostContractCreate' =>
      #{post =>
            #{parameters => [],path => <<"/debug/contracts/create">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"contract">>,<<"debug">>]}},
  'GetMicroBlockTransactionsByHash' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","hash"},
                    {"description","The hash of the block"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"mh_ZCWcnCG5YF2LhQMTmZ5K5rRmGxatgc5YWxDpGNy2YBAHP6urH">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/micro-blocks/hash/{hash}/transactions">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'GetCurrentKeyBlockHash' =>
      #{get =>
            #{parameters => [],path => <<"/key-blocks/current/hash">>,
              responses => #{200 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'PostNameClaim' =>
      #{post =>
            #{parameters => [],path => <<"/debug/names/claim">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"name_service">>,<<"debug">>]}},
  'PostContractCall' =>
      #{post =>
            #{parameters => [],path => <<"/debug/contracts/call">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"contract">>,<<"debug">>]}},
  'GetPeerPubkey' =>
      #{get =>
            #{parameters => [],path => <<"/peers/pubkey">>,
              responses => #{200 => undefined},
              tags => [<<"external">>,<<"node_info">>]}},
  'PostNameUpdate' =>
      #{post =>
            #{parameters => [],path => <<"/debug/names/update">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"name_service">>,<<"debug">>]}},
  'PostChannelCloseMutual' =>
      #{post =>
            #{parameters => [],path => <<"/debug/channels/close/mutual">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"internal">>,<<"channel">>,<<"debug">>]}},
  'PostChannelDeposit' =>
      #{post =>
            #{parameters => [],path => <<"/debug/channels/deposit">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"internal">>,<<"channel">>,<<"debug">>]}},
  'GetPendingTransactions' =>
      #{get =>
            #{parameters => [],path => <<"/debug/transactions/pending">>,
              responses => #{200 => undefined},
              tags => [<<"internal">>,<<"transaction">>,<<"debug">>]}},
  'GetContractCode' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","The pubkey of the contract"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ct_TV5KbBYdjw1ufKWvAtNNjUnagvRmWMMugFzLKzmLASXB5iH1E">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/contracts/{pubkey}/code">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"contract">>]}},
  'GetOracleByPubkey' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","The public key of the oracle"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ok_24jcHLTZQfsou7NvomRJ1hKEnjyNqbYSq2Az7DmyrAyUHPq8uR">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/oracles/{pubkey}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"oracle">>]}},
  'GetTokenSupplyByHeight' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","height"},
                    {"description","The height"},
                    {"required",true},
                    {"schema",
                     #{<<"maximum">> => 18446744073709552000,
                       <<"minimum">> => 0,<<"type">> => <<"integer">>}}]],
              path => <<"/debug/token-supply/height/{height}">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"internal">>,<<"debug">>]}},
  'GetNodeBeneficiary' =>
      #{get =>
            #{parameters => [],path => <<"/debug/accounts/beneficiary">>,
              responses => #{200 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"node_info">>,<<"debug">>]}},
  'GetCurrentKeyBlockHeight' =>
      #{get =>
            #{parameters => [],path => <<"/key-blocks/current/height">>,
              responses => #{200 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'GetContractPoI' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","Contract pubkey to get proof for"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ct_TV5KbBYdjw1ufKWvAtNNjUnagvRmWMMugFzLKzmLASXB5iH1E">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/contracts/{pubkey}/poi">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"contract">>]}},
  'GetMicroBlockHeaderByHash' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","hash"},
                    {"description","The hash of the block"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"mh_ZCWcnCG5YF2LhQMTmZ5K5rRmGxatgc5YWxDpGNy2YBAHP6urH">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/micro-blocks/hash/{hash}/header">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'PostTransaction' =>
      #{post =>
            #{parameters => [],path => <<"/transactions">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"external">>,<<"transaction">>]}},
  'GetOracleQueryByPubkeyAndQueryId' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","The public key of the oracle"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ok_24jcHLTZQfsou7NvomRJ1hKEnjyNqbYSq2Az7DmyrAyUHPq8uR">>,
                       <<"type">> => <<"string">>}}],
                   [{"in","path"},
                    {"name","query-id"},
                    {"description","The ID of the query"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"oq_q3UrSagF6JfgvAwMiLa6yyEoerx6tQC87m5X8W98NrdDArNZH">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/oracles/{pubkey}/queries/{query-id}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"oracle">>]}},
  'PostOracleQuery' =>
      #{post =>
            #{parameters => [],path => <<"/debug/oracles/query">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"oracle">>,<<"debug">>]}},
  'GetContract' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","The pubkey of the contract"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ct_TV5KbBYdjw1ufKWvAtNNjUnagvRmWMMugFzLKzmLASXB5iH1E">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/contracts/{pubkey}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"contract">>]}},
  'GetPendingKeyBlock' =>
      #{get =>
            #{parameters => [],path => <<"/key-blocks/pending">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'PostChannelCreate' =>
      #{post =>
            #{parameters => [],path => <<"/debug/channels/create">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"channel">>,<<"debug">>]}},
  'GetPendingAccountTransactionsByPubkey' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","The public key of the account"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ak_dArxCkAsk1mZB1L9CX3cdz1GDN4hN84L3Q8dMLHN4v8cU85TF">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/accounts/{pubkey}/transactions/pending">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"account">>]}},
  'DryRunTxs' =>
      #{post =>
            #{parameters => [],path => <<"/debug/transactions/dry-run">>,
              responses => #{200 => undefined,403 => undefined},
              tags => [<<"internal">>,<<"debug">>]}},
  'GetMicroBlockTransactionByHashAndIndex' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","hash"},
                    {"description","The hash of the block"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"mh_ZCWcnCG5YF2LhQMTmZ5K5rRmGxatgc5YWxDpGNy2YBAHP6urH">>,
                       <<"type">> => <<"string">>}}],
                   [{"in","path"},
                    {"name","index"},
                    {"description","The index of the transaction in a block"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> => 1,<<"minimum">> => 1,
                       <<"type">> => <<"integer">>}}]],
              path =>
                  <<"/micro-blocks/hash/{hash}/transactions/index/{index}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'GetMicroBlockTransactionsCountByHash' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","hash"},
                    {"description","The hash of the block"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"mh_ZCWcnCG5YF2LhQMTmZ5K5rRmGxatgc5YWxDpGNy2YBAHP6urH">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/micro-blocks/hash/{hash}/transactions/count">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'PostNameRevoke' =>
      #{post =>
            #{parameters => [],path => <<"/debug/names/revoke">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"name_service">>,<<"debug">>]}},
  'GetGenerationByHeight' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","height"},
                    {"description","The height"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> => 1,
                       <<"maximum">> => 18446744073709552000,
                       <<"minimum">> => 0,<<"type">> => <<"integer">>}}]],
              path => <<"/generations/height/{height}">>,
              responses => #{200 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'GetKeyBlockByHash' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","hash"},
                    {"description","The hash of the block"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"kh_2ikjGFZGFpE99mDtsgkGFsTCqpPpXZRNRa5Pic989FJLcJStgx">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/key-blocks/hash/{hash}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'GetTransactionInfoByHash' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","hash"},
                    {"description","The hash of the transaction"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"th_2w75xjDjHEmphsHDSXrThRnPx6hSUiS7hhSRcuytJABZZ2KkdG">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/transactions/{hash}/info">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"transaction">>]}},
  'GetOracleQueriesByPubkey' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","The public key of the oracle"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ok_24jcHLTZQfsou7NvomRJ1hKEnjyNqbYSq2Az7DmyrAyUHPq8uR">>,
                       <<"type">> => <<"string">>}}],
                   [{"in","query"},
                    {"name","from"},
                    {"description","Last query id in previous page"},
                    {"required",false},
                    {"schema",
                     #{<<"example">> =>
                           <<"oq_q3UrSagF6JfgvAwMiLa6yyEoerx6tQC87m5X8W98NrdDArNZH">>,
                       <<"type">> => <<"string">>}}],
                   [{"in","query"},
                    {"name","limit"},
                    {"description","Max number of oracle queries"},
                    {"required",false},
                    {"schema",
                     #{<<"example">> => 1,<<"maximum">> => 1000,
                       <<"minimum">> => 1,<<"type">> => <<"integer">>}}],
                   [{"in","query"},
                    {"name","type"},
                    {"description","The type of a query: open, closed or all"},
                    {"required",false},
                    {"schema",
                     #{<<"enum">> => [<<"open">>,<<"closed">>,<<"all">>],
                       <<"example">> => <<"open">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/oracles/{pubkey}/queries">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"oracle">>]}},
  'PostChannelWithdraw' =>
      #{post =>
            #{parameters => [],path => <<"/debug/channels/withdraw">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"internal">>,<<"channel">>,<<"debug">>]}},
  'PostOracleRegister' =>
      #{post =>
            #{parameters => [],path => <<"/debug/oracles/register">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"oracle">>,<<"debug">>]}},
  'GetChannelByPubkey' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","The pubkey of the channel"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ch_2tceSwiqxgBcPirX3VYgW3sXgQdJeHjrNWHhLWyfZL7pT4gZF4">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/channels/{pubkey}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"channel">>]}},
  'PostSpend' =>
      #{post =>
            #{parameters => [],path => <<"/debug/transactions/spend">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"transaction">>,<<"debug">>]}},
  'PostChannelSnapshotSolo' =>
      #{post =>
            #{parameters => [],path => <<"/debug/channels/snapshot/solo">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"internal">>,<<"channel">>,<<"debug">>]}},
  'GetPeers' =>
      #{get =>
            #{parameters => [],path => <<"/debug/peers">>,
              responses => #{200 => undefined,403 => undefined},
              tags => [<<"internal">>,<<"node_info">>,<<"debug">>]}},
  'PostOracleExtend' =>
      #{post =>
            #{parameters => [],path => <<"/debug/oracles/extend">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"oracle">>,<<"debug">>]}},
  'GetTopHeader' =>
      #{get =>
            #{parameters => [],path => <<"/headers/top">>,
              responses => #{200 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'GetGenerationByHash' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","hash"},
                    {"description","The hash of the generation"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"kh_2ikjGFZGFpE99mDtsgkGFsTCqpPpXZRNRa5Pic989FJLcJStgx">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/generations/hash/{hash}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'GetAccountByPubkeyAndHash' =>
      #{get =>
            #{parameters =>
                  [[{"in","path"},
                    {"name","pubkey"},
                    {"description","The public key of the account"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"ak_dArxCkAsk1mZB1L9CX3cdz1GDN4hN84L3Q8dMLHN4v8cU85TF">>,
                       <<"type">> => <<"string">>}}],
                   [{"in","path"},
                    {"name","hash"},
                    {"description","The hash of the block"},
                    {"required",true},
                    {"schema",
                     #{<<"example">> =>
                           <<"kh_2ikjGFZGFpE99mDtsgkGFsTCqpPpXZRNRa5Pic989FJLcJStgx">>,
                       <<"type">> => <<"string">>}}]],
              path => <<"/accounts/{pubkey}/hash/{hash}">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"external">>,<<"account">>]}},
  'GetNodePubkey' =>
      #{get =>
            #{parameters => [],path => <<"/debug/accounts/node">>,
              responses => #{200 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"node_info">>,<<"debug">>]}},
  'GetCurrentGeneration' =>
      #{get =>
            #{parameters => [],path => <<"/generations/current">>,
              responses => #{200 => undefined,404 => undefined},
              tags => [<<"external">>,<<"chain">>]}},
  'PostNamePreclaim' =>
      #{post =>
            #{parameters => [],path => <<"/debug/names/preclaim">>,
              responses =>
                  #{200 => undefined,400 => undefined,404 => undefined},
              tags => [<<"internal">>,<<"name_service">>,<<"debug">>]}},
  'PostChannelSlash' =>
      #{post =>
            #{parameters => [],path => <<"/debug/channels/slash">>,
              responses => #{200 => undefined,400 => undefined},
              tags => [<<"internal">>,<<"channel">>,<<"debug">>]}}}.

definitions() ->
    [].

json_schema() ->
    maps:merge(#{<<"$schema">> =>
                     <<"http://json-schema.org/draft-04/schema#">>},
               #{<<"properties">> =>
                     maps:from_list([{list_to_binary(K), V}
                                     || {K, V} <- definitions()])}).

operation(Id) -> maps:get(Id, operations()).

path(Method, OperationId, Args) when is_map(Args) ->
    path(Method, OperationId, maps:to_list(Args));
path(Method, OperationId, Args) ->
    begin
        #{path := Endpoint, parameters := Parameters} =
            maps:get(Method, operation(OperationId)),
        InPath = [Param
                  || Param <- Parameters,
                     lists:member({"in", "path"}, Param)],
        lists:foldl(fun (Param, Path) ->
                            Name = proplists:get_value("name", Param),
                            case {proplists:get_value("required", Param, false),
                                  get_by_similar_key(Name, Args)}
                                of
                                {false, undefined} -> Path;
                                {true, undefined} ->
                                    throw({error,
                                           {required,
                                            Name,
                                            Param,
                                            OperationId}});
                                {_, {_Key, Value}} ->
                                    iolist_to_binary(string:replace(Path,
                                                                    "{" ++
                                                                        Name ++
                                                                            "}",
                                                                    to_str(Value)))
                            end
                    end,
                    Endpoint,
                    InPath)
    end.

query(Method, OperationId, Args) when is_map(Args) ->
    query(Method, OperationId, maps:to_list(Args));
query(Method, OperationId, Args) ->
    begin
        #{parameters := Parameters} = maps:get(Method,
                                               operation(OperationId)),
        InQuery = [Param
                   || Param <- Parameters,
                      lists:member({"in", "query"}, Param)],
        Query = lists:foldr(fun (Param, Query) ->
                                    Name = proplists:get_value("name", Param),
                                    case {proplists:get_value("required",
                                                              Param,
                                                              false),
                                          get_by_similar_key(Name, Args)}
                                        of
                                        {false, undefined} -> Query;
                                        {true, undefined} ->
                                            throw({error,
                                                   {required,
                                                    Name,
                                                    Param,
                                                    OperationId}});
                                        {_, {_, Value}} ->
                                            [{Name,
                                              http_uri:encode(to_str(Value))}
                                             | Query]
                                    end
                            end,
                            [],
                            InQuery),
        case [[K, "=", V] || {K, V} <- Query] of
            [] -> <<>>;
            Qs -> iolist_to_binary(["?" | lists:join("&", Qs)])
        end
    end.

prepare_validation() ->
    case ets:info(jesse_ets) of
        undefined ->
            [jesse:add_schema(Def, Schema)
             || {Def, Schema} <- definitions()];
        _ -> []
    end.

validate(Schema, Term) ->
    try jesse_schema_validator:validate(Schema, Term, [])
    catch
        Error -> {error, Error}
    end.

validate_request(OperationId, Method, Args)
    when is_map(Args) ->
    validate_request(OperationId,
                     Method,
                     maps:to_list(Args));
validate_request(OperationId, Method, Args)
    when is_list(Args) ->
    begin
        prepare_validation(),
        #{parameters := Parameters} = maps:get(Method,
                                               endpoints:operation(OperationId)),
        ToCheck = [Param
                   || Param <- Parameters,
                      not lists:member({"in", "path"}, Param)],
        Errors = lists:foldl(fun (Param, Errs) ->
                                     Name = proplists:get_value("name", Param),
                                     case {proplists:get_value("required",
                                                               Param,
                                                               false),
                                           get_by_similar_key(Name, Args)}
                                         of
                                         {false, undefined} -> Errs;
                                         {true, undefined} ->
                                             [{required,
                                               Name,
                                               Param,
                                               OperationId}
                                              | Errs];
                                         {_, {_, Value}} ->
                                             case
                                                 validate(proplists:get_value("schema",
                                                                              Param,
                                                                              #{}),
                                                          Value)
                                                 of
                                                 {error, E} -> [E | Errs];
                                                 _ -> Errs
                                             end
                                     end
                             end,
                             [],
                             ToCheck),
        case Errors of
            [] -> ok;
            _ -> {errors, {OperationId, Args, Errors}}
        end
    end.

validate_response(OperationId, Method, StatusCode,
                  Response) ->
    begin
        #{responses := Resps} = maps:get(Method,
                                         endpoints:operation(OperationId)),
        prepare_validation(),
        case maps:get(StatusCode, Resps, error) of
            error -> {error, {StatusCode, unspecified}};
            undefined -> {ok, StatusCode, Response};
            Schema ->
                case validate(Schema, Response) of
                    {ok, _} -> {ok, StatusCode, Response};
                    {error, E} -> {error, {validation, E}}
                end
        end
    end.

get_by_similar_key(Name, KVs) when is_list(Name) ->
    case lists:keyfind(Name, 1, KVs) of
        false ->
            case lists:keyfind(list_to_binary(Name), 1, KVs) of
                false ->
                    case lists:keyfind(catch list_to_existing_atom(Name),
                                       1,
                                       KVs)
                        of
                        false -> undefined;
                        AtomTuple -> AtomTuple
                    end;
                BinTuple -> BinTuple
            end;
        Tuple -> Tuple
    end.

to_str(Str) ->
    begin
        if is_list(Str); is_binary(Str) -> Str;
           true -> lists:concat([Str])
        end
    end.