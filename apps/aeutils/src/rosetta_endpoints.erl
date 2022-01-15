%% This code is generated from apps/aehttp/priv/rosetta.yaml
%% Using swagger_endpoints rebar3 plugin version: 0.2.3
%% Do not manually change this code!
%%
%% json_schema/0 implements a JSON Schema for the definitions
%% Reference should be fixed!
%% Use jsx:prettify(jsx:encode(json_schema())) to get a JSON string.

-module(rosetta_endpoints).

-export([operation/1, operations/0, definitions_prefix/0, definitions/0, json_schema/0,
         validate_request/3, validate_response/4, path/3, query/3,
         validate/2]).

operations() ->
    #{accountBalance =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/AccountBalanceRequest">>}}]],
        path => <<"/account/balance">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/AccountBalanceResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Account">>]},
  accountCoins =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/AccountCoinsRequest">>}}]],
        path => <<"/account/coins">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/AccountCoinsResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Account">>]},
  block =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> => <<"/components/schemas/BlockRequest">>}}]],
        path => <<"/block">>,
        responses =>
            #{200 => #{<<"$ref">> => <<"/components/schemas/BlockResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Block">>]},
  blockTransaction =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/BlockTransactionRequest">>}}]],
        path => <<"/block/transaction">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/BlockTransactionResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Block">>]},
  call =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> => <<"/components/schemas/CallRequest">>}}]],
        path => <<"/call">>,
        responses =>
            #{200 => #{<<"$ref">> => <<"/components/schemas/CallResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Call">>]},
  constructionCombine =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/ConstructionCombineRequest">>}}]],
        path => <<"/construction/combine">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/ConstructionCombineResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Construction">>]},
  constructionDerive =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/ConstructionDeriveRequest">>}}]],
        path => <<"/construction/derive">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/ConstructionDeriveResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Construction">>]},
  constructionHash =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/ConstructionHashRequest">>}}]],
        path => <<"/construction/hash">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/TransactionIdentifierResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Construction">>]},
  constructionMetadata =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/ConstructionMetadataRequest">>}}]],
        path => <<"/construction/metadata">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/ConstructionMetadataResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Construction">>]},
  constructionParse =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/ConstructionParseRequest">>}}]],
        path => <<"/construction/parse">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/ConstructionParseResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Construction">>]},
  constructionPayloads =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/ConstructionPayloadsRequest">>}}]],
        path => <<"/construction/payloads">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/ConstructionPayloadsResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Construction">>]},
  constructionPreprocess =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/ConstructionPreprocessRequest">>}}]],
        path => <<"/construction/preprocess">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/ConstructionPreprocessResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Construction">>]},
  constructionSubmit =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/ConstructionSubmitRequest">>}}]],
        path => <<"/construction/submit">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/TransactionIdentifierResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Construction">>]},
  eventsBlocks =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/EventsBlocksRequest">>}}]],
        path => <<"/events/blocks">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/EventsBlocksResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Events">>]},
  mempool =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> => <<"/components/schemas/NetworkRequest">>}}]],
        path => <<"/mempool">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> => <<"/components/schemas/MempoolResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Mempool">>]},
  mempoolTransaction =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/MempoolTransactionRequest">>}}]],
        path => <<"/mempool/transaction">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/MempoolTransactionResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Mempool">>]},
  networkList =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> => <<"/components/schemas/MetadataRequest">>}}]],
        path => <<"/network/list">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/NetworkListResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Network">>]},
  networkOptions =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> => <<"/components/schemas/NetworkRequest">>}}]],
        path => <<"/network/options">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/NetworkOptionsResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Network">>]},
  networkStatus =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> => <<"/components/schemas/NetworkRequest">>}}]],
        path => <<"/network/status">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/NetworkStatusResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Network">>]},
  searchTransactions =>
      #{method => "post",
        parameters =>
            [[{"in","body"},
              {"name","body"},
              {"required",true},
              {"schema",
               #{<<"$ref">> =>
                     <<"/components/schemas/SearchTransactionsRequest">>}}]],
        path => <<"/search/transactions">>,
        responses =>
            #{200 =>
                  #{<<"$ref">> =>
                        <<"/components/schemas/SearchTransactionsResponse">>},
              500 => #{<<"$ref">> => <<"/components/schemas/Error">>}},
        tags => [<<"Search">>]}}.

definitions_prefix() ->
    "/components/schemas/".

definitions() ->
    [{"/components/schemas/Error",#{<<"$ref">> => <<"models/Error.yaml">>}},
 {"/components/schemas/SearchTransactionsResponse",
  #{<<"description">> =>
        <<"SearchTransactionsResponse contains an ordered collection of BlockTransactions\nthat match the query in SearchTransactionsRequest. These BlockTransactions\nare sorted from most recent block to oldest block.\n">>,
    <<"properties">> =>
        #{<<"next_offset">> =>
              #{<<"description">> =>
                    <<"next_offset is the next offset to use when paginating through\ntransaction results. If this field is not populated, there are\nno more transactions to query.\n">>,
                <<"example">> => 5,<<"format">> => <<"int64">>,
                <<"minimum">> => 0,<<"type">> => <<"integer">>},
          <<"total_count">> =>
              #{<<"description">> =>
                    <<"total_count is the number of results for a given search. Callers\ntypically use this value to concurrently fetch results by offset\nor to display a virtual page number associated with results.\n">>,
                <<"example">> => 5,<<"format">> => <<"int64">>,
                <<"minimum">> => 0,<<"type">> => <<"integer">>},
          <<"transactions">> =>
              #{<<"description">> =>
                    <<"transactions is an array of BlockTransactions sorted by most recent\nBlockIdentifier (meaning that transactions in recent blocks appear\nfirst).\n\nIf there are many transactions for a particular search, transactions\nmay not contain all matching transactions. It is up to the caller to\npaginate these transactions using the max_block field.\n">>,
                <<"items">> =>
                    #{<<"$ref">> =>
                          <<"/components/schemas/BlockTransaction">>},
                <<"type">> => <<"array">>}},
    <<"required">> => [<<"transactions">>,<<"total_count">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/SearchTransactionsRequest",
  #{<<"description">> =>
        <<"SearchTransactionsRequest is used to search for transactions\nmatching a set of provided conditions in canonical blocks.\n">>,
    <<"properties">> =>
        #{<<"account_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/AccountIdentifier">>},
          <<"address">> =>
              #{<<"description">> =>
                    <<"address is AccountIdentifier.Address. This is used to get all\ntransactions related to an AccountIdentifier.Address, regardless\nof SubAccountIdentifier.\n">>,
                <<"example">> =>
                    <<"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347">>,
                <<"type">> => <<"string">>},
          <<"coin_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/CoinIdentifier">>},
          <<"currency">> =>
              #{<<"$ref">> => <<"/components/schemas/Currency">>},
          <<"limit">> =>
              #{<<"description">> =>
                    <<"limit is the maximum number of transactions to return in one call. The implementation\nmay return <= limit transactions.\n">>,
                <<"example">> => 5,<<"format">> => <<"int64">>,
                <<"minimum">> => 0,<<"type">> => <<"integer">>},
          <<"max_block">> =>
              #{<<"description">> =>
                    <<"max_block is the largest block index to consider when searching\nfor transactions. If this field is not populated, the current\nblock is considered the max_block.\n\nIf you do not specify a max_block, it is possible a newly synced\nblock will interfere with paginated transaction queries (as the offset\ncould become invalid with newly added rows).\n">>,
                <<"example">> => 5,<<"format">> => <<"int64">>,
                <<"minimum">> => 0,<<"type">> => <<"integer">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"offset">> =>
              #{<<"description">> =>
                    <<"offset is the offset into the query result to start returning transactions.\n\nIf any search conditions are changed, the query offset will change and you\nmust restart your search iteration.\n">>,
                <<"example">> => 5,<<"format">> => <<"int64">>,
                <<"minimum">> => 0,<<"type">> => <<"integer">>},
          <<"operator">> =>
              #{<<"$ref">> => <<"/components/schemas/Operator">>},
          <<"status">> =>
              #{<<"description">> =>
                    <<"status is the network-specific operation type.\n">>,
                <<"example">> => <<"reverted">>,<<"type">> => <<"string">>},
          <<"success">> =>
              #{<<"description">> =>
                    <<"success is a synthetic condition populated by parsing network-specific\noperation statuses (using the mapping provided in `/network/options`).\n">>,
                <<"type">> => <<"boolean">>},
          <<"transaction_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/TransactionIdentifier">>},
          <<"type">> =>
              #{<<"description">> =>
                    <<"type is the network-specific operation type.\n">>,
                <<"example">> => <<"transfer">>,<<"type">> => <<"string">>}},
    <<"required">> => [<<"network_identifier">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/EventsBlocksResponse",
  #{<<"description">> =>
        <<"EventsBlocksResponse contains an ordered collection of BlockEvents\nand the max retrievable sequence.\n">>,
    <<"properties">> =>
        #{<<"events">> =>
              #{<<"description">> =>
                    <<"events is an array of BlockEvents indicating the order to add\nand remove blocks to maintain a canonical view of blockchain\nstate. Lightweight clients can use this event stream to update\nstate without implementing their own block syncing logic.\n">>,
                <<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/BlockEvent">>},
                <<"type">> => <<"array">>},
          <<"max_sequence">> =>
              #{<<"description">> =>
                    <<"max_sequence is the maximum available sequence number to fetch.\n">>,
                <<"example">> => 5,<<"format">> => <<"int64">>,
                <<"minimum">> => 0,<<"type">> => <<"integer">>}},
    <<"required">> => [<<"max_sequence">>,<<"events">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/EventsBlocksRequest",
  #{<<"description">> =>
        <<"EventsBlocksRequest is utilized to fetch a sequence of BlockEvents\nindicating which blocks were added and removed from storage to\nreach the current state.\n">>,
    <<"properties">> =>
        #{<<"limit">> =>
              #{<<"description">> =>
                    <<"limit is the maximum number of events to fetch in one call. The implementation\nmay return <= limit events.\n">>,
                <<"example">> => 5,<<"format">> => <<"int64">>,
                <<"minimum">> => 0,<<"type">> => <<"integer">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"offset">> =>
              #{<<"description">> =>
                    <<"offset is the offset into the event stream to sync events from. If this\nfield is not populated, we return the limit events backwards from tip.\nIf this is set to 0, we start from the beginning.\n">>,
                <<"example">> => 5,<<"format">> => <<"int64">>,
                <<"minimum">> => 0,<<"type">> => <<"integer">>}},
    <<"required">> => [<<"network_identifier">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/CallResponse",
  #{<<"description">> =>
        <<"CallResponse contains the result of a `/call` invocation.\n">>,
    <<"properties">> =>
        #{<<"idempotent">> =>
              #{<<"description">> =>
                    <<"Idempotent indicates that if `/call` is invoked with the same\nCallRequest again, at any point in time, it will return the same\nCallResponse.\n\nIntegrators may cache the CallResponse if this is set to true\nto avoid making unnecessary calls to the Rosetta implementation. For\nthis reason, implementers should be very conservative about returning\ntrue here or they could cause issues for the caller.\n">>,
                <<"type">> => <<"boolean">>},
          <<"result">> =>
              #{<<"description">> =>
                    <<"Result contains the result of the `/call` invocation. This result\nwill not be inspected or interpreted by Rosetta tooling and is\nleft to the caller to decode.\n">>,
                <<"example">> => #{<<"count">> => 1000},
                <<"type">> => <<"object">>}},
    <<"required">> => [<<"result">>,<<"idempotent">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/CallRequest",
  #{<<"description">> =>
        <<"CallRequest is the input to the `/call` endpoint.\n">>,
    <<"properties">> =>
        #{<<"method">> =>
              #{<<"description">> =>
                    <<"Method is some network-specific procedure call. This method could\nmap to a network-specific RPC endpoint, a method in an SDK generated\nfrom a smart contract, or some hybrid of the two.\n\nThe implementation must define all available methods in the\nAllow object. However, it is up to the caller to determine\nwhich parameters to provide when invoking `/call`.\n">>,
                <<"example">> => <<"eth_call">>,<<"type">> => <<"string">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"parameters">> =>
              #{<<"description">> =>
                    <<"Parameters is some network-specific argument for a method. It is\nup to the caller to determine which parameters to provide when invoking\n`/call`.\n">>,
                <<"example">> =>
                    #{<<"address">> =>
                          <<"0x52bc44d5378309ee2abf1539bf71de1b7d7be3b5">>,
                      <<"block_number">> => 23},
                <<"type">> => <<"object">>}},
    <<"required">> => [<<"network_identifier">>,<<"method">>,<<"parameters">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/TransactionIdentifierResponse",
  #{<<"description">> =>
        <<"TransactionIdentifierResponse contains the transaction_identifier of a\ntransaction that was submitted to either `/construction/hash` or\n`/construction/submit`.\n">>,
    <<"properties">> =>
        #{<<"metadata">> => #{<<"type">> => <<"object">>},
          <<"transaction_identifier">> =>
              #{<<"$ref">> =>
                    <<"/components/schemas/TransactionIdentifier">>}},
    <<"required">> => [<<"transaction_identifier">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionSubmitRequest",
  #{<<"description">> =>
        <<"The transaction submission request includes a signed transaction.\n">>,
    <<"properties">> =>
        #{<<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"signed_transaction">> => #{<<"type">> => <<"string">>}},
    <<"required">> => [<<"network_identifier">>,<<"signed_transaction">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionHashRequest",
  #{<<"description">> =>
        <<"ConstructionHashRequest is the input to the `/construction/hash` endpoint.\n">>,
    <<"properties">> =>
        #{<<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"signed_transaction">> => #{<<"type">> => <<"string">>}},
    <<"required">> => [<<"network_identifier">>,<<"signed_transaction">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionParseResponse",
  #{<<"description">> =>
        <<"ConstructionParseResponse contains an array of operations that occur in\na transaction blob. This should match the array of operations provided\nto `/construction/preprocess` and `/construction/payloads`.\n">>,
    <<"properties">> =>
        #{<<"account_identifier_signers">> =>
              #{<<"items">> =>
                    #{<<"$ref">> =>
                          <<"/components/schemas/AccountIdentifier">>},
                <<"type">> => <<"array">>},
          <<"metadata">> => #{<<"type">> => <<"object">>},
          <<"operations">> =>
              #{<<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/Operation">>},
                <<"type">> => <<"array">>},
          <<"signers">> =>
              #{<<"description">> =>
                    <<"[DEPRECATED by `account_identifier_signers` in `v1.4.4`] All signers (addresses) of a particular transaction. If the transaction\nis unsigned, it should be empty.\n">>,
                <<"items">> => #{<<"type">> => <<"string">>},
                <<"type">> => <<"array">>}},
    <<"required">> => [<<"operations">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionParseRequest",
  #{<<"description">> =>
        <<"ConstructionParseRequest is the input to the `/construction/parse`\nendpoint. It allows the caller to parse either an unsigned or\nsigned transaction.\n">>,
    <<"properties">> =>
        #{<<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"signed">> =>
              #{<<"description">> =>
                    <<"Signed is a boolean indicating whether the transaction is signed.\n">>,
                <<"type">> => <<"boolean">>},
          <<"transaction">> =>
              #{<<"description">> =>
                    <<"This must be either the unsigned transaction blob returned by\n`/construction/payloads` or the signed transaction blob\nreturned by `/construction/combine`.\n">>,
                <<"type">> => <<"string">>}},
    <<"required">> =>
        [<<"network_identifier">>,<<"signed">>,<<"transaction">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionCombineResponse",
  #{<<"description">> =>
        <<"ConstructionCombineResponse is returned by `/construction/combine`.\nThe network payload will be sent directly to the\n`construction/submit` endpoint.\n">>,
    <<"properties">> =>
        #{<<"signed_transaction">> => #{<<"type">> => <<"string">>}},
    <<"required">> => [<<"signed_transaction">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionCombineRequest",
  #{<<"description">> =>
        <<"ConstructionCombineRequest is the input to the `/construction/combine`\nendpoint. It contains the unsigned transaction blob returned by\n`/construction/payloads` and all required signatures to create\na network transaction.\n">>,
    <<"properties">> =>
        #{<<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"signatures">> =>
              #{<<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/Signature">>},
                <<"type">> => <<"array">>},
          <<"unsigned_transaction">> => #{<<"type">> => <<"string">>}},
    <<"required">> =>
        [<<"network_identifier">>,<<"unsigned_transaction">>,<<"signatures">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionPayloadsResponse",
  #{<<"description">> =>
        <<"ConstructionTransactionResponse is returned by `/construction/payloads`. It\ncontains an unsigned transaction blob (that is usually needed to construct\nthe a network transaction from a collection of signatures) and an\narray of payloads that must be signed by the caller.\n">>,
    <<"properties">> =>
        #{<<"payloads">> =>
              #{<<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/SigningPayload">>},
                <<"type">> => <<"array">>},
          <<"unsigned_transaction">> => #{<<"type">> => <<"string">>}},
    <<"required">> => [<<"unsigned_transaction">>,<<"payloads">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionPayloadsRequest",
  #{<<"description">> =>
        <<"ConstructionPayloadsRequest is the request to\n`/construction/payloads`. It contains the network,\na slice of operations, and arbitrary metadata\nthat was returned by the call to `/construction/metadata`.\n\nOptionally, the request can also include an array\nof PublicKeys associated with the AccountIdentifiers\nreturned in ConstructionPreprocessResponse.\n">>,
    <<"properties">> =>
        #{<<"metadata">> => #{<<"type">> => <<"object">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"operations">> =>
              #{<<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/Operation">>},
                <<"type">> => <<"array">>},
          <<"public_keys">> =>
              #{<<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/PublicKey">>},
                <<"type">> => <<"array">>}},
    <<"required">> => [<<"network_identifier">>,<<"operations">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionPreprocessResponse",
  #{<<"description">> =>
        <<"ConstructionPreprocessResponse contains `options` that will\nbe sent unmodified to `/construction/metadata`. If it is\nnot necessary to make a request to `/construction/metadata`,\n`options` should be omitted. \n\nSome blockchains require the PublicKey of particular AccountIdentifiers\nto construct a valid transaction. To fetch these PublicKeys, populate\n`required_public_keys` with the AccountIdentifiers associated with the desired\nPublicKeys. If it is not necessary to retrieve any PublicKeys\nfor construction, `required_public_keys` should be omitted.\n">>,
    <<"properties">> =>
        #{<<"options">> =>
              #{<<"description">> =>
                    <<"The options that will be sent directly to `/construction/metadata` by\nthe caller.\n">>,
                <<"type">> => <<"object">>},
          <<"required_public_keys">> =>
              #{<<"items">> =>
                    #{<<"$ref">> =>
                          <<"/components/schemas/AccountIdentifier">>},
                <<"type">> => <<"array">>}},
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionPreprocessRequest",
  #{<<"description">> =>
        <<"ConstructionPreprocessRequest is passed to the `/construction/preprocess`\nendpoint so that a Rosetta implementation can determine which\nmetadata it needs to request for construction.\n\nMetadata provided in this object should NEVER be a product\nof live data (i.e. the caller must follow some network-specific\ndata fetching strategy outside of the Construction API to populate\nrequired Metadata). If live data is required for construction, it MUST\nbe fetched in the call to `/construction/metadata`.\n\nThe caller can provide a max fee they are willing\nto pay for a transaction. This is an array in the case fees\nmust be paid in multiple currencies.\n\nThe caller can also provide a suggested fee multiplier\nto indicate that the suggested fee should be scaled.\nThis may be used to set higher fees for urgent transactions\nor to pay lower fees when there is less urgency. It is assumed\nthat providing a very low multiplier (like 0.0001) will\nnever lead to a transaction being created with a fee\nless than the minimum network fee (if applicable).\n\nIn the case that the caller provides both a max fee\nand a suggested fee multiplier, the max fee will set an\nupper bound on the suggested fee (regardless of the\nmultiplier provided).\n">>,
    <<"properties">> =>
        #{<<"max_fee">> =>
              #{<<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/Amount">>},
                <<"type">> => <<"array">>},
          <<"metadata">> => #{<<"type">> => <<"object">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"operations">> =>
              #{<<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/Operation">>},
                <<"type">> => <<"array">>},
          <<"suggested_fee_multiplier">> =>
              #{<<"format">> => <<"double">>,<<"minimum">> => 0.0,
                <<"type">> => <<"number">>}},
    <<"required">> => [<<"network_identifier">>,<<"operations">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionDeriveResponse",
  #{<<"description">> =>
        <<"ConstructionDeriveResponse is returned by the `/construction/derive`\nendpoint.\n">>,
    <<"properties">> =>
        #{<<"account_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/AccountIdentifier">>},
          <<"address">> =>
              #{<<"description">> =>
                    <<"[DEPRECATED by `account_identifier` in `v1.4.4`] Address in network-specific format.\n">>,
                <<"type">> => <<"string">>},
          <<"metadata">> => #{<<"type">> => <<"object">>}},
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionDeriveRequest",
  #{<<"description">> =>
        <<"ConstructionDeriveRequest is passed to the `/construction/derive`\nendpoint. Network is provided in the request because some blockchains\nhave different address formats for different networks.\nMetadata is provided in the request because some blockchains\nallow for multiple address types (i.e. different address\nfor validators vs normal accounts).\n">>,
    <<"properties">> =>
        #{<<"metadata">> => #{<<"type">> => <<"object">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"public_key">> =>
              #{<<"$ref">> => <<"/components/schemas/PublicKey">>}},
    <<"required">> => [<<"network_identifier">>,<<"public_key">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionMetadataResponse",
  #{<<"description">> =>
        <<"The ConstructionMetadataResponse returns network-specific metadata\nused for transaction construction.\n\nOptionally, the implementer can return the suggested fee associated\nwith the transaction being constructed. The caller may use this info\nto adjust the intent of the transaction or to create a transaction with\na different account that can pay the suggested fee. Suggested fee is an array\nin case fee payment must occur in multiple currencies.\n">>,
    <<"properties">> =>
        #{<<"metadata">> =>
              #{<<"example">> =>
                    #{<<"account_sequence">> => 23,
                      <<"recent_block_hash">> =>
                          <<"0x52bc44d5378309ee2abf1539bf71de1b7d7be3b5">>},
                <<"type">> => <<"object">>},
          <<"suggested_fee">> =>
              #{<<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/Amount">>},
                <<"type">> => <<"array">>}},
    <<"required">> => [<<"metadata">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/ConstructionMetadataRequest",
  #{<<"description">> =>
        <<"A ConstructionMetadataRequest is utilized to get information required\nto construct a transaction.\n\nThe Options object used to specify which metadata to return is left\npurposely unstructured to allow flexibility for implementers. Options\nis not required in the case that there is network-wide metadata of\ninterest.\n\nOptionally, the request can also include an array\nof PublicKeys associated with the AccountIdentifiers\nreturned in ConstructionPreprocessResponse.\n">>,
    <<"properties">> =>
        #{<<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"options">> =>
              #{<<"description">> =>
                    <<"Some blockchains require different metadata for different types of\ntransaction construction (ex: delegation versus a transfer). Instead\nof requiring a blockchain node to return all possible types of\nmetadata for construction (which may require multiple node fetches),\nthe client can populate an options object to limit the metadata\nreturned to only the subset required.\n">>,
                <<"type">> => <<"object">>},
          <<"public_keys">> =>
              #{<<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/PublicKey">>},
                <<"type">> => <<"array">>}},
    <<"required">> => [<<"network_identifier">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/NetworkOptionsResponse",
  #{<<"description">> =>
        <<"NetworkOptionsResponse contains information about the versioning of the\nnode and the allowed operation statuses, operation types, and errors.\n">>,
    <<"properties">> =>
        #{<<"allow">> => #{<<"$ref">> => <<"/components/schemas/Allow">>},
          <<"version">> => #{<<"$ref">> => <<"/components/schemas/Version">>}},
    <<"required">> => [<<"version">>,<<"allow">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/NetworkStatusResponse",
  #{<<"description">> =>
        <<"NetworkStatusResponse contains basic information about the node's\nview of a blockchain network. It is assumed that any BlockIdentifier.Index\nless than or equal to CurrentBlockIdentifier.Index can be queried.\n\nIf a Rosetta implementation prunes historical state, it should\npopulate the optional `oldest_block_identifier` field with the\noldest block available to query. If this is not populated,\nit is assumed that the `genesis_block_identifier` is the oldest\nqueryable block.\n\nIf a Rosetta implementation performs some pre-sync before it is\npossible to query blocks, sync_status should be populated so that\nclients can still monitor healthiness. Without this field, it may\nappear that the implementation is stuck syncing and needs to be\nterminated.\n">>,
    <<"properties">> =>
        #{<<"current_block_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/BlockIdentifier">>},
          <<"current_block_timestamp">> =>
              #{<<"$ref">> => <<"/components/schemas/Timestamp">>},
          <<"genesis_block_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/BlockIdentifier">>},
          <<"oldest_block_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/BlockIdentifier">>},
          <<"peers">> =>
              #{<<"items">> => #{<<"$ref">> => <<"/components/schemas/Peer">>},
                <<"type">> => <<"array">>},
          <<"sync_status">> =>
              #{<<"$ref">> => <<"/components/schemas/SyncStatus">>}},
    <<"required">> =>
        [<<"current_block_identifier">>,<<"current_block_timestamp">>,
         <<"genesis_block_identifier">>,<<"peers">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/NetworkRequest",
  #{<<"description">> =>
        <<"A NetworkRequest is utilized to retrieve some data specific exclusively\nto a NetworkIdentifier.\n">>,
    <<"properties">> =>
        #{<<"metadata">> => #{<<"type">> => <<"object">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>}},
    <<"required">> => [<<"network_identifier">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/NetworkListResponse",
  #{<<"description">> =>
        <<"A NetworkListResponse contains all NetworkIdentifiers\nthat the node can serve information for.\n">>,
    <<"properties">> =>
        #{<<"network_identifiers">> =>
              #{<<"items">> =>
                    #{<<"$ref">> =>
                          <<"/components/schemas/NetworkIdentifier">>},
                <<"type">> => <<"array">>}},
    <<"required">> => [<<"network_identifiers">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/MetadataRequest",
  #{<<"description">> =>
        <<"A MetadataRequest is utilized in any request where\nthe only argument is optional metadata.\n">>,
    <<"properties">> => #{<<"metadata">> => #{<<"type">> => <<"object">>}},
    <<"type">> => <<"object">>}},
 {"/components/schemas/MempoolTransactionResponse",
  #{<<"description">> =>
        <<"A MempoolTransactionResponse contains an estimate of a mempool\ntransaction. It may not be possible to know the full impact of\na transaction in the mempool (ex: fee paid).\n">>,
    <<"properties">> =>
        #{<<"metadata">> =>
              #{<<"example">> =>
                    #{<<"ancestor_count">> => 2,
                      <<"descendant_fees">> => 123923},
                <<"type">> => <<"object">>},
          <<"transaction">> =>
              #{<<"$ref">> => <<"/components/schemas/Transaction">>}},
    <<"required">> => [<<"transaction">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/MempoolTransactionRequest",
  #{<<"description">> =>
        <<"A MempoolTransactionRequest is utilized to retrieve a transaction\nfrom the mempool.\n">>,
    <<"properties">> =>
        #{<<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"transaction_identifier">> =>
              #{<<"$ref">> =>
                    <<"/components/schemas/TransactionIdentifier">>}},
    <<"required">> => [<<"network_identifier">>,<<"transaction_identifier">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/MempoolResponse",
  #{<<"description">> =>
        <<"A MempoolResponse contains all transaction identifiers in the mempool\nfor a particular network_identifier.\n">>,
    <<"properties">> =>
        #{<<"transaction_identifiers">> =>
              #{<<"items">> =>
                    #{<<"$ref">> =>
                          <<"/components/schemas/TransactionIdentifier">>},
                <<"type">> => <<"array">>}},
    <<"required">> => [<<"transaction_identifiers">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/BlockTransactionResponse",
  #{<<"description">> =>
        <<"A BlockTransactionResponse contains information about a block transaction.\n">>,
    <<"properties">> =>
        #{<<"transaction">> =>
              #{<<"$ref">> => <<"/components/schemas/Transaction">>}},
    <<"required">> => [<<"transaction">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/BlockTransactionRequest",
  #{<<"description">> =>
        <<"A BlockTransactionRequest is used to fetch a Transaction included in a\nblock that is not returned in a BlockResponse.\n">>,
    <<"properties">> =>
        #{<<"block_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/BlockIdentifier">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>},
          <<"transaction_identifier">> =>
              #{<<"$ref">> =>
                    <<"/components/schemas/TransactionIdentifier">>}},
    <<"required">> =>
        [<<"network_identifier">>,<<"block_identifier">>,
         <<"transaction_identifier">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/BlockResponse",
  #{<<"description">> =>
        <<"A BlockResponse includes a fully-populated block or a partially-populated\nblock with a list of other transactions to fetch (other_transactions).\n\nAs a result of the consensus algorithm of some blockchains, blocks\ncan be omitted (i.e. certain block indices can be skipped). If a query\nfor one of these omitted indices is made, the response should not include\na `Block` object.\n\nIt is VERY important to note that blocks MUST still form a canonical,\nconnected chain of blocks where each block has a unique index. In other words,\nthe `PartialBlockIdentifier` of a block after an omitted block should\nreference the last non-omitted block.\n">>,
    <<"properties">> =>
        #{<<"block">> => #{<<"$ref">> => <<"/components/schemas/Block">>},
          <<"other_transactions">> =>
              #{<<"description">> =>
                    <<"Some blockchains may require additional transactions to be fetched\nthat weren't returned in the block response\n(ex: block only returns transaction hashes). For blockchains with a\nlot of transactions in each block, this\ncan be very useful as consumers can concurrently fetch all\ntransactions returned.\n">>,
                <<"items">> =>
                    #{<<"$ref">> =>
                          <<"/components/schemas/TransactionIdentifier">>},
                <<"type">> => <<"array">>}},
    <<"type">> => <<"object">>}},
 {"/components/schemas/BlockRequest",
  #{<<"description">> =>
        <<"A BlockRequest is utilized to make a block request on the\n/block endpoint.\n">>,
    <<"properties">> =>
        #{<<"block_identifier">> =>
              #{<<"$ref">> =>
                    <<"/components/schemas/PartialBlockIdentifier">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>}},
    <<"required">> => [<<"network_identifier">>,<<"block_identifier">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/AccountCoinsResponse",
  #{<<"description">> =>
        <<"AccountCoinsResponse is returned on the /account/coins endpoint and includes\nall unspent Coins owned by an AccountIdentifier.\n">>,
    <<"properties">> =>
        #{<<"block_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/BlockIdentifier">>},
          <<"coins">> =>
              #{<<"description">> =>
                    <<"If a blockchain is UTXO-based, all unspent Coins owned by an account_identifier\nshould be returned alongside the balance. It is highly recommended to\npopulate this field so that users of the Rosetta API implementation\ndon't need to maintain their own indexer to track their UTXOs.\n">>,
                <<"items">> => #{<<"$ref">> => <<"/components/schemas/Coin">>},
                <<"type">> => <<"array">>},
          <<"metadata">> =>
              #{<<"description">> =>
                    <<"Account-based blockchains that utilize a nonce or sequence number\nshould include that number in the metadata. This number could be\nunique to the identifier or global across the account address.\n">>,
                <<"example">> => #{<<"sequence_number">> => 23},
                <<"type">> => <<"object">>}},
    <<"required">> => [<<"block_identifier">>,<<"coins">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/AccountCoinsRequest",
  #{<<"description">> =>
        <<"AccountCoinsRequest is utilized to make a request on the /account/coins\nendpoint.\n">>,
    <<"properties">> =>
        #{<<"account_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/AccountIdentifier">>},
          <<"currencies">> =>
              #{<<"description">> =>
                    <<"In some cases, the caller may not want to retrieve coins for all\ncurrencies for an AccountIdentifier. If the currencies field\nis populated, only coins for the specified currencies\nwill be returned. If not populated, all unspent coins\nwill be returned.\n">>,
                <<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/Currency">>},
                <<"type">> => <<"array">>},
          <<"include_mempool">> =>
              #{<<"description">> =>
                    <<"Include state from the mempool when looking up an account's\nunspent coins. Note, using this functionality\nbreaks any guarantee of idempotency.\n">>,
                <<"type">> => <<"boolean">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>}},
    <<"required">> =>
        [<<"network_identifier">>,<<"account_identifier">>,
         <<"include_mempool">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/AccountBalanceResponse",
  #{<<"description">> =>
        <<"An AccountBalanceResponse is returned on the /account/balance endpoint.\nIf an account has a balance for each AccountIdentifier describing it\n(ex: an ERC-20 token balance on a few smart contracts), an account\nbalance request must be made with each AccountIdentifier.\n\nThe `coins` field was removed and replaced by by `/account/coins` in `v1.4.7`.\n">>,
    <<"properties">> =>
        #{<<"balances">> =>
              #{<<"description">> =>
                    <<"A single account may have a balance in multiple currencies.\n">>,
                <<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/Amount">>},
                <<"type">> => <<"array">>},
          <<"block_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/BlockIdentifier">>},
          <<"metadata">> =>
              #{<<"description">> =>
                    <<"Account-based blockchains that utilize a nonce or sequence number\nshould include that number in the metadata. This number could be\nunique to the identifier or global across the account address.\n">>,
                <<"example">> => #{<<"sequence_number">> => 23},
                <<"type">> => <<"object">>}},
    <<"required">> => [<<"block_identifier">>,<<"balances">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/AccountBalanceRequest",
  #{<<"description">> =>
        <<"An AccountBalanceRequest is utilized to make a balance request\non the /account/balance endpoint. If the block_identifier is populated,\na historical balance query should be performed.\n">>,
    <<"properties">> =>
        #{<<"account_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/AccountIdentifier">>},
          <<"block_identifier">> =>
              #{<<"$ref">> =>
                    <<"/components/schemas/PartialBlockIdentifier">>},
          <<"currencies">> =>
              #{<<"description">> =>
                    <<"In some cases, the caller may not want to retrieve all available\nbalances for an AccountIdentifier. If the currencies field\nis populated, only balances for the specified currencies\nwill be returned. If not populated, all available balances\nwill be returned.\n">>,
                <<"items">> =>
                    #{<<"$ref">> => <<"/components/schemas/Currency">>},
                <<"type">> => <<"array">>},
          <<"network_identifier">> =>
              #{<<"$ref">> => <<"/components/schemas/NetworkIdentifier">>}},
    <<"required">> => [<<"network_identifier">>,<<"account_identifier">>],
    <<"type">> => <<"object">>}},
 {"/components/schemas/Direction",
  #{<<"$ref">> => <<"models/Direction.yaml">>}},
 {"/components/schemas/RelatedTransaction",
  #{<<"$ref">> => <<"models/RelatedTransaction.yaml">>}},
 {"/components/schemas/BlockTransaction",
  #{<<"$ref">> => <<"models/BlockTransaction.yaml">>}},
 {"/components/schemas/Operator",#{<<"$ref">> => <<"models/Operator.yaml">>}},
 {"/components/schemas/BlockEventType",
  #{<<"$ref">> => <<"models/BlockEventType.yaml">>}},
 {"/components/schemas/BlockEvent",
  #{<<"$ref">> => <<"models/BlockEvent.yaml">>}},
 {"/components/schemas/ExemptionType",
  #{<<"$ref">> => <<"models/ExemptionType.yaml">>}},
 {"/components/schemas/BalanceExemption",
  #{<<"$ref">> => <<"models/BalanceExemption.yaml">>}},
 {"/components/schemas/Coin",#{<<"$ref">> => <<"models/Coin.yaml">>}},
 {"/components/schemas/CoinChange",
  #{<<"$ref">> => <<"models/CoinChange.yaml">>}},
 {"/components/schemas/CoinIdentifier",
  #{<<"$ref">> => <<"models/CoinIdentifier.yaml">>}},
 {"/components/schemas/CoinAction",
  #{<<"$ref">> => <<"models/CoinAction.yaml">>}},
 {"/components/schemas/SignatureType",
  #{<<"$ref">> => <<"models/SignatureType.yaml">>}},
 {"/components/schemas/Signature",
  #{<<"$ref">> => <<"models/Signature.yaml">>}},
 {"/components/schemas/SigningPayload",
  #{<<"$ref">> => <<"models/SigningPayload.yaml">>}},
 {"/components/schemas/CurveType",
  #{<<"$ref">> => <<"models/CurveType.yaml">>}},
 {"/components/schemas/PublicKey",
  #{<<"$ref">> => <<"models/PublicKey.yaml">>}},
 {"/components/schemas/Timestamp",
  #{<<"$ref">> => <<"models/Timestamp.yaml">>}},
 {"/components/schemas/OperationStatus",
  #{<<"$ref">> => <<"models/OperationStatus.yaml">>}},
 {"/components/schemas/Allow",#{<<"$ref">> => <<"models/Allow.yaml">>}},
 {"/components/schemas/Version",#{<<"$ref">> => <<"models/Version.yaml">>}},
 {"/components/schemas/Peer",#{<<"$ref">> => <<"models/Peer.yaml">>}},
 {"/components/schemas/SyncStatus",
  #{<<"$ref">> => <<"models/SyncStatus.yaml">>}},
 {"/components/schemas/Currency",#{<<"$ref">> => <<"models/Currency.yaml">>}},
 {"/components/schemas/Amount",#{<<"$ref">> => <<"models/Amount.yaml">>}},
 {"/components/schemas/Operation",
  #{<<"$ref">> => <<"models/Operation.yaml">>}},
 {"/components/schemas/Transaction",
  #{<<"$ref">> => <<"models/Transaction.yaml">>}},
 {"/components/schemas/Block",#{<<"$ref">> => <<"models/Block.yaml">>}},
 {"/components/schemas/SubAccountIdentifier",
  #{<<"$ref">> => <<"models/SubAccountIdentifier.yaml">>}},
 {"/components/schemas/AccountIdentifier",
  #{<<"$ref">> => <<"models/AccountIdentifier.yaml">>}},
 {"/components/schemas/OperationIdentifier",
  #{<<"$ref">> => <<"models/OperationIdentifier.yaml">>}},
 {"/components/schemas/TransactionIdentifier",
  #{<<"$ref">> => <<"models/TransactionIdentifier.yaml">>}},
 {"/components/schemas/PartialBlockIdentifier",
  #{<<"$ref">> => <<"models/PartialBlockIdentifier.yaml">>}},
 {"/components/schemas/BlockIdentifier",
  #{<<"$ref">> => <<"models/BlockIdentifier.yaml">>}},
 {"/components/schemas/SubNetworkIdentifier",
  #{<<"$ref">> => <<"models/SubNetworkIdentifier.yaml">>}},
 {"/components/schemas/NetworkIdentifier",
  #{<<"$ref">> => <<"models/NetworkIdentifier.yaml">>}}].

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
        #{path := Endpoint, parameters := Parameters,
          method := Method1} =
            operation(OperationId),
        case Method =:= Method1 of
            true ->
                InPath = [Param
                          || Param <- Parameters,
                             lists:member({"in", "path"}, Param)],
                lists:foldl(fun (Param, Path) ->
                                    Name = proplists:get_value("name", Param),
                                    case {proplists:get_value("required",
                                                              Param,
                                                              false),
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
                                                                            "{"
                                                                                ++
                                                                                Name
                                                                                    ++
                                                                                    "}",
                                                                            to_str(Value)))
                                    end
                            end,
                            Endpoint,
                            InPath);
            false -> {error, unexpected_method}
        end
    end.

query(Method, OperationId, Args) when is_map(Args) ->
    query(Method, OperationId, maps:to_list(Args));
query(Method, OperationId, Args) ->
    begin
        #{parameters := Parameters, method := Method1} =
            operation(OperationId),
        case Method =:= Method1 of
            true ->
                InQuery = [Param
                           || Param <- Parameters,
                              lists:member({"in", "query"}, Param)],
                Query = lists:foldr(fun (Param, Query) ->
                                            Name = proplists:get_value("name",
                                                                       Param),
                                            case
                                                {proplists:get_value("required",
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
                                                      encode(to_str(Value))}
                                                     | Query]
                                            end
                                    end,
                                    [],
                                    InQuery),
                case [[K, "=", V] || {K, V} <- Query] of
                    [] -> <<>>;
                    Qs -> iolist_to_binary(["?" | lists:join("&", Qs)])
                end;
            false -> {error, unexpected_method}
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
        #{parameters := Parameters, method := Method1} =
            operation(OperationId),
        case Method =:= Method1 of
            true ->
                ToCheck = [Param
                           || Param <- Parameters,
                              not lists:member({"in", "path"}, Param)],
                Errors = lists:foldl(fun (Param, Errs) ->
                                             Name = proplists:get_value("name",
                                                                        Param),
                                             case
                                                 {proplists:get_value("required",
                                                                      Param,
                                                                      false),
                                                  get_by_similar_key(Name,
                                                                     Args)}
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
                                                         {error, E} ->
                                                             [E | Errs];
                                                         _ -> Errs
                                                     end
                                             end
                                     end,
                                     [],
                                     ToCheck),
                case Errors of
                    [] -> ok;
                    _ -> {errors, {OperationId, Args, Errors}}
                end;
            false -> {error, unexpected_method}
        end
    end.

validate_response(OperationId, Method, StatusCode,
                  Response) ->
    begin
        #{responses := Resps, method := Method1} =
            operation(OperationId),
        case Method =:= Method1 of
            true ->
                prepare_validation(),
                case maps:get(StatusCode, Resps, error) of
                    error -> {error, {StatusCode, unspecified}};
                    undefined -> {ok, StatusCode, Response};
                    Schema ->
                        case validate(Schema, Response) of
                            {ok, _} -> {ok, StatusCode, Response};
                            {error, E} -> {error, {validation, E}}
                        end
                end;
            false -> {error, unexpected_method}
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

encode(URI) when is_list(URI) ->
    begin
        Reserved = reserved(),
        lists:append([uri_encode(Char, Reserved)
                      || Char <- URI])
    end;
encode(URI) when is_binary(URI) ->
    begin
        Reserved = reserved(),
        << <<(uri_encode_binary(Char, Reserved))/binary>>
            || <<Char>> <= URI >>
    end.

uri_encode(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
        true -> [$% | integer_to_list(Char, 16)];
        false -> [Char]
    end.

uri_encode_binary(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
        true -> <<$%, (integer_to_binary(Char, 16))/binary>>;
        false -> <<Char>>
    end.

reserved() ->
    sets:from_list([$;,
                    $:,
                    $@,
                    $&,
                    $=,
                    $+,
                    $,,
                    $/,
                    $?,
                    $#,
                    $[,
                    $],
                    $<,
                    $>,
                    $",
                    ${,
                    $},
                    $|,
                    $\\,
                    $',
                    $^,
                    $%,
                    $\s]).