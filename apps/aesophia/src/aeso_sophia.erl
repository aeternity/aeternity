-module(aeso_sophia).

-export_type([data/0,
              type/0,
              heap/0]).

-type type() :: word | signed_word | string | typerep | function
              | {list,   type()}
              | {option, type()}
              | {tuple, [type()]}.


-type data() :: none
              | {some,   data()}
              | {option, data()}
              | word
              | string
              | {list,   data()}
              | {tuple, [data()]}
              | integer()
              | binary()
              | [data()]
              | {}
              | {data()}
              | {data(), data()}.

-type heap() :: binary().

