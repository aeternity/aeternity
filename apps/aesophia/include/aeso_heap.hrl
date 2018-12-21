
-record(pmap, {key_t  :: aeso_sophia:type(),
               val_t  :: aeso_sophia:type(),
               parent :: none | non_neg_integer(),
               size   = 0 :: non_neg_integer(),
               data   :: #{aeso_heap:binary_value() => aeso_heap:binary_value() | tombstone}
                       | stored}).

-record(maps, { maps    = #{} :: #{ non_neg_integer() => #pmap{} }
              , next_id = 0   :: non_neg_integer() }).

-record(heap, { maps   :: #maps{},
                offset :: aeso_heap:offset(),
                heap   :: binary() | #{non_neg_integer() => non_neg_integer()} }).

