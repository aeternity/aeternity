
-define(WORDSIZE, 256).
-define(MASK256, ((1 bsl 256) -1)).
-define(ALIGN256, 16#1f).
-define(NEG2TO255, (- (1 bsl 256) band ?MASK256)).
-define(MASK160, ((1 bsl 160) -1)).
