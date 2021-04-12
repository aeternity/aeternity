%% Arguments like <<_:256>> are encoded as addresses.
%% For FATE we need to distinguish between the type of object.
%% For test cases that is run both for FATE and AEVM, wrap args in
%% the constructors below.
-define(cid(__X__), {'@ct', __X__}).
-define(hsh(__X__), {'#', __X__}).
-define(sig(__X__), {'$sg', __X__}).
-define(oid(__X__), {'@ok', __X__}).
-define(qid(__X__), {'@oq', __X__}).
