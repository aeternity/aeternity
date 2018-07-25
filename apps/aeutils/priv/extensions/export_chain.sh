#!/bin/bash

error() {
    msg="$*"
    echo "ERROR: $msg"
    echo "USAGE: $0 export FILE_PATH"
    exit 1
}

if [ $# -lt 1 ]; then
    error "missing argument"
fi

if [ $# -gt 1 ]; then
    error "unexpected arguments"
fi

FILE_PATH="$1"

if [ -f "$FILE_PATH" ]; then
    error "file already exists"
fi

FILE_DIR=$(dirname "$FILE_PATH")

if [ $(mkdir -p "$FILE_DIR") ]; then
    error "failed to create parent directory"
fi

CODE="
    case aeu_export:to_disklog(\"$FILE_PATH\") of
        {ok, BlockCount} ->
            \"ok:\" ++ integer_to_list(BlockCount);
        {error, Reason} ->
            \"error:\" ++ lists:flatten(io_lib:format(\"~p\", [Reason]))
    end.
"

! RESULT=$(relx_nodetool eval $CODE)

if [ $? -eq 1 ]; then
    RESULT="${RESULT%\"}"
    RESULT="${RESULT#\"}"
    RESULT_TYPE=$(echo $RESULT | cut -d':' -f1)
    RESULT_PAYLOAD=$(echo $RESULT | cut -d':' -f2-)
    case $RESULT_TYPE in
        error)
            echo "ERROR: $RESULT_PAYLOAD"
            exit 1
            ;;
        ok)
            echo "$RESULT_PAYLOAD blocks exported to $FILE_PATH"
            exit 0
            ;;
    esac
fi

echo "ERROR: $RESULT"
exit 1
