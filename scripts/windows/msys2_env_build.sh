# Msys2 config
export MSYS=winsymlinks:nativestrict

if [ "${MSYSTEM}" = "" -o "${MSYSTEM}" = "MSYS" ]; then
    export MSYSTEM="MINGW64"
fi

make_winpath()
{
    P=$1
    if [ "$IN_CYGWIN" = "true" ]; then
        cygpath -d "$P"
    else
        (cd "$P" && /bin/cmd //C "for %i in (".") do @echo %~fsi")
    fi
}

make_upath()
{
    P=$1
    if [ "$IN_CYGWIN" = "true" ]; then
        cygpath "$P"
    else
        echo "$P" | /bin/sed 's,^\([a-zA-Z]\):\\,/\L\1/,;s,\\,/,g'
    fi
}

# Without this the path conversion won't work
COMSPEC='C:\Windows\System32\cmd.exe'

obe_otp_gcc_vsn_map="
    .*=>default
"
obe_otp_64_gcc_vsn_map="
    .*=>default
"

IN_CYGWIN=false
CPPFLAGS=${CPPFLAGS:-"-D _WIN32"}

MSYS2_ROOT=$(make_upath "${WIN_MSYS2_ROOT:-"C:\\tools\\msys64"}")

WIN_OTP_PATH="${WIN_OTP_PATH:-${PROGRAMFILES}\\erl${ERTS_VERSION}}"
OTP_PATH="$(make_upath "${WIN_OTP_PATH}")"

WIN_MSVC_ROOT=${VCToolsInstallDir}
WIN_MSVC=${WIN_MSVC_ROOT}bin\\Hostx64\\x64

MSVC_ROOT="$(make_upath "${WIN_MSVC_ROOT}")"
MSVC="$(make_upath "${WIN_MSVC}")"

PATH="/usr/local/bin:/usr/bin:/bin:/c/Windows/system32:/c/Windows:/c/Windows/System32/Wbem:${PATH}"
PATH="${HOME}/.local/bin:${MSVC}:${OTP_PATH}/bin:${OTP_PATH}/erts-${ERTS_VERSION}/bin:${MSYS2_ROOT}/mingw64/bin:${PATH}"
PATH="${MSYS_INCLUDE_PATH:-""}:${PATH}"

INCLUDE="${INCLUDE};${WIN_MSYS2_ROOT}\\mingw64\\include;${WIN_MSYS2_ROOT}\\usr\\include"
LIB="${LIB};${WIN_MSYS2_ROOT}\\mingw64\\lib;${WIN_MSYS2_ROOT}\\mingw64\\bin;${WIN_OTP_PATH}\\usr\\lib;"

export INCLUDE LIB PATH OTP_PATH MSYS_PATH COMSPEC CPPFLAGS
