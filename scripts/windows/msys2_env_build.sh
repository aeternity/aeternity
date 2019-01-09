# Msys2 config
export MSYS=winsymlinks:nativestrict

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

C_DRV="/c"
WIN_C_DRV="C:\\"
IN_CYGWIN=false

PRG_FLS64="${C_DRV}/Program Files"
PRG_FLS32="${C_DRV}/Program Files (x86)"
WIN_PRG_FLS64="${WIN_C_DRV}\\Program Files"
WIN_PRG_FLS32="${WIN_C_DRV}\\Program Files (x86)"
ERL_TOP="${PRG_FLS64}/erl${ERTS_VERSION}"
WIN_ERL_TOP="${WIN_PRG_FLS64}\\erl${ERTS_VERSION}"

VISUAL_STUDIO_ROOT=${PRG_FLS32}/Microsoft\ Visual\ Studio/2017/Community
WIN_VISUAL_STUDIO_ROOT="${WIN_PRG_FLS32}\\Microsoft Visual Studio\\2017\\Community"

MSVC_ROOT=${VISUAL_STUDIO_ROOT}/VC/Tools/MSVC/${MSVC_VERSION}
MSVC=${MSVC_ROOT}/bin/Hostx64/x64
WIN_MSVC_ROOT=${WIN_VISUAL_STUDIO_ROOT}\\VC\\Tools\\MSVC\\${MSVC_VERSION}
WIN_MSVC=${WIN_MSVC_ROOT}/bin\\Hostx64\\x64

PATH="/usr/local/bin:/usr/bin:/bin:/c/Windows/system32:/c/Windows:/c/Windows/System32/Wbem:${PATH}"
PATH="${MSVC}:${ERL_TOP}/bin:${PATH}:${ERL_TOP}/erts-${ERTS_VERSION}/bin:${MSYS_ROOT}/mingw64/bin"

WIN_MSYS_ROOT="${WIN_C_DRV}\\msys64"

INCLUDE="${INCLUDE};${WIN_MSYS_ROOT}\\mingw64\\include;${WIN_MSYS_ROOT}\\usr\\include"
LIB="${LIB};${WIN_MSYS_ROOT}\\mingw64\\lib;${WIN_MSYS_ROOT}\\mingw64\\bin;${WIN_ERL_TOP}\\usr\\lib;"

export INCLUDE LIB PATH ERL_TOP WIN_ERL_TOP COMSPEC
