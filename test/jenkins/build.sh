#! /bin/bash

# This is a script for compilation of Extpar by Jenkins slaves

case "$(hostname)" in
    # CSCS machines
    daint*)
        git submodule init
        git submodule update
        ./configure.daint.gcc
        source modules.env
        make clean
        echo compile extpar...
        make &> compile.log
        echo          ...done
        echo See compile.log for more information!
        ;;

    kesch*)
        echo Extpar is no longer supported on Kesch!
        ;;

    tsa*)
        git submodule init
        git submodule update
        ./configure.tsa.gcc
        source modules.env
        make clean
        echo compile extpar...
        make &> compile.log
        echo          ...done
        echo See compile.log for more information!
        ;;

    # DKRZ machines    
    mlogin*)
        if [[ -r /sw/rhel6-x64/etc/profile.mistral ]]
        then
           source /sw/rhel6-x64/etc/profile.mistral
        fi
        git submodule init
        git submodule update
        ./configure.mistral.$compiler
        source modules.env
        make clean
        echo compile extpar...
        make &> compile.log
        echo          ...done
        echo See compile.log for more information!

        ;;
esac 


