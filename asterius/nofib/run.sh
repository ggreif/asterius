#!/bin/bash

MODE="NORM"              # Default mode is NORM (can also be FAST or SLOW)
COMPILER="$(which ghc)"  # Default is ~/.stack/programs/x86_64-linux/ghc-custom-asterius-8.8.3/bin/ghc

if [ $# -eq 2 ]; then
    MODE=$1
    COMPILER=$2
fi

echo "-------------------------------------------------------------------------------"
echo "                                PARAMETERS                                     "
echo "-------------------------------------------------------------------------------"
echo "Compiler : ${COMPILER}"
echo "Mode     : ${MODE}"

echo "-------------------------------------------------------------------------------"
echo "                                 BUILDING                                      "
echo "-------------------------------------------------------------------------------"

for category in *; do
  if [ -d "${category}" -a "${category}" != "common" ]; then
    # For each category of tests:
    cd ${category} && echo "Entering $PWD ..." # Enter the category folder
    for testfolder in *; do
      if [ -d "${testfolder}" ]; then
        # For each test within this category
        cd ${testfolder} && echo "Entering $PWD ..." # Enter the test folder
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # BUILDING ALL TEST FILES
        for testfile in *.hs; do
          [ -f "$testfile" ] || break # If the file does not exist, move on..
          noext=${testfile%.hs} # Filename without extension

          # Retrieve the compile options for the current mode
          copts_file=${noext}.${MODE}_COMPILE_OPTS
          if [ -f "${copts_file}" ]; then
            copts=$(<${copts_file})
          else
            copts="" # no compiler options, use the empty string
          fi

          # Do the actual building
          # echo "${COMPILER} ${copts} -c $testfile -o ${noext}.o"
          echo "EXECUTING: ${COMPILER} ${copts} $testfile"
          ${COMPILER} ${copts} $testfile # -c $testfile -o ${noext}.o
          # TODO: IT DOES NOT COMPILE THEM IN ORDER :/
        done
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # RUNNING ALL TEST FILES

        # Retrieve the runtime options for the current mode
        ropts_file=${MODE}_OPTS
        if [ -f "${ropts_file}" ]; then
          ropts=$(<${ropts_file})
        else
          ropts="" # no runtime options options, use the empty string
        fi

        # Create input and output file names
        input_file_name=${testfolder}.$(echo ${MODE} | tr '[:upper:]' '[:lower:]')stdin # e.g. primetest.faststdin
        output_file_name=${testfolder}.stdout # e.g. primetest.stdout

        # Do the actual running
        if [ -f "${input_file_name}" ]; then
          echo "EXECUTING: ${PWD}/Main ${ropts} <${input_file_name} >${output_file_name}"
          $(${PWD}/Main ${ropts} <${input_file_name} >${output_file_name})
        else
          echo "EXECUTING: ${PWD}/Main ${ropts} >${output_file_name}"
          $(${PWD}/Main ${ropts} >${output_file_name})
        fi
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        echo "Leaving $PWD ..." && cd ..             # Leave the test folder
      fi
    done
    echo "Leaving $PWD ..." && cd ..           # Leave the category folder
  fi
done
