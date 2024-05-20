#!/bin/sh
# Shell script to update golden test files

# Step 0: Define constants
#########
STYLE_BLINK=$(tput blink)
STYLE_BOLD=$(tput bold)
STYLE_BLUE=$(tput setaf 4)
STYLE_NORMAL=$(tput sgr0)

DIR_STARTING=$(pwd)
DIR_PROJECT_NAME='PhyG-Integration-Tests'
DIR_PROJECT_ROOT=$(pwd | sed "s/\(${DIR_PROJECT_NAME}\).*/\1/g")
DIR_PROJECT_EXES="${DIR_PROJECT_ROOT}/bin"
DIR_PROJECT_TEST="${DIR_PROJECT_ROOT}/tests"
EXE_REFRESH='refresh-phyg.sh'
BINARY_NAME='phyg'


# Step 1: Define how to move a file
#########
blessing_of_Midas() {
    local FILE_PATH="${1}"
    cp "${FILE_PATH}" "${FILE_PATH}.golden" > /dev/null 2>&1
}


# Step 2: Define how to generate the output files
#########
generate_annointed() {
    local FILE_PATH="${1}"

    printf "\t>>> Running %s...\n" "phyg"
    local TIME_START=$(date +%s)
    # Choose which line to uncomment based on whether or not you want to see PhyG output
#    (cd "./${FILE_PATH}"; "${BINARY_PATH}" *.pg)
    (cd "./${FILE_PATH}"; ${BINARY_PATH} *.pg > screen.log 2>&1)
    local TIME_FINAL=$(date +%s)

    # Display time in a standardized format.
    local B=${TIME_START}
    local E=${TIME_FINAL}
    local T=$(( E-B ))
    local D=$(( T/60/60/24 ))
    local H=$(( T/60/60%24 ))
    local M=$(( T/60%60    ))
    local S=$(( T%60       ))
    printf '\t>>> Ouput files generation completed in %02dd %02dh %02dm %02ds\n' $D $H $M $S
}


# Step 3: Before beginning any work, we must check that we are using the newest version of PhyG
#########
printf "\n${STYLE_BLUE}${STYLE_BOLD}Refreshing the binary component${STYLE_NORMAL} '%s'\n\n" "${BINARY_NAME}"
cabal clean
"${DIR_PROJECT_EXES}/${EXE_REFRESH}"
BINARY_PATH=$(which "${BINARY_NAME}")
printf "Setting '%s'\t= '%s'\n" "BINARY_PATH" "${BINARY_PATH}"


# Step 4: Get all the test directories
#########
cd "${DIR_PROJECT_TEST}"
declare -a SUB_DIRS=( $(find . -maxdepth 1 -type d -regex '.*/t[0-9][0-9]*' | xargs basename | sort -t 't' -k 2n) )


# Step 5: Loop through each sub-directory
#########
for SUBDIRECTORY in "${SUB_DIRS[@]}"; do
	printf "${STYLE_BLUE}Test case:\t${STYLE_BLINK}${STYLE_BOLD}%s${STYLE_NORMAL}\n" "${SUBDIRECTORY}"
        generate_annointed "${SUBDIRECTORY}"
        printf "\t>>> Copying Outputs..."
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}.tre"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_gv.tre"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_collapse.tre"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_threshold-10.tre"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_threshold-1.tre"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_threshold-05.tre"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_atrandom.tre"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_unique.tre"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_best.tre"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}.csv"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_ia.txt"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_diag.txt" 
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_ascii.txt"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_cr.csv"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_pd.csv"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}.dot"
        blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}.ss"
        printf " done!\n\n"
done


# Step 6: Clean up
#########
cd "${DIR_STARTING}"
