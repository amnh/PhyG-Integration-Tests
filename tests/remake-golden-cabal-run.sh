#Shell script to update golden test files

# Step 0: Define constants
STYLE_BLINK=$(tput blink)
STYLE_BOLD=$(tput bold)
STYLE_BLUE=$(tput setaf 4)
STYLE_NORMAL=$(tput sgr0)

# Step 1: Define how to move a file
blessing_of_Midas() {
    local FILE_PATH="${1}"
    cp "${FILE_PATH}" "${FILE_PATH}.golden" > /dev/null 2>&1
}

# Step 2: Define how to generate the output files
generate_annointed() {
    local FILE_PATH="${1}"

    printf "\t>>> Running %s...\n" "phyg"
    local TIME_START=$(date +%s)
    # Choose which line to uncomment based on whether or not you want to see PhyG output
    (cd "./${FILE_PATH}"; /Users/louise/.cabal/bin/phyg *.pg)
#    (cd "./${FILE_PATH}"; /Users/louise/.cabal/bin/phyg *.pg > /dev/null 2>&1)
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


# Step 3: Get all the test directories
declare -a SUB_DIRS=( $(find . -depth 1 -type d | xargs basename | sort -t 't' -k 2n) )

# Step 4: Loop through each sub-directory
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
