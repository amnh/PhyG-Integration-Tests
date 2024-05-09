#!/bin/sh
# Shell script to update golden test files

# Step 0: Define constants
STYLE_BLINK=$(tput blink)
STYLE_BOLD=$(tput bold)
STYLE_BLUE=$(tput setaf 4)
STYLE_NORMAL=$(tput sgr0)

BINARY_NAME='phyg'
#TARGET_NAME="PhyG:exe:${BINARY_NAME}"
TARGET_NAME="${BINARY_NAME}"

PHYG_FINAL_DIR="${HOME}/.cabal/bin"
PHYG_FINAL_BIN="${PHYG_FINAL_DIR}/${BINARY_NAME}"

GIT_BRANCH='merging-all'
GIT_SOURCE="https://github.com/amnh/${BINARY_NAME}"


PROJECT_FILEPATH=$(readlink -f 'single-threaded.project')
PROJECT_FILE_ARG="--project-file=$PROJECT_FILEPATH"

# Step 1: Get PhyG using git
STARTING_DIR=$(pwd)
rm -Rf "${BINARY_NAME}"
git clone "${GIT_SOURCE}"


# Step 2: Locate and navigate to local PhyG repository
cd "${BINARY_NAME}"
PHYG_LOCAL_DIR=$(find . -maxdepth 1 -type d -iname "PhyG*")
printf '>>> Located: %s\n' "${PHYG_DIR}"
cd "${PHYG_LOCAL_DIR}"
WORKING_DIR=$(pwd)
printf '>>> Entered:\t%s\n' "${WORKING_DIR}"
printf '>>> Checkout:\t%s\n' "${GIT_BRANCH}"
git checkout "${GIT_BRANCH}"
GIT_HASH=$(git rev-parse HEAD)
printf '>>> Git Hash:\t%s\n' "${GIT_HASH}"


# Step 3:  Check for cached versions of the PhyG binary/libs
printf '>>> Is Cached?\t%s\n' "${BINARY_NAME}"
PHYG_CACHE_DIR=$(find "${HOME}/.cabal/store/ghc-9.8.2" -maxdepth 1 -type d -name "PhyG-*")
if [ -z "${VAR}" ]; then
    rm -Rf "${PHYG_CACHE_DIR}" 
    printf '>>> Cleared: %s\n' "${PHYG_CACHE_DIR}"
else
    printf '>>> No Cache!\n'
fi


# Step 4: Build local copy of PhyG 
printf "\n${STYLE_BLUE}${STYLE_BOLD}Beginning fresh rebuild of${STYLE_NORMAL} '%s'\n\n" "${BINARY_NAME}"
cabal build "${TARGET_NAME}"
printf "\n${STYLE_BLUE}${STYLE_BOLD}Successfully built${STYLE_NORMAL} '%s'\n\n" "${BINARY_NAME}"


# Step 5: Place PhyG on the $PATH
PHYG_LOCAL_BIN=$(cabal list-bin phyg | tail -n 1)
PHYG_FINAL_DIR=$(echo "${HOME}/.cabal/bin")
echo "${PHYG_FINAL_DIR}"
mkdir -p "${PHYG_FINAL_DIR}"
rm -f "${PHYG_FINAL_BIN}"
mv "${PHYG_LOCAL_BIN}" "${PHYG_FINAL_BIN}"
printf "\n${STYLE_BLUE}${STYLE_BOLD}Finalizing binary at${STYLE_NORMAL} '%s'\n\n" "${PHYG_FINAL_DIR}/${BINARY_NAME}"


# Step 6: Clean up
cd "${STARTING_DIR}"
rm -Rf "${BINARY_NAME}"



# # Step 2: Define how to generate the output files
# generate_annointed() {
#     local FILE_PATH="${1}"
# 
#     printf "\t>>> Running %s...\n" "phyg"
#     local TIME_START=$(date +%s)
#     # Choose which line to uncomment based on whether or not you want to see PhyG output
# #    (cd "./${FILE_PATH}"; "${BINARY_PATH}" *.pg)
#     (cd "./${FILE_PATH}"; ${BINARY_PATH} *.pg > screen.log 2>&1)
#     local TIME_FINAL=$(date +%s)
# 
#     # Display time in a standardized format.
#     local B=${TIME_START}
#     local E=${TIME_FINAL}
#     local T=$(( E-B ))
#     local D=$(( T/60/60/24 ))
#     local H=$(( T/60/60%24 ))
#     local M=$(( T/60%60    ))
#     local S=$(( T%60       ))
#     printf '\t>>> Ouput files generation completed in %02dd %02dh %02dm %02ds\n' $D $H $M $S
# }
# 
# 
# # Step 3: Before beginning any work, we must check that we are using the newest version of PhyG
# BINARY_NAME='phyg'
# TARGET_NAME="PhyG:exe:${BINARY_NAME}"
# 
# # Refresh
# printf "\n${STYLE_BLUE}${STYLE_BOLD}Refreshing the binary component${STYLE_NORMAL} '%s'\n\n" "${BINARY_NAME}"
# cabal clean
# cabal build "${TARGET_NAME}" --project-file=single-threaded.project
# 
# # Locate
# BINARY_DIR=$(cabal list-bin "${TARGET_NAME}" --project-file=single-threaded.project | tail -n 1 | xargs dirname)
# BINARY_PATH=$(find "${BINARY_DIR}" -maxdepth 1 -type f -iname "*${BINARY_NAME}" -print -quit)
# printf "Setting '%s'\t= '%s'\n" "BINARY_DIR" "${BINARY_DIR}"
# printf "Setting '%s'\t= '%s'\n" "BINARY_PATH" "${BINARY_PATH}"
# 
# 
# # Step 4: Get all the test directories
# declare -a SUB_DIRS=( $(find . -maxdepth 1 -type d | xargs basename | sort -t 't' -k 2n) )
# 
# 
# # Step 5: Loop through each sub-directory
# for SUBDIRECTORY in "${SUB_DIRS[@]}"; do
# 	printf "${STYLE_BLUE}Test case:\t${STYLE_BLINK}${STYLE_BOLD}%s${STYLE_NORMAL}\n" "${SUBDIRECTORY}"
#         generate_annointed "${SUBDIRECTORY}"
#         printf "\t>>> Copying Outputs..."
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}.tre"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_gv.tre"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_collapse.tre"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_threshold-10.tre"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_threshold-1.tre"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_threshold-05.tre"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_atrandom.tre"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_unique.tre"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_best.tre"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}.csv"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_ia.txt"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_diag.txt" 
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_ascii.txt"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_cr.csv"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}_pd.csv"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}.dot"
#         blessing_of_Midas "${SUBDIRECTORY}/${SUBDIRECTORY}.ss"
#         printf " done!\n\n"
# done
