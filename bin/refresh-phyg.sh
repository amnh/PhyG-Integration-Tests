#!/bin/sh
# Shell script to build the latest version of PhyG to be used by the integration tests-suite.


# Step 0: Define constants
#########
STYLE_BLINK=$(tput blink)
STYLE_BOLD=$(tput bold)
STYLE_BLUE=$(tput setaf 4)
STYLE_NORMAL=$(tput sgr0)

GHC_VERSION='9.12.2'

BINARY_NAME='phyg'
BINARY_OPTS=( 'Single-Threaded' 'Super-Optimization' )
#TARGET_NAME="PhyG:exe:${BINARY_NAME}"
TARGET_NAME="${BINARY_NAME}"
TARGET_OPTS=( "${BINARY_OPTS[@]/#/-f}" )

PHYG_FINAL_DIR="${HOME}/.cabal/bin"
PHYG_FINAL_BIN="${PHYG_FINAL_DIR}/${BINARY_NAME}"

GIT_BRANCH='main' # previously 'merging-all'
GIT_SOURCE="https://github.com/amnh/${BINARY_NAME}"

PROJECT_FILEPATH=$(readlink -f 'single-threaded.project')
PROJECT_FILE_ARG="--project-file=${PROJECT_FILEPATH}"

# The relevant project directories
DIR_PROJECT_NAME='PhyG-Integration-Tests'
DIR_PROJECT_ROOT=$(pwd | sed "s/\(${DIR_PROJECT_NAME}\).*/\1/g")
DIR_PROJECT_TEMP="${DIR_PROJECT_ROOT}/temp"

# How to find the local PhyG directory
FIND_LOCAL_PHYG=( find . -maxdepth 1 -type d -iname "PhyG*" )


# Step 1: Get PhyG using git
#########
DIR_STARTING=$(pwd)                       # Save starting directory

mkdir -p "${DIR_PROJECT_TEMP}"            # Ensure the temp directory exists
cd "${DIR_PROJECT_TEMP}"                  # Enter temp directory
DIR_PHYG_EXIST=$("${FIND_LOCAL_PHYG[@]}") # Find any existing local PhyG directory
if [ -n "${DIR_PHYG_EXIST}" ]; then       # Remove any existing local PhyG directory/file
    rm -Rf "${DIR_PHYG_EXIST}"
fi
git clone "${GIT_SOURCE}"                 # Clone Phyg into local temp directory


# Step 2: Locate and navigate to local PhyG repository
#########
cd "${BINARY_NAME}"
DIR_PHYG_LOCAL=$("${FIND_LOCAL_PHYG[@]}")
cd "${DIR_PHYG_LOCAL}"
DIR_WORKING=$(pwd)
git checkout "${GIT_BRANCH}"
GIT_HASH=$(git rev-parse HEAD)
printf '>>> Entered:\t%s\n' "${DIR_WORKING}"
printf '>>> Version:\t%s\n' "${GIT_HASH}"


# Step 3:  Check for cached versions of the PhyG binary/libs
#########
PHYG_CACHE_DIR=$(find "${HOME}/.cabal/store/ghc-${GHC_VERSION}" -maxdepth 1 -type d -name "PhyG-*")
if [ -n "${PHYG_CACHE_DIR}" ]; then
    printf '>>> Cleared:\t( Cached PhyG Instances )\n' "${PHYG_CACHE_DIR}"
    while read -r -u 9 FILEPATH || [ -n "${FILEPATH}" ];
    do
        rm -Rf "${FILEPATH}"
        printf '\t%s\n' "${FILEPATH}"    
    done 9<<<"${PHYG_CACHE_DIR}";    
else
    printf '>>> No PhyG:\t( Nothing Cached )\n'
fi


# Step 4: Build local copy of PhyG
#########
CABAL_BUILD_COMMAND=( cabal build "${TARGET_NAME}" "${TARGET_OPTS[@]}" )
CABAL_BUILD_TEXTUAL=$(echo "${CABAL_BUILD_COMMAND[*]}")
printf "\n${STYLE_BLUE}${STYLE_BOLD}Beginning fresh rebuild of${STYLE_NORMAL} '%s'\n\n" "${BINARY_NAME}"
printf '^^^\n>>>\t%s\nvvv\n\n' "${CABAL_BUILD_TEXTUAL}"
"${CABAL_BUILD_COMMAND[@]}"
printf "\n${STYLE_BLUE}${STYLE_BOLD}Successfully built${STYLE_NORMAL} '%s'\n\n" "${BINARY_NAME}"


# Step 5: Place PhyG on the $PATH
#########
PHYG_LOCAL_BIN=$(cabal list-bin phyg | tail -n 1)
PHYG_FINAL_DIR=$(echo "${HOME}/.cabal/bin")
echo "${PHYG_FINAL_DIR}"
mkdir -p "${PHYG_FINAL_DIR}"
rm -f "${PHYG_FINAL_BIN}"
mv "${PHYG_LOCAL_BIN}" "${PHYG_FINAL_BIN}"
printf "\n${STYLE_BLUE}${STYLE_BOLD}Finalizing binary at${STYLE_NORMAL} '%s'\n\n" "${PHYG_FINAL_DIR}/${BINARY_NAME}"


# Step 6: Clean up
#########
cd "${DIR_STARTING}"
rm -Rf "${DIR_PROJECT_TEMP}"
