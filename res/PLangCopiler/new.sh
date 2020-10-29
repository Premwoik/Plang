#!/bin/bash

COMPILER_PATH=$(dirname "$0")
NEW_TEMPLATE_PATH="${COMPILER_PATH}/templates/empty"
DIR_PATH="$(pwd)/$1"

if [ -d "${DIR_PATH}" ]
then
    echo "Directory ${DIR_PATH} exists."
else
    echo "Project directory ${DIR_PATH} created succesfully!"
    cp -r "$NEW_TEMPLATE_PATH" "$DIR_PATH"
fi
