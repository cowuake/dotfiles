#################################
# ==============================
# ====== USEFUL FUNCTIONS ======
# ==============================
#################################

# ================================
# ====== Generic Facilities ======
# ================================
function random-files() {
    # Set number of files to be created
    if [ $# -eq 0 ] ; then N=1; else N=$1; fi
    # Confirm the operation to be performed
    echo "Going to create $N files with random names."
    # LOOP
    for i in $(seq 1 $N)
    do
        # Choose a random name
        name="$(random | tr '\\/' '(' | tr '-' ')')"
        # Generate the file
        touch "$name"
        # Confirm file creation
        echo "Created file $i: $(pwd)/$name"
    done
}

# ============================
# ====== PDF Facilities ======
# ============================
function pdf-rm-annots() {
    OLD_FNAME=$1
    if [[ $OLD_FNAME != "" && $OLD_FNAME != " " ]] ; then
        NEW_FNAME=$(echo $OLD_FNAME | sed -e 's/\.[^.]*$/_no-annots&/')
        pdftk $OLD_FNAME output - uncompress \
            | sed '/^\/Annots/d' \
            | pdftk - output $NEW_FNAME compress \
            && echo "SUCCESS! Produced file $NEW_FNAME."
    fi
}

# ==========================================
# ====== Git facilities for lazy devs ======
# ==========================================

function gitLastCommitHash() {
    BRANCH=$1
    git log -n 1 $BRANCH --pretty=format:"%H"
}

function gitCherryPickLastCommit() {
    BRANCH=$1
    HASH=$(gitLastCommitHash $BRANCH)
    git cherry-pick $HASH
}

# ==================================================
# ====== Frequent file conversions (shorties) ======
# ==================================================
function convert-webm-to-mp4() {
    if [[ "${1: -5}" == ".webm" ]] ; then
        OLD_NAME=$1
        NEW_NAME="${OLD_NAME%.*}.mp4"
        ffmpeg -fflags +genpts -i $OLD_NAME -r 24 $NEW_NAME
    else
        echo "Not a webm file!"
    fi
}
